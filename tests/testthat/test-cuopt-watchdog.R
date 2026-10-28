# The cuOpt watchdog (R/cuopt_watchdog.R): the log parser, the two stop rules
# on lines taken from real cuopt_cli logs, and the settings' precedence. No GPU.

# a converging PDLP run: objectives near 3.5e5
wd_converging <- c(
  "   Iter    Primal Obj.      Dual Obj.    Gap        Primal Res.  Dual Res.   Time",
  "      0 -5.85583601e+06 -5.85583601e+06  0.00e+00   3.17e+05     2.05e+03   66.303s",
  "   1000 +5.38833829e+05 -8.09606461e+07  8.15e+07   5.77e+05     3.48e+01   67.757s",
  "  40000 +5.58112908e+05 +2.40324021e+05  3.18e+05   5.64e+01     2.69e-01   113.623s",
  "  80000 +3.62833235e+05 +3.73032355e+05  1.02e+04   1.80e+03     2.02e-02   160.546s",
  " 120000 +3.46963428e+05 +3.43787540e+05  3.18e+03   1.18e+03     8.62e-03   207.486s",
  " 360000 +3.50006861e+05 +3.49833781e+05  1.73e+02   3.52e+02     1.36e-03   489.371s")

# a diverging one (an infeasible sample): the dual objective runs away
wd_runaway <- c(
  "   Iter    Primal Obj.      Dual Obj.    Gap        Primal Res.  Dual Res.   Time",
  sprintf(" %6d +6.61801716e+08 +3.59675%03de+22  3.60e+22   6.54e+01     3.88e+08   %.3fs",
          (1:6) * 1000, 1:6, 1:6 * 0.63))

test_that("the parser reads cuOpt's iteration lines and nothing else", {
  it <- .cuopt_log_iters(c("Setting parameter method to 1", wd_converging, "Status: Optimal"))
  expect_equal(nrow(it), 6)
  expect_equal(it$iter[3], 40000)
  expect_equal(it$dobj[2], -8.09606461e+07)
  expect_equal(it$time[6], 489.371)
  expect_null(.cuopt_log_iters(c("no", "iterations")))
})

test_that("a converging run gives no verdict", {
  expect_null(.cuopt_watchdog_verdict(.cuopt_log_iters(wd_converging), cuopt_watchdog()))
})

test_that("a runaway objective stops after runaway_n lines", {
  it <- .cuopt_log_iters(wd_runaway)
  expect_match(.cuopt_watchdog_verdict(it, cuopt_watchdog()), "objective beyond 1e\\+20 for 6 log lines")
  expect_null(.cuopt_watchdog_verdict(it, cuopt_watchdog(runaway_n = 10)))
  expect_null(.cuopt_watchdog_verdict(it, cuopt_watchdog(runaway = NA)))
})

test_that("a stall stops once a window passes without a factor of improvement", {
  t <- seq(0, 4000, by = 100)
  flat <- data.frame(iter = t * 100, pobj = 1, dobj = 1, gap = 10, pres = 2.9e4, dres = 4e7, time = t)
  expect_match(.cuopt_watchdog_verdict(flat, cuopt_watchdog(stall_iter = NA)), "in the last 1800 s")
  expect_match(.cuopt_watchdog_verdict(flat, cuopt_watchdog(stall_window = NA, stall_iter = 1e5)),
               "in the last 100,000 iterations")
  # halving every 1000 s is progress by more than 2 within any 1800 s
  fall <- transform(flat, pres = 2.9e4 * 0.5^(t / 1000), dres = 4e7 * 0.5^(t / 1000))
  expect_null(.cuopt_watchdog_verdict(fall, cuopt_watchdog(stall_iter = NA)))
  # a window not yet elapsed gives no verdict
  expect_null(.cuopt_watchdog_verdict(flat[t <= 1500, ], cuopt_watchdog(stall_iter = NA)))
})

test_that("the settings come from the solve, then the session, then the defaults", {
  skip_if_not_installed("processx")
  expect_equal(.cuopt_watchdog_resolve(list())$stall_window, 1800)
  withr::with_options(list(multimod.cuopt_watchdog = cuopt_watchdog(stall_window = 600)), {
    expect_equal(.cuopt_watchdog_resolve(list())$stall_window, 600)
    expect_equal(.cuopt_watchdog_resolve(list(watchdog = list(stall_window = 60)))$stall_window, 60)
    expect_null(.cuopt_watchdog_resolve(list(watchdog = FALSE)))
  })
  withr::with_options(list(multimod.cuopt_watchdog = FALSE), expect_null(.cuopt_watchdog_resolve(list())))
})

test_that("the infeasibility switches reach the command line", {
  expect_false(any(grepl("infeasib", .cuopt_options(list()))))
  o <- .cuopt_options(list(infeasibility_detection = TRUE, strict_infeasibility = FALSE))
  expect_equal(o[match("--infeasibility-detection", o) + 1], "true")
  expect_equal(o[match("--strict-infeasibility", o) + 1], "false")
  withr::with_options(list(multimod.cuopt_infeasibility_detection = TRUE),
                      expect_true("--infeasibility-detection" %in% .cuopt_options(list())))
})

test_that("the cloud script runs cuOpt under the watchdog, or plainly when it is off", {
  s <- .cuopt_worker_script("model.mps.gz", "model.sol.gz", list(time_limit = 600))
  expect_true(any(grepl("^run_cuopt cuopt_cli /job/model.mps.gz", s)))
  expect_true(any(grepl("WATCHDOG_STOP", s)))
  expect_true(any(grepl("-v R=100000000000000000000 -v N=5 -v SW=1800 -v SI=500000 -v F=2", s, fixed = TRUE)))
  s0 <- .cuopt_worker_script("model.mps.gz", "model.sol.gz", list(time_limit = 600, watchdog = FALSE))
  expect_false(any(grepl("run_cuopt|WATCHDOG", s0)))
  expect_true(any(grepl("^cuopt_cli /job/model.mps.gz", s0)))
})

test_that("the cloud rules (awk) give the local verdicts on the same logs", {
  awk <- Sys.which("awk")
  skip_if(!nzchar(awk), "no awk on the PATH")
  prog <- withr::local_tempfile(fileext = ".awk"); writeLines(.cuopt_watchdog_awk(), prog)
  run <- function(lines, w = cuopt_watchdog()) {
    f <- withr::local_tempfile(fileext = ".log"); writeLines(lines, f)
    out <- system2(awk, c(strsplit(.cuopt_watchdog_awk_vars(w), " ")[[1]], "-f", prog, f), stdout = TRUE)
    paste(out, collapse = "")
  }
  expect_equal(run(wd_converging), "")
  expect_match(run(wd_runaway), "objective beyond 1e\\+20 for 6 log lines")
  expect_equal(run(wd_runaway, cuopt_watchdog(runaway_n = 10)), "")
})

test_that("the Python interface writes its script, parameters and warm-start output", {
  s <- .cuopt_worker_script("model.mps.gz", "model.sol.gz",
                            list(api = "python", time_limit = 3600, save_warm_start = TRUE))
  expect_true(any(grepl("^run_cuopt python3 /tmp/solve_cuopt.py /job/model.mps.gz /tmp/model.sol /tmp/params.json '' '/tmp/warm_start.pkl'", s)))
  expect_true(any(grepl("pickle.dump(sol.get_pdlp_warm_start_data()", s, fixed = TRUE)))
  p <- jsonlite::fromJSON(s[which(s == "cat > /tmp/params.json << 'PJEOF'") + 1L])
  expect_equal(p$time_limit, 3600)
  expect_equal(p$pdlp_solver_mode, 1L)        # warm starts need Stable2
  expect_equal(p$presolve, 0L)                # and the unpresolved problem's size
  expect_true(any(grepl("cp /tmp/warm_start.pkl /out/warm_start.pkl", s, fixed = TRUE)))
  # no presolve retry: the Python run always writes its solution file
  expect_false(any(grepl("--presolve 0", s, fixed = TRUE)))
  # continuing from a warm start reads it from the job's inputs
  s2 <- .cuopt_worker_script("model.mps.gz", "model.sol.gz",
                             list(api = "python", warm_start = "C:/runs/warm_start.pkl"))
  expect_true(any(grepl("'/job/warm_start.pkl' ''$", s2)))
  # without a warm start the default PDLP mode stays
  p0 <- .cuopt_py_params(list(time_limit = 60, options = "--relative-gap-tolerance 1e-5 --crossover false"), FALSE)
  expect_null(p0$pdlp_solver_mode)
  expect_equal(p0$relative_gap_tolerance, 1e-5)
  expect_false(p0$crossover)
  # a warm start through cuopt_cli is refused
  expect_error(.cuopt_worker_script("m.mps.gz", "m.sol.gz", list(save_warm_start = TRUE)), "api")
})

test_that("a WSL run goes through Python beside cuopt_cli, with '-' for no warm-start file", {
  ctl <- list(api = "python", cuopt_cli = "/opt/cuopt-env/bin/cuopt_cli")
  cm <- .cuopt_local_command("C:/runs/model.mps", "/tmp/m.sol", "-", ctl, wsl = TRUE,
                             "C:/runs/solve_cuopt.py", "C:/runs/cuopt_params.json")
  expect_equal(cm$exe, "/opt/cuopt-env/bin/python3")
  expect_equal(cm$args, c("-u", "/mnt/c/runs/solve_cuopt.py", "/mnt/c/runs/model.mps", "/tmp/m.sol",
                          "/mnt/c/runs/cuopt_params.json", "-", "-"))
  # continuing from a warm start: its path as WSL sees it
  cm2 <- .cuopt_local_command("C:/runs/model.mps", "/tmp/m.sol", "/tmp/m.pkl",
                              c(ctl, warm_start = "C:/runs/old/warm_start.pkl"), wsl = TRUE,
                              "C:/runs/solve_cuopt.py", "C:/runs/cuopt_params.json")
  expect_equal(tail(cm2$args, 2), c("/mnt/c/runs/old/warm_start.pkl", "/tmp/m.pkl"))
  # the Python can be named outright
  expect_equal(.cuopt_python_path(list(cuopt_python = "/usr/bin/python3.12"), "cuopt_cli"), "/usr/bin/python3.12")
  withr::with_envvar(c(MULTIMOD_CUOPT_PYTHON = ""),
                     expect_equal(.cuopt_python_path(list(), "cuopt_cli"), "python3"))
  # without api = "python" the command stays cuopt_cli's
  cm3 <- .cuopt_local_command("C:/runs/model.mps", "/tmp/m.sol", "-", list(cuopt_cli = "cuopt_cli"),
                              wsl = TRUE, "s.py", "p.json")
  expect_equal(cm3$exe, "cuopt_cli")
  expect_equal(cm3$args[1:3], c("/mnt/c/runs/model.mps", "--solution-file", "/tmp/m.sol"))
  # the script reads '-' as no file
  expect_true(any(grepl("'' if w == '-' else w", .cuopt_py_source(), fixed = TRUE)))
  # a warm start without the Python interface is refused before anything runs
  expect_error(.solve_mps_cuopt_local(tempfile(fileext = ".mps"), tempfile(fileext = ".sol"),
                                      list(save_warm_start = TRUE), FALSE, FALSE, wsl = TRUE), "api")
})
