# solve_mps(): solvers and places. The commands are checked as data; only the
# in-process HiGHS solve runs here.

test_that("an unsupported solver/place pair is refused with the list", {
  f <- tempfile(fileext = ".mps"); writeLines("NAME x", f); on.exit(unlink(f))
  expect_error(solve_mps(f, solver = "highs", where = "remote"),
               "does not run where = 'remote'.*cuopt/remote")
  expect_error(solve_mps(f, solver = "julia_highs", where = "wsl"),
               "does not run where = 'wsl'")
})

test_that("control must be a named list", {
  f <- tempfile(fileext = ".mps"); writeLines("NAME x", f); on.exit(unlink(f))
  expect_error(solve_mps(f, control = list(1)), "named list")
})

test_that("cuopt_cli arguments carry method, time limit, presolve and flags", {
  a <- multimod:::.cuopt_cli_args("/job/m.mps.gz", "/tmp/model.sol",
                                  list(method = 1L, time_limit = 30000,
                                       presolve = FALSE,
                                       options = "--crossover 1"))
  expect_identical(a, c("/job/m.mps.gz", "--solution-file", "/tmp/model.sol",
                        "--method", "1", "--time-limit", "30000",
                        "--presolve", "0", "--crossover", "1"))
  # defaults: PDLP, no time limit set here
  expect_identical(multimod:::.cuopt_options(list()), c("--method", "1"))
})

test_that("Windows paths map to WSL mounts", {
  expect_identical(multimod:::.wsl_path("/already/linux"), "/already/linux")
  p <- multimod:::.wsl_path("C:/Users/x/model.mps")
  expect_identical(p, "/mnt/c/Users/x/model.mps")
})

test_that("the WSL user comes from control, then the environment", {
  withr::local_envvar(MULTIMOD_WSL_USER = "")
  expect_identical(multimod:::.wsl_user_args(list()), character(0))
  expect_identical(multimod:::.wsl_user_args(list(wsl_user = "os")), c("-u", "os"))
  withr::local_envvar(MULTIMOD_WSL_USER = "me")
  expect_identical(multimod:::.wsl_user_args(list()), c("-u", "me"))
})

test_that("the remote worker writes to /tmp and compresses into /out", {
  s <- multimod:::.cuopt_worker_script("model.mps.gz", "model.sol.gz",
                                       list(time_limit = 600))
  solve <- grep("^cuopt_cli ", s, value = TRUE)
  expect_length(solve, 1L)
  expect_match(solve, "^cuopt_cli /job/model.mps.gz --solution-file /tmp/model.sol --method 1 --time-limit 600$")
  expect_true("gzip -1 -c /tmp/model.sol > /out/model.sol.gz" %in% s)
  # nothing but the compressed solution is written to /out
  expect_false(any(grepl("/out/model.sol( |$)", s)))
  # the install is retried, and comes before the solve
  pip <- grep("pip install", s)
  expect_length(pip, 1L)
  expect_match(s[pip], "for i in 1 2 3 4 5")
  expect_lt(pip, grep("^cuopt_cli ", s))
  expect_identical(s[1], "set -e")
  # a presolve that solves the whole problem writes no file: solved again
  # without it, and only when the caller did not set presolve
  expect_true(paste("[ -s /tmp/model.sol ] ||", solve, "--presolve 0") %in% s)
  s0 <- multimod:::.cuopt_worker_script("m.mps.gz", "m.sol.gz", list(presolve = TRUE))
  expect_false(any(grepl("\\[ -s /tmp/model.sol \\]", s0)))
})

test_that("the cuOpt header gives status and objective, plain or gzipped", {
  f <- tempfile(fileext = ".sol"); on.exit(unlink(c(f, paste0(f, ".gz"))))
  writeLines(c("# Status: Optimal", "# Objective value: 36880.25", "c0 1", "c1 0"), f)
  h <- multimod:::.cuopt_sol_header(f)
  expect_identical(h$status, "Optimal")
  expect_equal(h$objective, 36880.25)
  con <- gzfile(paste0(f, ".gz"), "w"); writeLines(readLines(f), con); close(con)
  expect_identical(multimod:::.cuopt_sol_header(paste0(f, ".gz"))$status, "Optimal")
  expect_error(multimod:::.check_cuopt_status(list(status = "TimeLimit"), TRUE, "remote"),
               "not optimal")
})

test_that("a remote solve wants a compressed solution file", {
  skip_if_not_installed("mmcloud")
  f <- tempfile(fileext = ".mps"); writeLines("NAME x", f); on.exit(unlink(f))
  expect_error(solve_mps(f, sol_file = tempfile(fileext = ".sol"),
                         solver = "cuopt", where = "remote"), "must end in .gz")
})

test_that("the Julia script reads, sets options, solves and writes", {
  j <- multimod:::.julia_highs_script(list(solver = "hipo", time_limit = 60,
                                           run_crossover = FALSE, threads = 4L))
  expect_true(any(grepl('Highs_setStringOptionValue(h, "solver", "hipo")', j, fixed = TRUE)))
  expect_true(any(grepl('Highs_setDoubleOptionValue(h, "time_limit", 60)', j, fixed = TRUE)))
  expect_true(any(grepl('Highs_setBoolOptionValue(h, "run_crossover", 0)', j, fixed = TRUE)))
  expect_true(any(grepl('Highs_setIntOptionValue(h, "threads", 4)', j, fixed = TRUE)))
  expect_true(any(grepl("^println\\(\"MMJULIA ", j)))
  # the status is HiGHS' numeric code: functions beyond the C API's core differ
  # between HiGHS.jl versions
  expect_true("st = HiGHS.Highs_getModelStatus(h)" %in% j)
  expect_identical(multimod:::.highs_model_status("7"), "Optimal")
  expect_identical(multimod:::.highs_model_status(13), "Time limit reached")
  expect_identical(multimod:::.highs_model_status("99"), "status 99")
})

test_that("Julia solves the toy model to the HiGHS objective", {
  skip_on_cran()
  probe <- withr::local_tempfile(fileext = ".jl", lines = "using HiGHS")
  ok <- tryCatch(suppressWarnings(system2(get_multimod_julia(), shQuote(probe),
                                          stdout = FALSE, stderr = FALSE)) == 0,
                 error = function(e) FALSE)
  if (!isTRUE(ok)) skip("Julia with HiGHS.jl not available")
  d <- withr::local_tempdir()
  f <- file.path(d, "model.mps")
  write_mps(example_models$toy, f, index_dir = d)
  h <- solve_mps(f, sol_file = file.path(d, "h.sol"))
  j <- solve_mps(f, sol_file = file.path(d, "j.sol"), solver = "julia_highs")
  expect_identical(j$status_message, "Optimal")
  expect_equal(j$objective, h$objective, tolerance = 1e-8)
})
