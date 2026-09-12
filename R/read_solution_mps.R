# =============================================================================
# Reading a solved MPS back into a model
# =============================================================================
#
# The return leg of the cloud workflow. `write_mps()` emits positional column
# and row names (`c0`, `r0`, ...) - that is part of why HiGHS writes MPS quickly
# - so a solution file cannot be interpreted on its own. The `col_index` /
# `row_index` tables written alongside carry the mapping back to
# (symbol, index tuple), and these functions apply it.
#
# The output is deliberately written in the layout energyRt's `read_solution()`
# already consumes (one feather file per variable, dim-named columns plus
# `value`, nonzeros only), so nothing downstream has to change.
# =============================================================================

#' Parse a HiGHS solution file
#'
#' Reads the raw (machine) format written by `Highs.writeSolution(path, 0)` /
#' `highs_write_solution(..., style = 0)`. The pretty style (`1`) is a human
#' table and is not parsed here.
#'
#' @param file Path to the solution file; `.gz` is read transparently, matching
#'   how large models are transferred.
#' @param require_optimal Logical; error unless the model status is optimal.
#'   Set `FALSE` to inspect an infeasible or unbounded result.
#'
#' @return A list with `status`, `objective`, and `columns` / `rows`
#'   (`data.table`s of `name`, `primal`, `dual`). `dual` is `NA` when the file
#'   carries no dual section.
#' @export
read_highs_solution <- function(file, require_optimal = TRUE) {
  if (!file.exists(file)) stop("Solution file not found: ", file)
  con <- if (grepl("\\.gz$", file, ignore.case = TRUE)) gzfile(file, "rt") else file(file, "rt")
  on.exit(close(con), add = TRUE)
  ln <- readLines(con, warn = FALSE)
  ln <- trimws(ln)

  status <- .hi_sol_status(ln)
  if (require_optimal && !grepl("optimal", status, ignore.case = TRUE)) {
    stop("HiGHS solution status is '", status, "', not optimal.\n",
         "  Pass require_optimal = FALSE to read it anyway.")
  }

  primal <- .hi_sol_section(ln, "# Primal solution values")
  dual <- .hi_sol_section(ln, "# Dual solution values")

  obj <- NA_real_
  oi <- grep("^Objective\\b", ln)
  if (length(oi)) obj <- suppressWarnings(as.numeric(sub("^Objective\\s+", "", ln[oi[1]])))

  cols <- .hi_sol_join(primal$columns, dual$columns, "column")
  rows <- .hi_sol_join(primal$rows, dual$rows, "row")

  list(status = status, objective = obj, columns = cols, rows = rows)
}

#' Model status line
#' @keywords internal
#' @noRd
.hi_sol_status <- function(ln) {
  i <- match("Model status", ln)
  if (is.na(i)) return(NA_character_)
  nxt <- ln[(i + 1):length(ln)]
  nxt <- nxt[nzchar(nxt)]
  if (!length(nxt)) NA_character_ else nxt[1]
}

#' Extract the Columns / Rows blocks of one `# ...` section
#' @keywords internal
#' @noRd
.hi_sol_section <- function(ln, header) {
  start <- match(header, ln)
  if (is.na(start)) return(list(columns = NULL, rows = NULL))
  # the section ends at the next top-level "# " header
  later <- grep("^# ", ln)
  later <- later[later > start]
  end <- if (length(later)) {
    nxt <- later[!grepl("^# (Columns|Rows)\\b", ln[later])]
    if (length(nxt)) nxt[1] - 1L else length(ln)
  } else {
    length(ln)
  }
  blk <- ln[start:end]
  list(columns = .hi_sol_block(blk, "# Columns"),
       rows    = .hi_sol_block(blk, "# Rows"))
}

#' One `# Columns <n>` / `# Rows <n>` block of `name value` pairs
#' @keywords internal
#' @noRd
.hi_sol_block <- function(blk, marker) {
  i <- grep(paste0("^", marker, "\\b"), blk)
  if (!length(i)) return(NULL)
  i <- i[1]
  n <- suppressWarnings(as.integer(sub(paste0("^", marker, "\\s+"), "", blk[i])))
  if (is.na(n) || n < 0L) {
    stop("Malformed '", marker, "' header in the solution file: ", blk[i])
  }
  if (n == 0L) {
    return(data.table::data.table(name = character(), value = numeric()))
  }
  body <- blk[(i + 1):min(i + n, length(blk))]
  if (length(body) < n) {
    stop("Solution file is truncated: '", marker, "' declares ", n,
         " entries but only ", length(body), " are present.\n",
         "  A partial download must not be read as a partial solution.")
  }
  parts <- strsplit(body, "[[:space:]]+")
  nm <- vapply(parts, function(p) p[1], character(1))
  vl <- suppressWarnings(as.numeric(vapply(parts, function(p) p[2], character(1))))

  # Counting lines is not enough: if entries are missing, the next section
  # header slides into the block and parses as a bogus `name value` pair.
  bad <- which(startsWith(nm, "#") | is.na(vl))
  if (length(bad)) {
    stop("Solution file is malformed or truncated: '", marker, "' declares ", n,
         " entries but entry ", bad[1], " reads '", body[bad[1]], "'.
",
         "  A partial download must not be read as a partial solution.")
  }

  data.table::data.table(name = nm, value = vl)
}

#' Combine the primal and dual blocks for columns or rows
#' @keywords internal
#' @noRd
.hi_sol_join <- function(pr, du, what) {
  if (is.null(pr)) {
    stop("Solution file has no primal ", what, " values.")
  }
  out <- data.table::data.table(name = pr$name, primal = pr$value)
  if (is.null(du)) {
    data.table::set(out, j = "dual", value = NA_real_)
  } else {
    if (nrow(du) != nrow(pr)) {
      stop("Solution file has ", nrow(pr), " primal and ", nrow(du),
           " dual ", what, " entries; they must agree.")
    }
    data.table::set(out, j = "dual", value = du$value)
  }
  out
}

#' Parse a cuOpt solution file
#'
#' cuOpt writes a different shape from HiGHS: two comment lines carrying the
#' status and the objective, then one `name value` pair per column.
#'
#' ```
#' # Status: Optimal
#' # Objective value: 36880.2354212843493
#' c0 0
#' c1 1.5
#' ```
#'
#' There is **no row section**: cuOpt's `--solution-file` writes primal values
#' only, so nothing on this path yields duals or marginals.
#'
#' @param file Path to the solution file; `.gz` is read transparently.
#' @param require_optimal Logical; error unless the status is optimal. A capped
#'   or interrupted solve returns a feasible but sub-optimal primal, and
#'   presenting that as the optimum is the failure this guards.
#'
#' @return The same shape as [read_highs_solution()]: `status`, `objective`,
#'   `columns` (`name`, `primal`, `dual` = `NA`), and `rows` = `NULL` to mark
#'   the absence of dual information.
#' @export
read_cuopt_solution <- function(file, require_optimal = TRUE) {
  if (!file.exists(file)) stop("Solution file not found: ", file)
  con <- if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    gzfile(file, "rt")
  } else {
    file(file, "rt")
  }
  on.exit(close(con), add = TRUE)
  ln <- readLines(con, warn = FALSE)

  hdr <- grep("^#", ln, value = TRUE)
  status <- .cuopt_header(hdr, "Status")
  obj <- suppressWarnings(as.numeric(.cuopt_header(hdr, "Objective value")))

  if (require_optimal &&
      (is.na(status) || !grepl("optimal", status, ignore.case = TRUE))) {
    stop("cuOpt solution status is '", status, "', not optimal.\n",
         "  Pass require_optimal = FALSE to read it anyway.")
  }

  body <- ln[!startsWith(ln, "#") & nzchar(trimws(ln))]
  if (!length(body)) stop("cuOpt solution file has no values: ", file)

  parts <- strsplit(trimws(body), "[[:space:]]+")
  nm <- vapply(parts, function(p) p[1], character(1))
  vl <- suppressWarnings(as.numeric(vapply(
    parts, function(p) if (length(p) > 1) p[2] else NA_character_,
    character(1))))
  bad <- which(is.na(vl))
  if (length(bad)) {
    stop("cuOpt solution file is malformed: entry ", bad[1], " reads '",
         body[bad[1]], "'.\n",
         "  A partial download must not be read as a partial solution.")
  }

  list(status = status, objective = obj,
       columns = data.table::data.table(name = nm, primal = vl,
                                        dual = NA_real_),
       rows = NULL)
}

#' One `# <key>: <value>` header line
#' @keywords internal
#' @noRd
.cuopt_header <- function(hdr, key) {
  i <- grep(paste0("^#\\s*", key, "\\s*:"), hdr)
  if (!length(i)) return(NA_character_)
  trimws(sub(paste0("^#\\s*", key, "\\s*:"), "", hdr[i[1]]))
}

#' Read a solution file, whichever solver wrote it
#'
#' @param file Path to the solution file.
#' @param solver `"auto"` sniffs the first comment line, or name the format.
#' @param require_optimal Passed through.
#' @return As [read_highs_solution()].
#' @export
read_solver_solution <- function(file, solver = c("auto", "highs", "cuopt"),
                                 require_optimal = TRUE) {
  solver <- match.arg(solver)
  if (identical(solver, "auto")) solver <- .sniff_solution(file)
  switch(solver,
         cuopt = read_cuopt_solution(file, require_optimal),
         highs = read_highs_solution(file, require_optimal))
}

#' Which solver wrote a solution file
#'
#' cuOpt opens with `# Status:`; HiGHS's raw format has a bare `Model status`
#' line. Sniffing beats a caller-supplied guess, which is one more thing to get
#' wrong at the point where a mismatch produces plausible numbers.
#' @keywords internal
#' @noRd
.sniff_solution <- function(file) {
  con <- if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    gzfile(file, "rt")
  } else {
    file(file, "rt")
  }
  on.exit(close(con), add = TRUE)
  head_ln <- trimws(readLines(con, n = 20, warn = FALSE))
  if (any(grepl("^#\\s*Status\\s*:", head_ln))) return("cuopt")
  if (any(head_ln == "Model status")) return("highs")
  stop("Cannot tell which solver wrote '", file, "': expected a cuOpt ",
       "'# Status:' line or a HiGHS 'Model status' line in the first 20 ",
       "lines. Pass solver = ... explicitly.")
}

# --- mapping positions back to symbols ---------------------------------------

#' Map a solved MPS back onto model symbols
#'
#' @param sol Path to a HiGHS solution file, or the list returned by
#'   [read_highs_solution()].
#' @param index_dir Directory holding the `*_col_index` / `*_row_index` tables
#'   written by [write_mps()]. Ignored when both index tables are supplied.
#' @param col_index,row_index Index tables, if already in memory.
#' @param base Basename used when the index files were written; inferred from
#'   the directory when there is exactly one candidate.
#' @param format Storage format of the index tables.
#' @param solver Which solver wrote `sol`; `"auto"` sniffs the file.
#' @param require_optimal Passed to the reader.
#'
#' @return A list with `objective`, `status`, `primal` (`j`, symbol, name,
#'   value) and `dual` (`i`, symbol, name, value, marginal) - the same shape
#'   [solve_highs()] returns, so the two paths are interchangeable.
#' @export
read_mps_solution <- function(sol, index_dir = NULL, col_index = NULL,
                              row_index = NULL, base = NULL,
                              format = c("parquet", "ipc", "csv"),
                              solver = c("auto", "highs", "cuopt"),
                              require_optimal = TRUE) {
  format <- match.arg(format)
  solver <- match.arg(solver)
  if (is.character(sol)) sol <- read_solver_solution(sol, solver, require_optimal)
  if (is.null(col_index) || is.null(row_index)) {
    if (is.null(index_dir)) {
      stop("Supply either index_dir or both col_index and row_index.")
    }
    idx <- .load_mps_index(index_dir, base, format)
    col_index <- idx$col_index
    row_index <- idx$row_index
  }

  ci <- data.table::as.data.table(col_index)
  ri <- data.table::as.data.table(row_index)

  # Carry the index columns through: the per-variable reshape needs i1..iN to
  # rebuild dimension-named results.
  primal <- data.table::copy(ci)
  data.table::set(primal, j = "value",
                  value = .place_by_position(sol$columns$name,
                                             sol$columns$primal,
                                             nrow(ci), "c", "column"))
  data.table::set(primal, j = "reduced_cost",
                  value = .place_by_position(sol$columns$name,
                                             sol$columns$dual,
                                             nrow(ci), "c", "column"))
  dual <- data.table::copy(ri)
  if (is.null(sol$rows) || !nrow(sol$rows)) {
    # cuOpt writes primal values only. An absent dual section is a property of
    # the solver, not a damaged file, so the rows carry NA rather than erroring.
    data.table::set(dual, j = "value", value = NA_real_)
    data.table::set(dual, j = "marginal", value = NA_real_)
  } else {
    data.table::set(dual, j = "value",
                    value = .place_by_position(sol$rows$name, sol$rows$primal,
                                               nrow(ri), "r", "row"))
    data.table::set(dual, j = "marginal",
                    value = .place_by_position(sol$rows$name, sol$rows$dual,
                                               nrow(ri), "r", "row"))
  }
  data.table::setattr(primal, "dim_names", attr(col_index, "dim_names"))

  list(objective = sol$objective, status = sol$status,
       primal = primal, dual = dual,
       col_index = col_index, row_index = row_index)
}

#' Place values into index order by their parsed position
#'
#' Zipping the solution onto the index by line order assumes the solver returns
#' every entry, once, in the order it read them. Parsing `c<i>` and assigning
#' into that slot holds even if a solver reorders its output, and turns the
#' cases that would silently misalign every value -- a duplicate, a gap, an
#' out-of-range index -- into an error naming the offending entry.
#' @keywords internal
#' @noRd
.place_by_position <- function(names_vec, values, n_index, prefix, what) {
  if (length(names_vec) != n_index) {
    stop("Solution has ", length(names_vec), " ", what, "s but the index table",
         " has ", n_index, ". They must describe the same model.")
  }
  pos <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", names_vec)))
  bad <- which(is.na(pos) | pos < 0L | pos >= n_index)
  if (length(bad)) {
    stop("Solution ", what, " name '", names_vec[bad[1]], "' is not a ",
         prefix, "<position> in 0..", n_index - 1L, ".\n",
         "  The solution does not correspond to the index tables.")
  }
  if (anyDuplicated(pos)) {
    d <- pos[anyDuplicated(pos)]
    stop("Solution ", what, " position ", d, " appears more than once; the ",
         "file does not describe one value per ", what, ".")
  }
  out <- rep(NA_real_, n_index)
  out[pos + 1L] <- values
  out
}

#' Verify the positional names line up with the index, rather than assuming it
#'
#' A reordering between writer and solver, or a solution for a different model,
#' would otherwise produce plausible but wrong results.
#' @keywords internal
#' @noRd
.check_positions <- function(names_vec, n_index, prefix, what) {
  if (length(names_vec) != n_index) {
    stop("Solution has ", length(names_vec), " ", what, "s but the index table",
         " has ", n_index, ". They must describe the same model.")
  }
  pos <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", names_vec)))
  if (anyNA(pos) || !identical(pos, seq_len(n_index) - 1L)) {
    bad <- which(is.na(pos) | pos != (seq_len(n_index) - 1L))[1]
    stop("Solution ", what, " names are not the expected positional sequence",
         " (first mismatch at position ", bad, ": '", names_vec[bad], "').\n",
         "  The solution does not correspond to the index tables.")
  }
  invisible(TRUE)
}

#' Locate and load the index tables written next to an .mps
#' @keywords internal
#' @noRd
.load_mps_index <- function(index_dir, base, format) {
  if (is.null(base)) {
    cand <- list.files(index_dir, pattern = "_col_index")
    cand <- unique(sub("_col_index.*$", "", cand))
    if (length(cand) != 1L) {
      stop("Cannot infer the index basename in '", index_dir, "': found ",
           length(cand), " candidates. Pass base = ...")
    }
    base <- cand
  }
  ci <- .load_dataframe(file.path(index_dir, paste0(base, "_col_index")), format)
  ri <- .load_dataframe(file.path(index_dir, paste0(base, "_row_index")), format)
  if (is.null(ci) || is.null(ri)) {
    stop("Index tables not found for base '", base, "' in ", index_dir)
  }
  dn <- .read_dim_names(index_dir, base)
  if (!is.null(dn)) data.table::setattr(ci, "dim_names", dn)
  list(col_index = ci, row_index = ri)
}

# --- writing results in energyRt's layout ------------------------------------

#' Reshape one symbol's columns into a long, dim-named data frame
#'
#' Shared by [solve_highs()]'s in-memory attach and the file writer, so both
#' produce identically-shaped results.
#' @keywords internal
#' @noRd
.solution_by_variable <- function(tbl, sym, dim_names, nonzero_only = FALSE) {
  # Compute the mask outside `[`: data.table evaluates the i-expression in the
  # table's scope, so a bare argument sharing a column's name (`symbol`) would
  # resolve to the column and match every row.
  keep <- which(tbl[["symbol"]] == sym)
  blk <- tbl[keep]
  if (nonzero_only) blk <- blk[which(blk[["value"]] != 0)]
  if (!nrow(blk)) return(NULL)
  dn <- dim_names[[sym]]
  if (is.null(dn) || !length(dn)) {
    return(data.frame(value = blk$value))
  }
  idx <- paste0("i", seq_along(dn))
  out <- as.data.frame(blk[, idx, with = FALSE])
  names(out) <- dn
  # energyRt stores year-like indices as integers
  for (cl in intersect(c("year", "yearp", "yeare", "yearn", "year2"), dn)) {
    out[[cl]] <- suppressWarnings(as.integer(out[[cl]]))
  }
  out$value <- as.numeric(blk$value)
  out
}

#' Write a solution in energyRt's `output/` layout
#'
#' Reproduces what energyRt's generated `output.py` writes, so
#' `energyRt::read_solution()` consumes it unchanged: one file per variable with
#' the variable's own dimension names plus `value`, **nonzero rows only**, and a
#' `variable_list.csv` naming the variables written.
#'
#' @param solution The list returned by [read_mps_solution()] or [solve_highs()].
#' @param dir Output directory; created if absent.
#' @param format `"arrow"` (feather, lz4 - what energyRt writes), `"csv"` or
#'   `"parquet"`.
#' @param variables Optional character vector limiting which variables to write.
#'
#' @return Invisibly, the paths written.
#' @export
write_energyrt_output <- function(solution, dir,
                                  format = c("arrow", "csv", "parquet"),
                                  variables = NULL) {
  format <- match.arg(format)
  primal <- solution$primal
  if (is.null(primal)) stop("solution has no $primal table.")
  dim_names <- attr(primal, "dim_names")
  if (is.null(dim_names)) dim_names <- attr(solution$col_index, "dim_names")
  if (is.null(dim_names)) {
    stop("No dim_names available: the index table lost them (CSV does not ",
         "preserve attributes). Re-write the index with format = 'parquet', ",
         "or keep the companion *_dim_names.csv written by write_mps().")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  syms <- unique(primal$symbol)
  if (!is.null(variables)) syms <- intersect(syms, variables)

  written <- character()
  kept <- character()
  for (nm in syms) {
    df <- .solution_by_variable(primal, nm, dim_names, nonzero_only = TRUE)
    if (is.null(df)) next                      # all zero: energyRt omits it
    p <- switch(
      format,
      arrow = {
        f <- file.path(dir, paste0(nm, ".arrow"))
        arrow::write_feather(df, f, compression = "lz4")
        f
      },
      parquet = {
        f <- file.path(dir, paste0(nm, ".parquet"))
        arrow::write_parquet(df, f)
        f
      },
      csv = {
        f <- file.path(dir, paste0(nm, ".csv"))
        utils::write.csv(df, f, row.names = FALSE)
        f
      }
    )
    written <- c(written, p)
    kept <- c(kept, nm)
  }

  vl <- file.path(dir, "variable_list.csv")
  writeLines(c("value", kept), vl)

  meta <- c(.write_raw_data_set(primal, dim_names, dir),
            .write_solution_log(solution, dir))
  invisible(c(vl, written, meta))
}

#' Set membership, in the long form energyRt's reader expects
#'
#' `read_solution()` maps every dimension column through this table; a column
#' whose set is missing reads back as all `NA` without complaint, so an
#' incomplete file is worse than an absent one. Members are collected from the
#' index itself, which by construction covers every value the solution can
#' reference.
#' @keywords internal
#' @noRd
.write_raw_data_set <- function(primal, dim_names, dir) {
  members <- list()
  for (sym in names(dim_names)) {
    dn <- dim_names[[sym]]
    if (is.null(dn) || !length(dn)) next
    rows <- which(primal[["symbol"]] == sym)
    if (!length(rows)) next
    for (k in seq_along(dn)) {
      col <- paste0("i", k)
      if (!col %in% names(primal)) next
      v <- unique(primal[[col]][rows])
      v <- v[!is.na(v)]
      if (!length(v)) next
      members[[dn[k]]] <- unique(c(members[[dn[k]]], as.character(v)))
    }
  }
  if (!length(members)) return(character())
  out <- do.call(rbind, lapply(names(members), function(s) {
    data.frame(set = s, value = members[[s]], stringsAsFactors = FALSE)
  }))
  f <- file.path(dir, "raw_data_set.csv")
  utils::write.csv(out, f, row.names = FALSE, quote = FALSE)
  f
}

#' The run log energyRt reads to decide whether a scenario is solved
#'
#' `"solution status",1` is what marks a run as solved. A capped or interrupted
#' solve returns a feasible but sub-optimal primal, and reporting that as
#' success would hand back a wrong answer presented as the optimum -- plausible,
#' silent, and the worst failure this path can produce. The status therefore
#' comes from the solution, never from the fact that a file was written.
#' @keywords internal
#' @noRd
.write_solution_log <- function(solution, dir) {
  st <- solution$status
  ok <- !is.null(st) && !is.na(st) && grepl("optimal", st, ignore.case = TRUE)
  now <- format(Sys.time(), "%H:%M:%S")
  f <- file.path(dir, "log.csv")
  writeLines(c(
    "parameter,value,time",
    paste0('"model language",multimod,"', now, '"'),
    paste0('"solver status",', shQuote(st %||% "unknown"), ',"', now, '"'),
    paste0('"solution status",', as.integer(ok), ',"', now, '"'),
    paste0('"objective",', format(solution$objective %||% NA_real_,
                                  digits = 17), ',"', now, '"'),
    paste0('"done",,"', now, '"')
  ), f)
  f
}

# --- dim-name sidecar --------------------------------------------------------

#' Persist per-symbol dimension names next to the index tables
#'
#' `dim_names` rides along as an attribute in parquet/feather but is lost by
#' CSV, so it is also written as a small long table. That keeps the exported
#' bundle self-describing whichever format was chosen.
#' @keywords internal
#' @noRd
.write_dim_names <- function(dim_names, dir, base) {
  if (is.null(dim_names) || !length(dim_names)) return(invisible(NULL))
  rows <- lapply(names(dim_names), function(s) {
    d <- dim_names[[s]]
    if (is.null(d) || !length(d)) return(NULL)
    data.frame(symbol = s, position = seq_along(d), dim = d,
               stringsAsFactors = FALSE)
  })
  rows <- do.call(rbind, rows[!vapply(rows, is.null, TRUE)])
  if (is.null(rows)) return(invisible(NULL))
  f <- file.path(dir, paste0(base, "_dim_names.csv"))
  utils::write.csv(rows, f, row.names = FALSE)
  invisible(f)
}

#' Read the dim-name sidecar back into a named list
#' @keywords internal
#' @noRd
.read_dim_names <- function(dir, base) {
  f <- file.path(dir, paste0(base, "_dim_names.csv"))
  if (!file.exists(f)) return(NULL)
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  if (!nrow(d)) return(NULL)
  d <- d[order(d$symbol, d$position), ]
  split(d$dim, d$symbol)
}
