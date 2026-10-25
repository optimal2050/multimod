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
  # the header is a few `#` lines at the top; the body is read by fread
  top <- readLines(con, n = 50L, warn = FALSE)
  n_hdr <- 0L
  while (n_hdr < length(top) &&
         (startsWith(top[n_hdr + 1L], "#") || !nzchar(trimws(top[n_hdr + 1L])))) {
    n_hdr <- n_hdr + 1L
  }
  hdr <- grep("^#", top[seq_len(n_hdr)], value = TRUE)
  status <- .cuopt_header(hdr, "Status")
  obj <- suppressWarnings(as.numeric(.cuopt_header(hdr, "Objective value")))

  if (require_optimal &&
      (is.na(status) || !grepl("optimal", status, ignore.case = TRUE))) {
    stop("cuOpt solution status is '", status, "', not optimal.\n",
         "  Pass require_optimal = FALSE to read it anyway.")
  }

  if (n_hdr >= length(top)) stop("cuOpt solution file has no values: ", file)
  plain <- file
  if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    # decompressed to a temporary file in blocks, then read like the plain
    # one: lines through R would hold the whole text twice
    plain <- tempfile(fileext = ".sol")
    on.exit(unlink(plain), add = TRUE)
    .gunzip_to(file, plain)
  }
  body <- data.table::fread(plain, skip = n_hdr, header = FALSE, fill = TRUE,
                            sep = " ", colClasses = list(character = 1L),
                            blank.lines.skip = TRUE, showProgress = FALSE)
  body <- body[!startsWith(body[[1]], "#")]
  if (!nrow(body)) stop("cuOpt solution file has no values: ", file)
  nm <- body[[1]]
  vl <- if (ncol(body) >= 2) suppressWarnings(as.numeric(body[[2]])) else
    rep(NA_real_, nrow(body))
  bad <- which(is.na(vl))
  if (ncol(body) > 2) bad <- sort(unique(c(bad, which(nzchar(body[[3]])))))
  if (length(bad)) {
    stop("cuOpt solution file is malformed: entry ", bad[1], " reads '",
         trimws(paste(unlist(body[bad[1]]), collapse = " ")), "'.\n",
         "  A partial download must not be read as a partial solution.")
  }

  list(status = status, objective = obj,
       columns = data.table::data.table(name = nm, primal = vl,
                                        dual = NA_real_),
       rows = NULL)
}

#' Decompress a gzip file to another file, in blocks
#' @keywords internal
#' @noRd
.gunzip_to <- function(from, to, block = 64L * 2^20) {
  inp <- gzfile(from, "rb")
  on.exit(close(inp), add = TRUE)
  out <- file(to, "wb")
  on.exit(close(out), add = TRUE)
  repeat {
    chunk <- readBin(inp, "raw", n = block)
    if (!length(chunk)) break
    writeBin(chunk, out)
  }
  invisible(to)
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
#' @param format Storage format of the index tables: `"parquet"` (default),
#'   `"ipc"` or `"csv"`. When no table of that format is found, the one
#'   format present in `index_dir` is read.
#' @param solver Which solver wrote `sol`; `"auto"` sniffs the file.
#' @param require_optimal Passed to the reader.
#' @param substituted For a matrix written with `write_mps(substitute = )`:
#'   `"expand"` rebuilds the substituted variables into `primal`; `"defer"`
#'   leaves them out and [write_solution()] builds each from the bundle when
#'   it writes it, so a large variable is never held with the others.
#'
#' @return A list with `objective`, `status`, `primal` (`j`, symbol, name,
#'   value) and `dual` (`i`, symbol, name, value, marginal) - the same shape
#'   [solve_highs()] returns, so the two paths are interchangeable.
#' @export
#' @examples
#' if (requireNamespace("highs", quietly = TRUE)) {
#'   dir <- tempfile("mps-")
#'   dir.create(dir)
#'   mps <- file.path(dir, "toy.mps")
#'   write_mps(example_models$toy, mps, format = "csv")
#'   run <- solve_mps(mps)
#'   sol <- read_mps_solution(run$sol_file, index_dir = dir, format = "csv")
#'   names(sol)
#' }
read_mps_solution <- function(sol, index_dir = NULL, col_index = NULL,
                              row_index = NULL, base = NULL,
                              format = c("parquet", "ipc", "csv"),
                              solver = c("auto", "highs", "cuopt"),
                              require_optimal = TRUE,
                              substituted = c("expand", "defer")) {
  format <- match.arg(format)
  solver <- match.arg(solver)
  substituted <- match.arg(substituted)
  if (is.character(sol)) sol <- read_solver_solution(sol, solver, require_optimal)
  if (is.null(col_index) || is.null(row_index)) {
    if (is.null(index_dir)) {
      stop("Supply either index_dir or both col_index and row_index.")
    }
    idx <- .load_mps_index(index_dir, base, format)
    col_index <- idx$col_index
    row_index <- idx$row_index
  }

  # The tables are rewrapped with their columns shared: a copy of each
  # would be the largest allocation of the decode. The index columns are
  # carried through, as the per-variable reshape needs i1..iN to rebuild
  # dimension-named results; the value columns are added to the rewraps.
  elim <- attr(col_index, "eliminated")
  # as.list() keeps a table's own attributes, and Arrow would serialise
  # every attribute of a table it writes: the rewraps carry none
  bare <- function(x) {
    out <- data.table::setDT(as.list(x))
    for (a in setdiff(names(attributes(out)), c("names", "row.names", "class", ".internal.selfref"))) {
      data.table::setattr(out, a, NULL)
    }
    out
  }
  ci <- bare(col_index)
  ri <- bare(row_index)
  primal <- bare(ci)
  if ("name" %in% names(ci)) {
    # a matrix written with names (another writer's labels): the index
    # carries each column's label, and values are placed by name
    data.table::set(primal, j = "value",
                    value = .place_by_name(ci$name, sol$columns$name,
                                           sol$columns$primal, "column"))
    data.table::set(primal, j = "reduced_cost",
                    value = .place_by_name(ci$name, sol$columns$name,
                                           sol$columns$dual, "column"))
  } else if ("jred" %in% names(ci)) {
    # a reduced LP (lp_presolve()): the solution covers the kept
    # columns; the eliminated ones are rebuilt from their references
    jred <- ci$jred
    n_red <- sum(!is.na(jred))
    data.table::set(primal, j = "value", value = .expand_eliminated(
      .place_by_position(sol$columns$name, sol$columns$primal, n_red, "c",
                         "column"), jred, elim))
    data.table::set(primal, j = "reduced_cost", value = .expand_eliminated(
      .place_by_position(sol$columns$name, sol$columns$dual, n_red, "c",
                         "column"), jred, elim, fill_eliminated = FALSE))
    data.table::set(primal, j = "jred", value = NULL)
  } else {
    data.table::set(primal, j = "value",
                    value = .place_by_position(sol$columns$name,
                                               sol$columns$primal,
                                               nrow(ci), "c", "column"))
    data.table::set(primal, j = "reduced_cost",
                    value = .place_by_position(sol$columns$name,
                                               sol$columns$dual,
                                               nrow(ci), "c", "column"))
  }
  sb <- attr(col_index, "subst")
  if (!is.null(sb) && substituted == "expand") primal <- .mx_subst_decode(primal, sb$dir, sb$format)
  dual <- bare(ri)
  ired <- if ("ired" %in% names(ri)) ri$ired else NULL
  if (!is.null(ired)) data.table::set(dual, j = "ired", value = NULL)
  n_rows <- if (is.null(ired)) nrow(ri) else sum(!is.na(ired))
  if (is.null(sol$rows) || !nrow(sol$rows)) {
    # cuOpt writes primal values only. An absent dual section is a property of
    # the solver, not a damaged file, so the rows carry NA rather than erroring.
    data.table::set(dual, j = "value", value = NA_real_)
    data.table::set(dual, j = "marginal", value = NA_real_)
  } else if ("name" %in% names(ri)) {
    data.table::set(dual, j = "value",
                    value = .place_by_name(ri$name, sol$rows$name, sol$rows$primal, "row"))
    data.table::set(dual, j = "marginal",
                    value = .place_by_name(ri$name, sol$rows$name, sol$rows$dual, "row"))
  } else {
    rv <- .place_by_position(sol$rows$name, sol$rows$primal, n_rows, "r", "row")
    rmg <- .place_by_position(sol$rows$name, sol$rows$dual, n_rows, "r", "row")
    if (!is.null(ired)) {
      # an eliminated row is an equality with zero right-hand side: its
      # activity is 0 by construction; its dual is not available
      rv <- ifelse(is.na(ired), 0, rv[ired])
      rmg <- rmg[ired]
    }
    data.table::set(dual, j = "value", value = rv)
    data.table::set(dual, j = "marginal", value = rmg)
  }
  data.table::setattr(primal, "dim_names", attr(col_index, "dim_names"))
  data.table::setattr(primal, "codes", attr(col_index, "codes"))
  if (!is.null(sb) && substituted == "defer") data.table::setattr(primal, "subst", sb)
  data.table::setattr(dual, "codes", attr(row_index, "codes"))

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
# `c123` -> 123; NA where the name does not start with the prefix
.position_of <- function(names_vec, prefix) {
  pos <- suppressWarnings(
    as.integer(substr(names_vec, nchar(prefix) + 1L, nchar(names_vec))))
  pos[!startsWith(names_vec, prefix)] <- NA_integer_
  pos
}

.place_by_position <- function(names_vec, values, n_index, prefix, what) {
  if (length(names_vec) != n_index) {
    stop("Solution has ", length(names_vec), " ", what, "s but the index table",
         " has ", n_index, ". They must describe the same model.")
  }
  pos <- .position_of(names_vec, prefix)
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

#' Place values into index order by name
#'
#' For a matrix written with names by another writer, whose index carries each
#' label. Every index entry must appear in the solution exactly once; solution
#' entries without one (a writer's own helper column, such as Pyomo's constant
#' term) are not results and are left out.
#' @keywords internal
#' @noRd
.place_by_name <- function(index_names, sol_names, values, what) {
  if (is.null(sol_names) || !length(sol_names)) return(rep(NA_real_, length(index_names)))
  if (anyDuplicated(sol_names)) {
    stop("Solution ", what, " '", sol_names[anyDuplicated(sol_names)],
         "' appears more than once; the file does not describe one value per ",
         what, ".")
  }
  m <- match(index_names, sol_names)
  miss <- which(is.na(m))
  if (length(miss)) {
    stop(length(miss), " ", what, "(s) of the index are not in the solution, ",
         "e.g. '", index_names[miss[1]], "'.\n",
         "  The solution does not correspond to the index tables.")
  }
  values[m]
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
  pos <- .position_of(names_vec, prefix)
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
  col_path <- file.path(index_dir, paste0(base, "_col_index"))
  ci <- .load_dataframe(col_path, format)
  if (is.null(ci)) {
    # a bundle written in another format: read the one that is there
    ext <- c(ipc = ".arrow", parquet = ".parquet", csv = ".csv")
    have <- names(ext)[file.exists(paste0(col_path, ext))]
    if (length(have) == 1L) {
      format <- have
      ci <- .load_dataframe(col_path, format)
    }
  }
  ri <- .load_dataframe(file.path(index_dir, paste0(base, "_row_index")), format)
  if (is.null(ci) || is.null(ri)) {
    stop("Index tables not found for base '", base, "' in ", index_dir)
  }
  ci <- data.table::setDT(ci)
  ri <- data.table::setDT(ri)
  # the symbol, type and sense columns are stored dictionary-encoded and
  # come back as factors: as character they are what the index tables hold
  # in memory (cheap: the levels are the only strings)
  for (cl in intersect(c("symbol", "vtype"), names(ci))) {
    if (is.factor(ci[[cl]])) data.table::set(ci, j = cl, value = as.character(ci[[cl]]))
  }
  for (cl in intersect(c("symbol", "sense"), names(ri))) {
    if (is.factor(ri[[cl]])) data.table::set(ri, j = cl, value = as.character(ri[[cl]]))
  }
  # a bundle of the streaming writer holds the members as integer codes,
  # with the dictionary beside it; the codes are carried through the
  # decode and turned into labels per variable
  lv <- .read_codes(index_dir, base)
  if (!is.null(lv)) {
    data.table::setattr(ci, "codes", lv)
    data.table::setattr(ri, "codes", lv)
  }
  dn <- .read_dim_names(index_dir, base)
  if (!is.null(dn)) data.table::setattr(ci, "dim_names", dn)
  el_path <- file.path(index_dir, paste0(base, "_eliminated"))
  el <- if ("jred" %in% names(ci)) .load_dataframe(el_path, format) else NULL
  if ("jred" %in% names(ci) && is.null(el)) {
    stop("Index tables for '", base, "' describe a reduced LP but the ",
         "eliminated-column table is missing in ", index_dir)
  }
  if (!is.null(el)) {
    data.table::setattr(ci, "eliminated", data.table::as.data.table(el))
  }
  # variables substituted at build time are rebuilt from their definitions
  sd <- file.path(index_dir, paste0(base, "_subst"))
  if (file.exists(file.path(sd, "manifest.csv"))) {
    data.table::setattr(ci, "subst", list(dir = sd, format = format))
  }
  list(col_index = ci, row_index = ri)
}

# --- writing results in energyRt's layout ------------------------------------

#' Reshape one symbol's columns into a long, dim-named data frame
#'
#' Shared by [solve_highs()]'s in-memory attach and the file writer, so both
#' produce identically-shaped results.
#' @keywords internal
#' @noRd
.solution_by_variable <- function(tbl, sym, dim_names, nonzero_only = FALSE, rows = NULL) {
  # Compute the mask outside `[`: data.table evaluates the i-expression in the
  # table's scope, so a bare argument sharing a column's name (`symbol`) would
  # resolve to the column and match every row. `rows` (from .symbol_rows())
  # spares the scan of the whole table per variable.
  keep <- if (is.null(rows)) which(tbl[["symbol"]] == sym) else rows
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
  # a bundle of the streaming writer carries the members as codes of the
  # dictionary in the `codes` attribute (or as factors, from a CSV bundle)
  lv <- attr(tbl, "codes")
  for (cl in dn) {
    if (is.factor(out[[cl]])) out[[cl]] <- as.character(out[[cl]])
    else if (!is.null(lv) && is.integer(out[[cl]])) out[[cl]] <- lv[out[[cl]]]
  }
  # energyRt stores year-like indices as integers
  for (cl in intersect(c("year", "yearp", "yeare", "yearn", "year2"), dn)) {
    out[[cl]] <- suppressWarnings(as.integer(out[[cl]]))
  }
  out$value <- as.numeric(blk$value)
  out
}

#' The rows of each symbol, from the runs of the symbol column
#'
#' A symbol's rows are contiguous in an index table built per symbol, so
#' one pass over the runs gives every symbol's rows without a scan of the
#' table per symbol; a symbol in several runs gets them concatenated.
#' @return A named list of integer row vectors, in order of first appearance.
#' @keywords internal
#' @noRd
.symbol_rows <- function(symbol) {
  n <- length(symbol)
  if (!n) return(list())
  # a factor's runs are found on its codes, without materialising labels
  if (is.factor(symbol)) {
    r <- rle(as.integer(symbol))
    r$values <- levels(symbol)[r$values]
  } else {
    r <- rle(as.character(symbol))
  }
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  runs <- split(seq_along(r$values), factor(r$values, levels = unique(r$values)))
  lapply(runs, function(k) {
    if (length(k) == 1L) seq.int(starts[k], ends[k]) else
      unlist(lapply(k, function(i) seq.int(starts[i], ends[i])), use.names = FALSE)
  })
}

#' Write a solution as one table per variable
#'
#' Writes each solved variable to its own file, with the index columns under
#' their real set names rather than positional `i1..iN`.
#'
#' This is multimod's own layout and carries no assumptions about who reads it.
#' [write_energyrt_output()] builds on it, adding the extra files energyRt's
#' `read_solution()` expects.
#'
#' @param solution A solution from [solve_highs()] or [read_mps_solution()].
#' @param dir Directory to write into; created if absent.
#' @param format One of `"arrow"`, `"csv"`, `"parquet"`.
#' @param variables Optional character vector; write only these variables.
#' @param nonzero_only Drop rows whose value is zero, and skip a variable that
#'   is entirely zero. `TRUE` by default, which is what solvers' readers
#'   generally expect.
#' @return The paths written, invisibly, with the kept variable names in the
#'   `"variables"` attribute.
#' @seealso [write_energyrt_output()], [read_mps_solution()]
#' @export
#' @examples
#' \donttest{
#' # Solving takes a few seconds
#' if (requireNamespace("highs", quietly = TRUE)) {
#'   m <- example_models$energyRt$multimod
#'   r <- solve_highs(m, attach = FALSE)
#'   write_solution(r, file.path(tempdir(), "energyRt_solution"), format = "csv")
#' }
#' }
write_solution <- function(solution, dir,
                           format = c("arrow", "csv", "parquet"),
                           variables = NULL,
                           nonzero_only = TRUE) {
  format <- match.arg(format)
  if (format != "csv") .need_pkg("arrow", paste0("format = \"", format, "\""))
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

  rows_of <- .symbol_rows(primal$symbol)
  # variables substituted at build time and not rebuilt into `primal` are
  # built from the bundle, one at a time
  sb <- attr(primal, "subst")
  deferred <- if (is.null(sb)) character() else .mx_subst_vars(sb$dir)
  syms <- union(names(rows_of), deferred)
  if (!is.null(variables)) syms <- intersect(syms, variables)

  written <- character()
  kept <- character()
  members <- list()
  for (nm in syms) {
    df <- if (!is.null(rows_of[[nm]])) {
      .solution_by_variable(primal, nm, dim_names,
                            nonzero_only = nonzero_only, rows = rows_of[[nm]])
    }
    if (nm %in% deferred) {
      sub <- .mx_subst_frame(nm, sb, primal$value, dim_names[[nm]], attr(primal, "codes"),
                             nonzero_only)
      if (!is.null(sub)) {
        for (cl in dim_names[[nm]]) {
          members[[cl]] <- unique(c(members[[cl]], as.character(unique(sub[[cl]]))))
        }
      }
      df <- if (is.null(df)) sub else if (is.null(sub)) df else rbind(df, sub)
      rm(sub)
    }
    if (is.null(df)) next                      # all zero: omitted
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
  attr(written, "variables") <- kept
  attr(written, "dim_names") <- dim_names
  attr(written, "members") <- members
  invisible(written)
}

#' Write a solution in energyRt's `output/` layout
#'
#' Reproduces what energyRt's generated `output.py` writes, so
#' `energyRt::read_solution()` consumes it unchanged: the per-variable tables
#' of [write_solution()], plus a `variable_list.csv` naming what was written,
#' a set-membership table and a run log.
#'
#' @param solution The list returned by [read_mps_solution()] or [solve_highs()].
#' @param dir Output directory; created if absent.
#' @param format `"arrow"` (feather, lz4 - what energyRt writes), `"csv"` or
#'   `"parquet"`.
#' @param variables Optional character vector limiting which variables to write.
#'
#' @return Invisibly, the paths written.
#' @seealso [write_solution()]
#' @export
write_energyrt_output <- function(solution, dir,
                                  format = c("arrow", "csv", "parquet"),
                                  variables = NULL) {
  format <- match.arg(format)
  # the per-variable tables are multimod's own layout; everything below is the
  # extra scaffolding energyRt::read_solution() additionally requires
  written <- write_solution(solution, dir, format = format,
                            variables = variables, nonzero_only = TRUE)
  kept <- attr(written, "variables")
  dim_names <- attr(written, "dim_names")

  vl <- file.path(dir, "variable_list.csv")
  writeLines(c("value", kept), vl)

  meta <- c(.write_raw_data_set(solution$primal, dim_names, dir, attr(written, "members")),
            .write_solution_log(solution, dir))
  invisible(c(vl, as.character(written), meta))
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
.write_raw_data_set <- function(primal, dim_names, dir, extra = list()) {
  members <- extra
  lv <- attr(primal, "codes")
  rows_of <- .symbol_rows(primal[["symbol"]])
  for (sym in names(dim_names)) {
    dn <- dim_names[[sym]]
    if (is.null(dn) || !length(dn)) next
    rows <- rows_of[[sym]]
    if (is.null(rows) || !length(rows)) next
    for (k in seq_along(dn)) {
      col <- paste0("i", k)
      if (!col %in% names(primal)) next
      v <- unique(primal[[col]][rows])
      v <- v[!is.na(v)]
      if (!length(v)) next
      if (!is.null(lv) && is.integer(v)) v <- lv[v]
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

#' The member dictionary of a coded bundle: one label per code, in order
#' @keywords internal
#' @noRd
.write_codes <- function(levels, path) {
  utils::write.csv(data.frame(code = seq_along(levels), label = levels, stringsAsFactors = FALSE),
                   path, row.names = FALSE)
  invisible(path)
}

.read_codes <- function(dir, base) {
  f <- file.path(dir, paste0(base, "_codes.csv"))
  if (!file.exists(f)) return(NULL)
  d <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = c("integer", "character"))
  if (!nrow(d) || !identical(d$code, seq_len(nrow(d)))) stop("The member dictionary ", f, " is not in code order.")
  d$label
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
