# =============================================================================
# LP assembly and in-process solving with HiGHS
# =============================================================================
#
# Turns a multimod model into the numeric arrays HiGHS wants, skipping the
# symbolic layer entirely. For large models the bottleneck is not the solver
# but the problem generator: Pyomo, GMPL and JuMP all expand indices and build
# an expression object per constraint. Here the sparsity comes from the model's
# own filtering maps and the matrix is assembled directly.
#
# Note on names: `highs::highs_model()` takes no row or column names, and
# `highs_write_model()` therefore emits positional ones (c0, r0, ...). That is
# part of why it is fast. The row/column index tables returned alongside carry
# the mapping back to (symbol, index tuple), and must travel with any .mps.
# =============================================================================

#' Assemble a model into LP arrays
#'
#' @param model A multimod model with data attached.
#' @param col_index,row_index Optional index tables; built when not supplied.
#' @param on_duplicate Passed to [build_triplets()]; how to resolve a parameter
#'   that stores one index tuple with several distinct values.
#' @param verbose Logical; report progress.
#' @param index_names Logical; carry the readable `name` column in the index
#'   tables. Default `FALSE`: nothing on the solve/MPS/decode path reads the
#'   names, and on a large model they are the biggest single allocation of
#'   index building (one interned string per LP row and column).
#'
#' @return A list with
#'   `A` (a `Matrix::dgCMatrix`), `obj`, `row_lo`, `row_up`, `col_lo`, `col_up`,
#'   `maximum`, and the `row_index` / `col_index` used. Row bounds encode the
#'   constraint sense: `==` gives equal bounds, `<=` an upper bound only, `>=`
#'   a lower bound only.
#' @export
model_to_lp <- function(model, col_index = NULL, row_index = NULL,
                        on_duplicate = c("error", "last", "first"),
                        verbose = FALSE, index_names = FALSE) {
  on_duplicate <- match.arg(on_duplicate)
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  if (is.null(col_index)) col_index <- build_col_index(model, verbose = verbose,
                                                       names = index_names)
  if (is.null(row_index)) row_index <- build_row_index(model, verbose = verbose,
                                                       names = index_names)

  tr <- build_triplets(model, col_index = col_index, row_index = row_index,
                       on_duplicate = on_duplicate, verbose = verbose)
  trip <- tr$triplets
  nrow_lp <- nrow(row_index)
  ncol_lp <- nrow(col_index)

  A <- Matrix::sparseMatrix(
    i = trip$i, j = trip$j, x = trip$coef,
    dims = c(nrow_lp, ncol_lp)
  )

  # right-hand side: equations with no constant term have rhs 0
  rhs <- rep(0, nrow_lp)
  if (nrow(tr$rhs)) rhs[tr$rhs$i] <- tr$rhs$rhs

  sense <- row_index$sense
  row_lo <- ifelse(sense == "<=", -Inf, rhs)
  row_up <- ifelse(sense == ">=",  Inf, rhs)

  obj_spec <- .lp_objective(model, col_index)

  lp <- list(
    A = A,
    obj = obj_spec$obj,
    row_lo = row_lo,
    row_up = row_up,
    col_lo = col_index$lo,
    col_up = col_index$up,
    maximum = obj_spec$maximum,
    objective_symbol = obj_spec$symbol,
    row_index = row_index,
    col_index = col_index
  )
  if (verbose) {
    message(sprintf("LP: %d rows x %d cols, %d nonzeros (%s %s)",
                    nrow_lp, ncol_lp, length(A@x),
                    if (obj_spec$maximum) "maximise" else "minimise",
                    obj_spec$symbol))
  }
  lp
}

#' Objective vector from the model's declared objective
#'
#' energyRt-style models minimise a single scalar variable (`vObjective`) that
#' a constraint defines, so the objective vector is an indicator on that column.
#' @keywords internal
#' @noRd
.lp_objective <- function(model, col_index) {
  objs <- model$objectives
  if (is.null(objs) || !length(objs)) {
    stop("model_to_lp: the model declares no objective (model$objectives is empty).")
  }
  o <- objs[[1]]
  vname <- o$variable
  if (is.null(vname) || !nzchar(vname)) {
    stop("model_to_lp: the objective does not name a variable.")
  }
  jj <- col_index$j[col_index$symbol == vname]
  if (!length(jj)) {
    stop("model_to_lp: objective variable '", vname,
         "' has no column in the LP.")
  }
  if (length(jj) > 1L) {
    stop("model_to_lp: objective variable '", vname, "' resolves to ",
         length(jj), " columns; expected a scalar.")
  }
  obj <- numeric(nrow(col_index))
  obj[jj] <- 1
  sense <- if (is.null(o$sense)) "minimize" else tolower(o$sense[1])
  list(obj = obj, maximum = sense %in% c("max", "maximize", "maximise"),
       symbol = vname)
}

#' Solve a model in-process with HiGHS
#'
#' Uses the solver-object interface rather than [highs::highs_solve()], because
#' only that path returns dual values - energy-system models generally need the
#' constraint marginals, not just primal levels.
#'
#' @param model A multimod model with data attached.
#' @param lp Optional pre-built LP from [model_to_lp()].
#' @param attach Logical; attach primal values back onto `model$variables` as a
#'   long data.frame with named index columns, matching what
#'   `read_csv_results()` produces so downstream readers keep working.
#' @param control Optional list of HiGHS options.
#' @param verbose Logical; report progress.
#'
#' @return A list with `objective`, `status`, `status_message`, `primal`
#'   (a `data.table` of `j`, symbol, index tuple and value), `dual` (per row),
#'   the `lp`, and - when `attach` is `TRUE` - the updated `model`.
#' @export
solve_highs <- function(model, lp = NULL, attach = TRUE, control = list(),
                        verbose = FALSE) {
  if (!requireNamespace("highs", quietly = TRUE)) {
    stop("solve_highs() needs the 'highs' package. Install it with ",
         "install.packages(\"highs\").")
  }
  if (is.null(lp)) lp <- model_to_lp(model, verbose = verbose)

  hm <- highs::highs_model(
    L = lp$obj, lower = lp$col_lo, upper = lp$col_up,
    A = lp$A, lhs = lp$row_lo, rhs = lp$row_up,
    maximum = isTRUE(lp$maximum)
  )
  solver <- highs::hi_new_solver(hm)
  if (length(control)) {
    for (nm in names(control)) highs::hi_solver_set_option(solver, nm, control[[nm]])
  }
  highs::hi_solver_run(solver)

  status <- highs::hi_solver_status(solver)
  msg <- highs::hi_solver_status_message(solver)
  sol <- highs::hi_solver_get_solution(solver)

  ci <- lp$col_index
  ri <- lp$row_index
  # Same shape as read_mps_solution(): index columns carried through so the
  # per-variable reshape and write_energyrt_output() work off either path.
  primal <- data.table::copy(ci)
  data.table::set(primal, j = "value", value = sol$col_value)
  data.table::set(primal, j = "reduced_cost",
                  value = if (isTRUE(sol$dual_valid)) sol$col_dual else NA_real_)
  dual <- data.table::copy(ri)
  data.table::set(dual, j = "value", value = sol$row_value)
  data.table::set(dual, j = "marginal",
                  value = if (isTRUE(sol$dual_valid)) sol$row_dual else NA_real_)
  data.table::setattr(primal, "dim_names", attr(ci, "dim_names"))

  objective <- sum(lp$obj * sol$col_value)
  if (verbose) {
    message(sprintf("HiGHS: %s, objective %.10f", msg, objective))
  }

  out <- list(objective = objective, status = status, status_message = msg,
              primal = primal, dual = dual, lp = lp)
  if (attach) out$model <- .attach_primal(model, ci, sol$col_value)
  out
}

#' Attach primal values back onto the model's variables
#'
#' Produces one long data.frame per variable with its real index columns, the
#' same shape `read_csv_results()` writes, so energyRt's `read_solution()` and
#' anything else downstream keeps working unchanged.
#' @keywords internal
#' @noRd
.attach_primal <- function(model, col_index, values) {
  dim_names <- attr(col_index, "dim_names")
  ci <- data.table::copy(col_index)
  data.table::set(ci, j = "value", value = as.numeric(values))

  for (nm in unique(ci$symbol)) {
    sol <- .solution_by_variable(ci, nm, dim_names, nonzero_only = FALSE)
    if (!is.null(sol)) model$variables[[nm]]$solution <- sol
  }
  model
}

#' Write a model to MPS, with the index tables needed to read results back
#'
#' HiGHS writes MPS quickly precisely because it does not carry symbolic names:
#' the file uses positional ones (`c0`, `r0`, ...). The row and column index
#' tables written alongside carry the mapping back to `(symbol, index tuple)`,
#' so a solution vector returned by a remote solver can be interpreted.
#'
#' **The `.mps` and its index files must travel together** - the file alone
#' cannot be mapped back to model symbols.
#'
#' @param model A multimod model with data attached.
#' @param file Path of the `.mps` file to write.
#' @param index_dir Directory for the index tables; defaults to the directory
#'   of `file`.
#' @param lp Optional pre-built LP from [model_to_lp()].
#' @param format Storage format for the index tables, passed to the package's
#'   `.save_dataframe()`: `"parquet"` (default), `"ipc"` or `"csv"`.
#' @param verbose Logical; report progress.
#'
#' @return Invisibly, a list of the paths written.
#' @export
write_mps <- function(model, file, index_dir = dirname(file), lp = NULL,
                      format = c("parquet", "ipc", "csv"), verbose = FALSE) {
  if (!requireNamespace("highs", quietly = TRUE)) {
    stop("write_mps() needs the 'highs' package. Install it with ",
         "install.packages(\"highs\").")
  }
  format <- match.arg(format)
  if (is.null(lp)) lp <- model_to_lp(model, verbose = verbose)

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)

  hm <- highs::highs_model(
    L = lp$obj, lower = lp$col_lo, upper = lp$col_up,
    A = lp$A, lhs = lp$row_lo, rhs = lp$row_up,
    maximum = isTRUE(lp$maximum)
  )
  highs::highs_write_model(hm, file)

  base <- tools::file_path_sans_ext(basename(file))
  col_path <- file.path(index_dir, paste0(base, "_col_index"))
  row_path <- file.path(index_dir, paste0(base, "_row_index"))
  # The decoder maps back by POSITION and by the i* columns; the readable
  # `name` column (present only with index_names = TRUE) is dead weight in
  # the bundle and the dominant string allocation — drop it. The shallow
  # column subset avoids as.data.frame()'s deep copy of the whole table.
  .index_df <- function(x) {
    keep <- setdiff(names(x), "name")
    data.table::setDF(x[, keep, with = FALSE])
  }
  .save_dataframe(.index_df(lp$col_index), col_path, format = format)
  .save_dataframe(.index_df(lp$row_index), row_path, format = format)
  # dim_names rides along inside parquet/feather but is lost by CSV, so write it
  # explicitly too - otherwise a csv bundle cannot be mapped back to symbols
  .write_dim_names(attr(lp$col_index, "dim_names"), index_dir, base)

  if (verbose) {
    message("wrote ", file, " (", nrow(lp$row_index), " rows x ",
            nrow(lp$col_index), " cols) plus index tables in ", index_dir)
  }
  invisible(list(mps = file, col_index = col_path, row_index = row_path))
}
