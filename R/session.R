# =============================================================================
# A solver session: state that the functional API currently threads by hand
# =============================================================================
#
# The matrix route is a sequence of calls that pass the same objects along:
#
#   lp <- model_to_lp(m)
#   check_matrix_numbers(lp)
#   r  <- solve_highs(m, lp = lp)
#   write_mps(m, f, lp = lp)
#   s  <- read_mps_solution(sol, index_dir = d)
#   write_energyrt_output(s, out)
#
# Measured across the 12 exported functions on that path, ~28 argument
# positions carry the same handful of objects - `verbose` alone accounts for
# 15+ of them, with inconsistent defaults. Forgetting `lp =` is not a cheap
# mistake either: `solve_highs()` and `write_mps()` each rebuild the entire LP
# when it is absent.
#
# This class holds that state and forwards. It is deliberately a WRAPPER, not a
# replacement:
#
#   * every method calls the same exported function you would call by hand, so
#     there is one implementation and one set of semantics to verify;
#   * the functional API is untouched and remains the supported interface.
#
# Why not make the whole package reference-based: copy-on-modify is load-bearing
# here. `solve_highs(attach = TRUE)` deliberately does NOT modify the caller's
# model, `energyRt::as_multimod()` rebinds `m <- f(m)` six times, and several
# tests take a copy of the packaged fixture and mutate it. Reference semantics
# would invert all of that silently.
#
# The data.table hazard, measured: R6's `$clone(deep = TRUE)` recurses into R6
# fields only, so a data.table field stays SHARED between the original and the
# clone even with `deep = TRUE`. `$clone()` here therefore carries an explicit
# `deep_clone` method - see private$deep_clone().
# =============================================================================

# The class object is built on first use and cached here. It cannot be a
# package-level binding assigned at load time, because R6 is only a Suggests;
# and it cannot be assigned into the namespace on demand either - the namespace
# is locked once the package is loaded ("cannot change value of locked binding").
.session_cache <- new.env(parent = emptyenv())

.make_session_class <- function() {
  R6::R6Class(
    "MultimodSession",
    cloneable = TRUE,
    public = list(
      #' @field model The multimod model.
      model = NULL,
      #' @field lp The assembled LP, as returned by [model_to_lp()].
      lp = NULL,
      #' @field solution The solve or read-back result.
      solution = NULL,
      #' @field opts Shared options forwarded to each step.
      opts = NULL,

      initialize = function(model = NULL, verbose = FALSE,
                            on_duplicate = c("error", "last", "first"),
                            chunk_rows = 1000000L, control = list()) {
        self$model <- model
        self$opts <- list(
          verbose = isTRUE(verbose),
          on_duplicate = match.arg(on_duplicate),
          chunk_rows = as.integer(chunk_rows),
          control = control
        )
        invisible(self)
      },

      # Build the LP, or reuse the one already built. `force = TRUE` rebuilds.
      build_lp = function(force = FALSE, ...) {
        if (is.null(self$model)) stop("No model on the session.", call. = FALSE)
        if (!is.null(self$lp) && !force) return(invisible(self))
        self$lp <- model_to_lp(self$model,
                               on_duplicate = self$opts$on_duplicate,
                               verbose = self$opts$verbose, ...)
        invisible(self)
      },

      check_numbers = function(...) {
        self$build_lp()
        private$last_numbers <- check_matrix_numbers(self$lp, ...)
        invisible(self)
      },

      # The report from the last check_numbers(), or NULL.
      numbers = function() private$last_numbers,

      solve = function(attach = FALSE, ...) {
        self$build_lp()
        self$solution <- solve_highs(self$model, lp = self$lp, attach = attach,
                                     control = self$opts$control,
                                     verbose = self$opts$verbose, ...)
        # solve_highs(attach = TRUE) returns a MODIFIED COPY of the model. The
        # functional API makes the caller re-bind it; a session can simply
        # adopt it, which is the one ergonomic win that needs care.
        if (isTRUE(attach) && !is.null(self$solution$model)) {
          self$model <- self$solution$model
        }
        invisible(self)
      },

      write_mps = function(file, ...) {
        self$build_lp()
        private$paths <- write_mps(self$model, file, lp = self$lp,
                                   verbose = self$opts$verbose, ...)
        invisible(self)
      },

      # Paths written by the last write_mps(), or NULL.
      mps_paths = function() private$paths,

      read_solution = function(sol, ...) {
        if (is.null(self$lp)) {
          stop("Read back needs the index tables: build_lp() or write_mps() ",
               "first, or call read_mps_solution() directly with index_dir.",
               call. = FALSE)
        }
        self$solution <- read_mps_solution(sol,
                                           col_index = self$lp$col_index,
                                           row_index = self$lp$row_index, ...)
        invisible(self)
      },

      # `writer` keeps the session from naming any particular consumer's output
      # layout. The default is multimod's own; energyRt's layout is
      # write_energyrt_output(), passed in by whoever wants it.
      write_output = function(dir, writer = write_solution, ...) {
        if (is.null(self$solution)) {
          stop("No solution on the session; solve() or read_solution() first.",
               call. = FALSE)
        }
        private$out_files <- writer(self$solution, dir, ...)
        invisible(self)
      },

      # Files written by the last write_output(), or NULL.
      output_files = function() private$out_files,

      print = function(...) {
        cat("<MultimodSession>\n")
        cat("  model    :", if (is.null(self$model)) "-" else
            sprintf("%d equations, %d variables", length(self$model$equations),
                    length(self$model$variables)), "\n")
        cat("  lp       :", if (is.null(self$lp)) "-" else
            sprintf("%d rows x %d cols, %d nonzeros", nrow(self$lp$row_index),
                    nrow(self$lp$col_index), length(self$lp$A@x)), "\n")
        cat("  solution :", if (is.null(self$solution)) "-" else
            sprintf("objective %.10g", self$solution$objective), "\n")
        invisible(self)
      }
    ),
    private = list(
      last_numbers = NULL,
      paths = NULL,
      out_files = NULL,

      # R6's deep clone recurses into R6 fields only. A data.table field would
      # otherwise stay SHARED between original and clone even with
      # `deep = TRUE` - verified - so copy those explicitly. The LP carries two
      # data.tables (`col_index`, `row_index`) plus a sparse matrix.
      deep_clone = function(name, value) {
        if (inherits(value, "data.table")) return(data.table::copy(value))
        if (is.list(value)) {
          return(lapply(value, function(el) {
            if (inherits(el, "data.table")) data.table::copy(el) else el
          }))
        }
        value
      }
    )
  )
}

#' Create a solver session
#'
#' Builds a `MultimodSession`: an R6 object holding the state the matrix route
#' otherwise threads through every call - the model, the assembled LP, the
#' solution and the shared options - and forwarding each step to the ordinary
#' exported function.
#'
#' Every method returns the session invisibly, so calls chain. Results are
#' reachable as fields (`$lp`, `$solution`) and are the same objects the
#' functional API returns. The session is a convenience layer: it does not
#' replace [model_to_lp()], [solve_highs()] or [write_mps()], which remain the
#' supported interface.
#'
#' Methods: `$build_lp()`, `$check_numbers()`, `$solve()`, `$write_mps()`,
#' `$read_solution()`, `$write_output()`, and the accessors `$numbers()`,
#' `$mps_paths()`, `$output_files()`.
#'
#' @section Copy semantics:
#' This is an R6 class, so a session is a reference: `s2 <- s1` makes an alias,
#' not a copy. Use `s$clone(deep = TRUE)` for an independent session; the deep
#' clone here also copies `data.table` fields, which R6's own deep clone does
#' not.
#'
#' @param model A multimod model with data attached.
#' @param verbose Logical; forwarded to every step.
#' @param on_duplicate Passed to [model_to_lp()].
#' @param chunk_rows Passed to [model_to_lp()].
#' @param control Solver options passed to [solve_highs()].
#'
#' @return An R6 object of class `MultimodSession`.
#' @export
mm_session <- function(model = NULL, verbose = FALSE,
                       on_duplicate = c("error", "last", "first"),
                       chunk_rows = 1000000L, control = list()) {
  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("mm_session() needs the 'R6' package. install.packages(\"R6\")",
         call. = FALSE)
  }
  # Built on first use so the package does not need R6 at load time.
  if (is.null(.session_cache$cls)) .session_cache$cls <- .make_session_class()
  .session_cache$cls$new(model = model, verbose = verbose,
                         on_duplicate = match.arg(on_duplicate),
                         chunk_rows = chunk_rows, control = control)
}
