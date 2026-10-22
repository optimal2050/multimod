# The matrix goldens: one LP as five plain tables, and their serialisation.
#
# Sourced by specs/goldens/generate.R (writer) and by
# tests/testthat/test-matrix-goldens.R (reader and comparison). Language
# neutral: CSV with doubles printed at 17 significant digits, infinities as
# "Inf" / "-Inf", row and column labels as `symbol[i1,i2,...]`.
#
# Tables of one case (directory `specs/goldens/matrix/<case>/`):
#   rows.csv      i, name, sense, lo, up           one row per LP row
#   cols.csv      j, name, lo, up, obj             one row per LP column
#   triplets.csv  i, j, coef                       nonzeros, sorted by (i, j)
#   meta.json     dims, nnz, objective symbol and sense, known optimum
#   presolve/     the same after lp_presolve(): eliminated.csv (col, ref,
#                 coef, pass), triplets.csv, rows.csv (i = reduced position,
#                 full = position in the full LP), cols.csv (j, full, ...)

golden_label <- function(index, id_col) {
  idx <- grep("^i[0-9]+$", names(index), value = TRUE)
  if (length(idx) == 0L) return(index$symbol)
  parts <- as.matrix(index[, idx, with = FALSE])
  vapply(seq_len(nrow(parts)), function(r) {
    p <- parts[r, ]
    p <- p[!is.na(p)]
    if (length(p) == 0L) index$symbol[r] else
      paste0(index$symbol[r], "[", paste(p, collapse = ","), "]")
  }, "")
}

golden_matrix_tables <- function(lp) {
  A <- lp$A
  tr <- Matrix::summary(A)
  trip <- data.frame(i = as.integer(tr$i), j = as.integer(tr$j), coef = as.numeric(tr$x))
  trip <- trip[trip$coef != 0, , drop = FALSE]
  trip <- trip[order(trip$i, trip$j), , drop = FALSE]
  rownames(trip) <- NULL

  if (!is.null(lp$eliminated)) {
    keep_r <- !is.na(lp$row_index$ired)
    keep_c <- !is.na(lp$col_index$jred)
    rows <- data.frame(i = lp$row_index$ired[keep_r],
                       full = lp$row_index$i[keep_r],
                       name = golden_label(lp$row_index, "i")[keep_r],
                       sense = lp$row_index$sense[keep_r],
                       lo = lp$row_lo, up = lp$row_up)
    cols <- data.frame(j = lp$col_index$jred[keep_c],
                       full = lp$col_index$j[keep_c],
                       name = golden_label(lp$col_index, "j")[keep_c],
                       lo = lp$col_lo, up = lp$col_up, obj = lp$obj)
    rows <- rows[order(rows$i), , drop = FALSE]
    cols <- cols[order(cols$j), , drop = FALSE]
    elim <- as.data.frame(lp$eliminated)[, c("col", "ref", "coef", "pass")]
    elim <- elim[order(elim$pass, elim$col), , drop = FALSE]
  } else {
    rows <- data.frame(i = lp$row_index$i,
                       name = golden_label(lp$row_index, "i"),
                       sense = lp$row_index$sense,
                       lo = lp$row_lo, up = lp$row_up)
    cols <- data.frame(j = lp$col_index$j,
                       name = golden_label(lp$col_index, "j"),
                       lo = lp$col_lo, up = lp$col_up, obj = lp$obj)
    elim <- NULL
  }
  rownames(rows) <- NULL
  rownames(cols) <- NULL
  list(rows = rows, cols = cols, triplets = trip, eliminated = elim,
       meta = list(nrow = nrow(A), ncol = ncol(A), nnz = nrow(trip),
                   maximum = isTRUE(lp$maximum),
                   objective_symbol = lp$objective_symbol))
}

golden_write_tables <- function(tables, dir, meta_extra = list()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  fmt <- function(df) {
    for (nm in names(df)) {
      if (is.double(df[[nm]])) df[[nm]] <- sprintf("%.17g", df[[nm]])
    }
    df
  }
  write <- function(df, name) {
    utils::write.csv(fmt(df), file.path(dir, name), row.names = FALSE, quote = TRUE)
  }
  write(tables$rows, "rows.csv")
  write(tables$cols, "cols.csv")
  write(tables$triplets, "triplets.csv")
  if (!is.null(tables$eliminated)) write(tables$eliminated, "eliminated.csv")
  meta <- c(tables$meta, meta_extra)
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, digits = NA),
             file.path(dir, "meta.json"))
  invisible(dir)
}

golden_read_tables <- function(dir) {
  read <- function(name) {
    f <- file.path(dir, name)
    if (!file.exists(f)) return(NULL)
    df <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
    for (nm in intersect(names(df), c("i", "j", "full", "col", "ref", "pass"))) {
      df[[nm]] <- as.integer(df[[nm]])
    }
    for (nm in intersect(names(df), c("lo", "up", "obj", "coef"))) {
      df[[nm]] <- as.numeric(df[[nm]])
    }
    df
  }
  list(rows = read("rows.csv"), cols = read("cols.csv"),
       triplets = read("triplets.csv"), eliminated = read("eliminated.csv"),
       meta = jsonlite::fromJSON(file.path(dir, "meta.json")))
}
