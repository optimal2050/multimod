# Generate the matrix goldens from the R package SOURCE.
#
# Run from the repository root:
#   Rscript specs/goldens/generate.R
#
# Cases: the two shipped example models (`toy`, and the energyRt model on the
# TOPIA R7 kit as `utopia`). A larger local model is added when
# MULTIMOD_GOLDEN_MODEL_DIR names a multimod workspace written by save_model();
# its goldens go to <that directory>/goldens/matrix/ and stay out of the
# repository.
#
# Every other implementation of the matrix route must reproduce these tables
# exactly in structure (rows, columns, nonzero pattern, senses) and to 1e-12
# in values. The known optima come from independent solves (GLPK on energyRt's
# own MathProg for `utopia`; HiGHS for `toy`).

pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
source(file.path("specs", "goldens", "matrix-tables.R"))

write_case <- function(model, dir, optimum = NULL) {
  lp <- model_to_lp(model, verbose = FALSE)
  golden_write_tables(golden_matrix_tables(lp), dir,
                      meta_extra = list(optimum = optimum))
  red <- lp_presolve(lp, verbose = FALSE)
  golden_write_tables(golden_matrix_tables(red), file.path(dir, "presolve"),
                      meta_extra = list(optimum = optimum))
  cat(sprintf("%s: %d x %d, %d nnz; presolved %d x %d, %d nnz, %d eliminated\n",
              dir, nrow(lp$A), ncol(lp$A), length(lp$A@x),
              nrow(red$A), ncol(red$A), length(red$A@x), nrow(red$eliminated)))
}

out <- file.path("specs", "goldens", "matrix")
write_case(example_models$toy, file.path(out, "toy"), optimum = 198000)
write_case(example_models$energyRt$multimod, file.path(out, "utopia"),
           optimum = 46398.4234209742)

large <- Sys.getenv("MULTIMOD_GOLDEN_MODEL_DIR", "")
if (nzchar(large)) {
  m <- load_model(large)
  write_case(m, file.path(large, "goldens", "matrix"))
}
