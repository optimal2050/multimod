# Copy the native core into the R package's src/ tree.
#
#   Rscript tools/sync_core.R
#
# `core/` is the canonical source of the C++ library, shared by every
# binding. R compiles only what lies under `src/`, and the package tarball
# carries no `core/`, so the library is mirrored into `src/core/` (committed)
# before a build. Nothing under `src/core/` is edited by hand.
src <- file.path("core")
dst <- file.path("src", "core")
unlink(dst, recursive = TRUE)
for (sub in c("include", "src")) {
  from <- file.path(src, sub)
  files <- list.files(from, recursive = TRUE, full.names = TRUE)
  for (f in files) {
    to <- file.path(dst, sub, sub(paste0("^", from, "/"), "", f))
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(f, to, overwrite = TRUE)
  }
}
cat("core mirrored to src/core:", length(list.files(dst, recursive = TRUE)), "files\n")
