# An empty table whose recorded misc$path resolves nowhere must import as
# empty, not error: an EMPTY table legitimately gets no on-disk file when a
# scenario is saved (e.g. an unused `group` set), while its shell may still
# carry the path the writer would have used. The import's no-silent-default
# hard stop is reserved for the aggregate case (nothing reachable at all,
# .report_rows) -- per-object emptiness is data, not failure.

test_that("an empty parameter with a dangling path imports as empty", {
  setClass("mm_mock_param",
           representation(data = "ANY", misc = "list", name = "character",
                          dimSets = "character"),
           where = environment())
  p <- new("mm_mock_param",
           data = data.frame(),
           misc = list(path = file.path(tempdir(), "does-not-exist-anywhere")),
           name = "group", dimSets = character())

  out <- collect_scenario_parameter_data(p)
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("get_data_path and detect_arrow_format return NULL on dangling paths", {
  # multimod's own objects are lists; a missing path is NULL, not an error
  expect_null(get_data_path(list(misc = list())))
  expect_null(detect_arrow_format(file.path(tempdir(), "nope-not-here")))
})
