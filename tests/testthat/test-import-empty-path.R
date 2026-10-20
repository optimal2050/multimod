# `get_data_path()` and `detect_arrow_format()` must return NULL rather than
# error when a recorded path resolves nowhere. The companion test - that an
# EMPTY energyRt parameter with a dangling path imports as empty rather than
# failing - moved to energyRt with collect_scenario_parameter_data() on
# 2026-09-24.

test_that("get_data_path and detect_arrow_format return NULL on dangling paths", {
  # multimod's own objects are lists; a missing path is NULL, not an error
  expect_null(get_data_path(list(misc = list())))
  expect_null(detect_arrow_format(file.path(tempdir(), "nope-not-here")))
})
