test_that("compute_age works", {
  expect_equal(compute_age("1995-01-01", "1997-01-01"), 2)
  expect_equal(compute_age("1995-01-01", "2023-01-01"), 28)
  expect_error(compute_age("1995-01-01", "1993-01-01"), regexp = "start_date is supposed to be older than the end_date")
  expect_error(compute_age("1995-01-01", "2023-21-01"), regexp = "Not a date format")
})
