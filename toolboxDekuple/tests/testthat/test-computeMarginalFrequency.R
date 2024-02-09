test_that("compute_marginal_frequency works with numeric columns", {

  df <- data.frame(sexe = c("homme", "femme"), "yeux_marron" = c(35,22), "yeux_bleus" = c(23,27), "yeux_verts" = c(12,33))
  res <- compute_marginal_frequency(df)
  expect_equal(ncol(df) + 1, ncol(res))
  expect_equal(nrow(df) + 1, nrow(res))
  expect_true("Total" %in% colnames(res))
  expect_true("Total" %in% res$sexe)

})


test_that("compute_marginal_frequency works without numeric columns", {

  df <- data.frame(sexe = c("homme", "femme"), "first_letter" = c("h", "f"), "last_letter" = c("e","e"))
  res <- compute_marginal_frequency(df)
  expect_equal(ncol(df), ncol(res))
  expect_equal(nrow(df), nrow(res))
  expect_true(!"Total" %in% colnames(res))
})
