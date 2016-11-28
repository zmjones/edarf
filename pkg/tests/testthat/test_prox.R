test_that("extract_proximity", {
  test_regr <- lapply(fits_regr, extract_proximity, newdata = df_regr)
  expect_true(all(sapply(test_regr, function(x) is.matrix(x) && dim(x) == c(n, n))))

  test_classif <- lapply(fits_classif, extract_proximity, newdata = df_classif)
  expect_true(all(sapply(test_classif, function(x) is.matrix(x) && dim(x) == c(n, n))))

  test_multi <- lapply(fits_multi, extract_proximity, newdata = df_multi)
  expect_true(all(sapply(test_multi, function(x) is.matrix(x) && dim(x) == c(n, n))))
})
