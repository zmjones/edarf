test_that("regression", {
  n <- c(10, 25)
  lapply(fits_regr, function(fit) {
    pd <- partial_dependence(fit, "X1", n, data = df_regr)
    expect_that(pd, is_a("data.frame"))
    expect_that(any(c("y", "prediction") %in% colnames(pd)), is_true())
    expect_that(pd$X1, is_a("numeric"))
    expect_that(all(sapply(pd, class) == "numeric"), is_true())
    expect_that(nrow(pd), equals(10))
    plot_pd(pd)

    pd_int <- partial_dependence(fit, c("X1", "X3"), n, TRUE, data = df_regr)
    expect_that(any(c("y", "prediction") %in% colnames(pd_int)), is_true())
    expect_that(pd_int$X1, is_a("numeric"))
    expect_that(pd_int$X3, is_a("ordered"))
    plot_pd(pd_int, "X3")

    pd_both <- partial_dependence(fit, c("X1", "X3"), n, FALSE, data = df_regr)
    expect_that(any(c("y", "prediction") %in% colnames(pd_both)), is_true())
    expect_that(all(c("X1", "X3") %in% colnames(pd_both)), is_true())
    expect_warning(plot_pd(pd_both))
  })
})

test_that("classification", {
  n <- c(10, 25)
  lapply(fits_classif, function(fit) {
    pd <- partial_dependence(fit, "X1", n, data = df_classif)
    expect_that(colnames(pd), equals(c("X1", "0", "1")))
    expect_that(all(sapply(pd, class) == "numeric"), is_true())
    plot_pd(pd)

    pd_int <- partial_dependence(fit, c("X1", "X3"), n, interaction = TRUE,
      data = df_classif)
    expect_that(colnames(pd_int), equals(c("X1", "X3", "0", "1")))
    plot_pd(pd_int, "X3")
    plot_pd(pd_int)

    pd_lst <- partial_dependence(fit, c("X1", "X3"), n, data = df_classif)
    expect_that(colnames(pd_lst), equals(c("X1", "X3", "0", "1")))
    expect_warning(plot_pd(pd_lst))
  })
})

test_that("multivariate", {
  n <- c(10, 25)
  lapply(fits_multi, function(fit) {
    pd <- partial_dependence(fit, "X1", n, data = df_multi)
    expect_that(colnames(pd), equals(c("X1", "yr", "0", "1")))
    expect_that(all(sapply(pd, class) == "numeric"), is_true())

    pd_int <- partial_dependence(fit, c("X1", "X2"), n, interaction = TRUE,
      data = df_multi)
    expect_that(colnames(pd_int), equals(c("X1", "X2", "yr", "0", "1")))
    expect_that(all(sapply(pd_int, class) == "numeric"), is_true())

    pd_lst <- partial_dependence(fit, c("X1", "X3"), n, data = df_multi)
    expect_that(colnames(pd_lst), equals(c("X1", "X3", "yr", "0", "1")))
  })
})
