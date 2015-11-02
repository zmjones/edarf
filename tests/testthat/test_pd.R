test_that("regression", {
  lapply(fits_regr, function(fit) {
    pd <- partial_dependence(fit, df_regr, "X1", cutoff)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("y", "X1")))
    expect_that(all(sapply(pd, class) == "numeric"), is_true())
    expect_that(nrow(pd), equals(cutoff))
    plot_pd(pd)

    pd_int <- partial_dependence(fit, df_regr, c("X1", "X2"), cutoff, TRUE)
    expect_that(all(sapply(pd_int, class) == "numeric"), is_true())
    expect_that(colnames(pd_int), equals(c("y", "X1", "X2")))
    plot_pd(pd_int)

    pd_ci <- partial_dependence(fit, df_regr, "X1", cutoff, ci = TRUE)
    expect_that(colnames(pd_ci), equals(c("lower", "y", "upper", "X1")))
    expect_that(all(sapply(pd_ci, class) == "numeric"), is_true())
    expect_that(all(pd_ci$lower <= pd_ci$y) & all(pd_ci$y <= pd_ci$upper), is_true())
    plot_pd(pd_ci)

    pd_both <- partial_dependence(fit, df_regr, c("X1", "X3"), cutoff, TRUE, ci = TRUE)
    expect_that(colnames(pd_both), equals(c("lower", "y", "upper", "X1", "X3")))
    expect_that(all(pd_both$lower <= pd_both$y) & all(pd_both$y <= pd_both$upper), is_true())
    plot_pd(pd_both, "X3")
  })
})

test_that("classification", {
  lapply(fits_classif, function(fit) {
    pd <- partial_dependence(fit, df_classif, "X1", cutoff)
    expect_that(colnames(pd), equals(c("0", "1", "X1")))
    expect_that(all(sapply(pd, class) == "numeric"), is_true())
    plot_pd(pd)

    pd_int <- partial_dependence(fit, df_classif, c("X1", "X2"), cutoff, interaction = TRUE)
    expect_that(colnames(pd_int), equals(c("0", "1", "X1", "X2")))
    expect_that(all(sapply(pd_int, class) == "numeric"), is_true())
    plot_pd(pd_int)

    pd_lst <- partial_dependence(fit, df_classif, c("X1", "X3"), cutoff)
    expect_that(colnames(pd_lst), equals(c("0", "1", "X1", "X3")))
    plot_pd(pd_lst)
  })
})

test_that("multivariate", {
  lapply(fits_multi, function(fit) {
    pd <- partial_dependence(fit, df_multi, "X1", cutoff)
    expect_that(colnames(pd), equals(c("yr", "0", "1", "X1")))
    expect_that(all(sapply(pd, class) == "numeric"), is_true())

    pd_int <- partial_dependence(fit, df_multi, c("X1", "X2"), cutoff, interaction = TRUE)
    expect_that(colnames(pd_int), equals(c("yr", "0", "1", "X1", "X2")))
    expect_that(all(sapply(pd_int, class) == "numeric"), is_true())

    pd_lst <- partial_dependence(fit, df_multi, c("X1", "X3"), cutoff)
    expect_that(colnames(pd_lst), equals(c("yr", "0", "1", "X1", "X3")))
  })
})
