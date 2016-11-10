test_that("variable_importance", {
  lapply(fits_regr, function(fit) {
    imp <- variable_importance(fit, c("X1", "X2"), nperm = nperm, data = df_regr)
    expect_that(imp, is_a("list"))
    expect_that(length(imp) == 2L, is_true())
    expect_that(names(imp), equals(c("X1", "X2")))
    expect_that(unique(sapply(imp, class)), equals("numeric"))
    plot_imp(imp)

    imp_int <- variable_importance(fit, c("X1", "X2"), TRUE, nperm = nperm,
      data = df_regr)
    expect_that(imp_int, is_a("numeric"))
  })
  
  lapply(fits_classif, function(fit) {
    imp <- variable_importance(fit, c("X1", "X2"), nperm = nperm, data = df_classif)
    expect_that(imp, is_a("list"))
    expect_that(length(imp) == 2L, is_true())
    expect_that(names(imp), equals(c("X1", "X2")))
    plot_imp(imp)

    imp_int <- variable_importance(fit, c("X1", "X2"), TRUE, nperm = nperm,
      data = df_classif)
    expect_that(imp_int, is_a("numeric"))
  })
})
