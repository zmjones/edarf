test_that("variable_importance", {
  lapply(fits_regr, function(fit) {
    imp <- variable_importance(fit, "X1", "aggregate", nperm = nperm, data = df_regr)
    expect_that(imp, is_a("numeric"))
    expect_that(length(imp) == 1, is_true())
    expect_that(is.finite(imp), is_true())
    plot_imp(imp)

    imp_local <- variable_importance(fit, "X1", "local", nperm = nperm, data = df_regr)
    expect_that(imp_local, is_a("data.frame"))
    expect_that(nrow(imp_local), equals(nrow(df_regr)))
    expect_that(imp_local$X1, is_a("numeric"))
    plot_imp(imp_local)

    imp_int <- variable_importance(fit, c("X1", "X2"), "aggregate", TRUE, nperm = nperm, data = df_regr)
    expect_that(imp_int, is_a("numeric"))
    expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
    expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))
    plot_imp(imp_int)

    imp_int_local <- variable_importance(fit, c("X1", "X2"), "local", TRUE, nperm = nperm, data = df_regr)
    expect_that(imp_int_local, is_a("data.frame"))
    expect_that(dim(imp_int_local), equals(c(nrow(df_regr), 4)))
    expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
    plot_imp(imp_int_local)
  })
  
  lapply(fits_classif, function(fit) {
    imp <- variable_importance(fit, "X1", "aggregate", nperm = nperm, data = df_classif)
    expect_that(imp, is_a("numeric"))
    expect_that(length(imp) == 1, is_true())
    expect_that(imp < 1 & imp > -1, is_true())
    plot_imp(imp)

    imp_local <- variable_importance(fit, "X1", "local", nperm = nperm, data = df_classif)
    expect_that(imp_local, is_a("data.frame"))
    expect_that(nrow(imp_local), equals(nrow(df_classif)))
    expect_that(imp_local$X1, is_a("numeric"))
    plot_imp(imp_local)

    imp_int <- variable_importance(fit, c("X1", "X2"), "aggregate", TRUE, nperm = nperm, data = df_classif)
    expect_that(imp_int, is_a("numeric"))
    expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
    expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))
    plot_imp(imp_int)

    imp_int_local <- variable_importance(fit, c("X1", "X2"), "local", TRUE, nperm = nperm, data = df_classif)
    expect_that(imp_int_local, is_a("data.frame"))
    expect_that(dim(imp_int_local), equals(c(nrow(df_classif), 4)))
    expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
    plot_imp(imp_int_local)
  })
})
