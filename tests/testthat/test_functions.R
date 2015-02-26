test_that("ivar_points works correctly", {
    df <- data.frame("x" = 1:20, "y" = rep(1, 20))
    expect_that(length(ivar_points(df, "x", 10)), equals(10))
    expect_that(length(unique(ivar_points(df, "x", 10))), equals(length(ivar_points(df, "x", 10))))
    expect_that(ivar_points(df, "x", nrow(df)), equals(df[, "x"]))
})
