test_that("ivar_points works correctly", {
    df <- data.frame("x" = 1:20, "y" = rep(1, 20))
    expect_that(length(ivar_points(df, "x", 10)), equals(10))
    expect_that(length(unique(ivar_points(df, "x", 10))), equals(length(ivar_points(df, "x", 10))))
    expect_that(ivar_points(df, "x", nrow(df)), equals(df[, "x"]))
})

test_that("fix_classes works correctly", {
    df <- data.frame(x = letters,
                     y = factor(letters),
                     z = seq(0, 1, length.out = length(letters)),
                     a = 1:length(letters), stringsAsFactors = FALSE)
    ndf <- data.frame(x = as.factor(df$x),
                      y = as.character(df$y),
                      z = as.character(df$z),
                      a = as.numeric(df$a), stringsAsFactors = FALSE)
    out <- fix_classes(df, ndf)
    expect_that(out$x, is_a("character"))
    expect_that(out$y, is_a("factor"))
    expect_that(out$z, is_a("numeric"))
    expect_that(out$a, is_a("integer"))
})
