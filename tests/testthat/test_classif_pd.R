library(randomForest)
library(party)
library(randomForestSRC)

df <-  read.csv("df_classif.csv")
df$y <- as.factor(df$y)
X <- df[, -which(colnames(df) == "y")]
y <- df$y
fit_rf_f <- randomForest(y ~ ., df, keep.inbag = TRUE)
fit_rf_d <- randomForest(X, y, keep.inbag = TRUE)
fit_cf <- cforest(y ~ ., df, controls = cforest_control(mtry = 1))
fit_rfsrc <- rfsrc(y ~ ., df)
cutoff <- 5L

test_that("randomForest.default", {
  pd <- partial_dependence(fit_rf_d, df, "X1", cutoff, type = "class")
  expect_that(pd, is_a("data.frame"))
  expect_that(colnames(pd), equals(c("X1", "y")))
  expect_that(class(pd$y) == "factor" & class(pd$X1) == "numeric", is_true())
  expect_that(nrow(pd), equals(cutoff))

  pd_int <- partial_dependence(fit_rf_d, df, c("X1", "X2"), cutoff, TRUE, type = "class")
  expect_that(pd_int$y, is_a("factor"))
  expect_that(all(sapply(pd_int[, -which(colnames(pd_int) == "y")], class) == "numeric"), is_true())
  expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

  pd_prob <- partial_dependence(fit_rf_d, df, "X1", cutoff, type = "prob")
  expect_that(all(sapply(pd_prob, class) == "numeric"), is_true())
  expect_that(colnames(pd_prob), equals(c("X1", "0", "1")))

  pd_drop <- partial_dependence(fit_rf_d, df, "X1", cutoff, type = "prob", drop_levels = "0")
  expect_that(colnames(pd_drop), equals(c("X1", "1")))

  pd_int_prob <- partial_dependence(fit_rf_d, df, c("X1", "X2"), cutoff, TRUE, type = "prob")
  expect_that(colnames(pd_int_prob), equals(c("X1", "X2", "0", "1")))

  pd_lst <- partial_dependence(fit_rf_d, df, c("X1", "X2"), cutoff, type = "class")
  expect_that(colnames(pd_lst), equals(c("y", "variable", "value")))
  expect_that(pd_lst$value, is_a("numeric"))
  expect_that(pd_lst$y, is_a("factor"))
  expect_that(pd_lst$variable, is_a("factor"))
})

test_that("randomForest.formula", {
  pd <- partial_dependence(fit_rf_f, df, "X1", cutoff, type = "class")
  expect_that(pd, is_a("data.frame"))
  expect_that(colnames(pd), equals(c("X1", "y")))
  expect_that(class(pd$y) == "factor" & class(pd$X1) == "numeric", is_true())
  expect_that(nrow(pd), equals(cutoff))

  pd_int <- partial_dependence(fit_rf_f, df, c("X1", "X2"), cutoff, TRUE, type = "class")
  expect_that(pd_int$y, is_a("factor"))
  expect_that(all(sapply(pd_int[, -which(colnames(pd_int) == "y")], class) == "numeric"), is_true())
  expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

  pd_prob <- partial_dependence(fit_rf_f, df, "X1", cutoff, type = "prob")
  expect_that(all(sapply(pd_prob, class) == "numeric"), is_true())
  expect_that(colnames(pd_prob), equals(c("X1", "0", "1")))

  pd_drop <- partial_dependence(fit_rf_f, df, "X1", cutoff, type = "prob", drop_levels = "0")
  expect_that(colnames(pd_drop), equals(c("X1", "1")))

  pd_int_prob <- partial_dependence(fit_rf_f, df, c("X1", "X2"), cutoff, TRUE, type = "prob")
  expect_that(colnames(pd_int_prob), equals(c("X1", "X2", "0", "1")))

  pd_lst <- partial_dependence(fit_rf_f, df, c("X1", "X2"), cutoff, type = "class")
  expect_that(colnames(pd_lst), equals(c("y", "variable", "value")))
  expect_that(pd_lst$value, is_a("numeric"))
  expect_that(pd_lst$y, is_a("factor"))
  expect_that(pd_lst$variable, is_a("factor"))
})

test_that("cforest", {
  pd <- partial_dependence(fit_cf, df, "X1", cutoff, type = "class")
  expect_that(pd, is_a("data.frame"))
  expect_that(colnames(pd), equals(c("X1", "y")))
  expect_that(class(pd$y) == "factor" & class(pd$X1) == "numeric", is_true())
  expect_that(nrow(pd), equals(cutoff))

  pd_int <- partial_dependence(fit_cf, df, c("X1", "X2"), cutoff, TRUE, type = "class")
  expect_that(pd_int$y, is_a("factor"))
  expect_that(all(sapply(pd_int[, -which(colnames(pd_int) == "y")], class) == "numeric"), is_true())
  expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

  pd_prob <- partial_dependence(fit_cf, df, "X1", cutoff, type = "prob")
  expect_that(all(sapply(pd_prob, class) == "numeric"), is_true())
  expect_that(colnames(pd_prob), equals(c("X1", "0", "1")))

  pd_drop <- partial_dependence(fit_cf, df, "X1", cutoff, type = "prob", drop_levels = "0")
  expect_that(colnames(pd_drop), equals(c("X1", "1")))

  pd_int_prob <- partial_dependence(fit_cf, df, c("X1", "X2"), cutoff, TRUE, type = "prob")
  expect_that(colnames(pd_int_prob), equals(c("X1", "X2", "0", "1")))

  pd_lst <- partial_dependence(fit_cf, df, c("X1", "X2"), cutoff, type = "class")
  expect_that(colnames(pd_lst), equals(c("y", "variable", "value")))
  expect_that(pd_lst$value, is_a("numeric"))
  expect_that(pd_lst$y, is_a("factor"))
  expect_that(pd_lst$variable, is_a("factor"))
})

test_that("rfsrc", {
  pd <- partial_dependence(fit_rfsrc, df, "X1", cutoff, type = "class")
  expect_that(pd, is_a("data.frame"))
  expect_that(colnames(pd), equals(c("X1", "y")))
  expect_that(class(pd$y) == "factor" & class(pd$X1) == "numeric", is_true())
  expect_that(nrow(pd), equals(cutoff))

  pd_int <- partial_dependence(fit_rfsrc, df, c("X1", "X2"), cutoff, TRUE, type = "class")
  expect_that(pd_int$y, is_a("factor"))
  expect_that(all(sapply(pd_int[, -which(colnames(pd_int) == "y")], class) == "numeric"), is_true())
  expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

  pd_prob <- partial_dependence(fit_rfsrc, df, "X1", cutoff, type = "prob")
  expect_that(all(sapply(pd_prob, class) == "numeric"), is_true())
  expect_that(colnames(pd_prob), equals(c("X1", "0", "1")))

  pd_drop <- partial_dependence(fit_rfsrc, df, "X1", cutoff, type = "prob", drop_levels = "0")
  expect_that(colnames(pd_drop), equals(c("X1", "1")))

  pd_int_prob <- partial_dependence(fit_rfsrc, df, c("X1", "X2"), cutoff, TRUE, type = "prob")
  expect_that(colnames(pd_int_prob), equals(c("X1", "X2", "0", "1")))

  pd_lst <- partial_dependence(fit_rfsrc, df, c("X1", "X2"), cutoff, type = "class")
  expect_that(colnames(pd_lst), equals(c("y", "variable", "value")))
  expect_that(pd_lst$value, is_a("numeric"))
  expect_that(pd_lst$y, is_a("factor"))
  expect_that(pd_lst$variable, is_a("factor"))
})
