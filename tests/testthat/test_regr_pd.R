library(randomForest)
library(party)
library(randomForestSRC)

n <- 25
k <- 2
X <- replicate(k, rnorm(n))
beta <- rep(1, k)
y <- as.numeric(X %*% beta + rnorm(n))
df <-  data.frame(X, y)
fit_rf_f <- randomForest(y ~ ., df, keep.inbag = TRUE)
fit_rf_d <- randomForest(X, y, keep.inbag = TRUE)
fit_cf <- cforest(y ~ ., df, controls = cforest_control(mtry = 1))
fit_rfsrc <- rfsrc(y ~ ., df)
cutoff <- 5L

test_that("randomForest.formula", {
              pd <- partial_dependence(fit_rf_f, df, "X1", cutoff)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("X1", "y")))
              expect_that(all(sapply(pd, class) == "numeric"), is_true())
              expect_that(nrow(pd), equals(cutoff))

              pd_int <- partial_dependence(fit_rf_f, df, c("X1", "X2"), cutoff, TRUE)
              expect_that(all(sapply(pd_int, class) == "numeric"), is_true())
              expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

              pd_ci <- partial_dependence(fit_rf_f, df, "X1", cutoff, ci = TRUE)
              expect_that(colnames(pd_ci), equals(c("X1", "y", "variance", "low", "high")))
              expect_that(all(sapply(pd_ci, class) == "numeric"), is_true())
              expect_that(all(pd_ci$variance > 0), is_true())

              pd_both <- partial_dependence(fit_rf_f, df, c("X1", "X2"), cutoff, TRUE, TRUE)
              expect_that(colnames(pd_both), equals(c("X1", "X2", "y", "variance", "low", "high")))
              expect_that(all(sapply(pd_both, class) == "numeric"), is_true())
              expect_that(all(pd_both$variance > 0), is_true())
          })

## broken
## test_that("randomForest.default", {
##               pd <- partial_dependence(fit_rf_d, df, "X1", cutoff)
##               expect_that(pd, is_a("data.frame"))
##               expect_that(colnames(pd), equals(c("X1", "y")))
##               expect_that(all(sapply(pd, class) == "numeric"), is_true())
##               expect_that(nrow(pd), equals(cutoff))

##               pd_int <- partial_dependence(fit_rf_d, df, c("X1", "X2"), cutoff, TRUE)
##               expect_that(all(sapply(pd_int, class) == "numeric"), is_true())
##               expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

##               pd_ci <- partial_dependence(fit_rf_d, df, "X1", cutoff, ci = TRUE)
##               expect_that(colnames(pd_ci), equals(c("X1", "y", "variance", "low", "high")))
##               expect_that(all(sapply(pd_ci, class) == "numeric"), is_true())
##               expect_that(all(pd_ci$variance > 0), is_true())

##               pd_both <- partial_dependence(fit_rf_d, df, c("X1", "X2"), cutoff, TRUE, TRUE)
##               expect_that(colnames(pd_both), equals(c("X1", "X2", "y", "variance", "low", "high")))
##               expect_that(all(sapply(pd_both, class) == "numeric"), is_true())
##               expect_that(all(pd_both$variance > 0), is_true())
##           })

test_that("cforest", {
              pd <- partial_dependence(fit_cf, df, "X1", cutoff)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("X1", "y")))
              expect_that(all(sapply(pd, class) == "numeric"), is_true())
              expect_that(nrow(pd), equals(cutoff))

              pd_int <- partial_dependence(fit_cf, df, c("X1", "X2"), cutoff, TRUE)
              expect_that(all(sapply(pd_int, class) == "numeric"), is_true())
              expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

              pd_ci <- partial_dependence(fit_cf, df, "X1", cutoff, ci = TRUE)
              expect_that(colnames(pd_ci), equals(c("X1", "y", "variance", "low", "high")))
              expect_that(all(sapply(pd_ci, class) == "numeric"), is_true())
              expect_that(all(pd_ci$variance > 0), is_true())

              pd_both <- partial_dependence(fit_cf, df, c("X1", "X2"), cutoff, TRUE, TRUE)
              expect_that(colnames(pd_both), equals(c("X1", "X2", "y", "variance", "low", "high")))
              expect_that(all(sapply(pd_both, class) == "numeric"), is_true())
              expect_that(all(pd_both$variance > 0), is_true())
          })

test_that("rfsrc", {
              pd <- partial_dependence(fit_rfsrc, df, "X1", cutoff)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("X1", "y")))
              expect_that(all(sapply(pd, class) == "numeric"), is_true())
              expect_that(nrow(pd), equals(cutoff))

              pd_int <- partial_dependence(fit_rfsrc, df, c("X1", "X2"), cutoff, TRUE)
              expect_that(all(sapply(pd_int, class) == "numeric"), is_true())
              expect_that(colnames(pd_int), equals(c("X1", "X2", "y")))

              pd_ci <- partial_dependence(fit_rfsrc, df, "X1", cutoff, ci = TRUE)
              expect_that(colnames(pd_ci), equals(c("X1", "y", "variance", "low", "high")))
              expect_that(all(sapply(pd_ci, class) == "numeric"), is_true())
              expect_that(all(pd_ci$variance > 0), is_true())

              pd_both <- partial_dependence(fit_rfsrc, df, c("X1", "X2"), cutoff, TRUE, TRUE)
              expect_that(colnames(pd_both), equals(c("X1", "X2", "y", "variance", "low", "high")))
              expect_that(all(sapply(pd_both, class) == "numeric"), is_true())
              expect_that(all(pd_both$variance > 0), is_true())
          })



