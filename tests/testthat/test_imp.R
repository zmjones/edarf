library(randomForest)
library(party)
library(randomForestSRC)

nperm <- 2L
df_regr <- read.csv("df_regr.csv")
df_classif <- read.csv("df_classif.csv")
df_classif$y <- as.factor(df_classif$y)

test_that("randomForest.formula", {
              fit_regr <- randomForest(y ~ ., df_regr)
              fit_classif <- randomForest(y ~ ., df_classif)

              imp <- variable_importance(fit_regr, "X1", "aggregate", nperm = nperm, data = df_regr)
              expect_that(imp, is_a("numeric"))
              expect_that(length(imp) == 1, is_true())
              expect_that(is.finite(imp), is_true())

              imp <- variable_importance(fit_classif, "X1", "aggregate", nperm = nperm, data = df_classif)
              expect_that(imp, is_a("numeric"))
              expect_that(length(imp) == 1, is_true())
              expect_that(imp < 1 & imp > -1, is_true())

              imp_local <- variable_importance(fit_regr, "X1", "local", nperm = nperm, data = df_regr)
              expect_that(imp_local, is_a("data.frame"))
              expect_that(nrow(imp_local), equals(nrow(df_regr)))
              expect_that(imp_local$X1, is_a("numeric"))

              imp_local <- variable_importance(fit_classif, "X1", "local", nperm = nperm, data = df_classif)
              expect_that(imp_local, is_a("data.frame"))
              expect_that(nrow(imp_local), equals(nrow(df_classif)))
              expect_that(imp_local$X1, is_a("numeric"))

              imp_int <- variable_importance(fit_regr, c("X1", "X2"), "aggregate", TRUE, nperm = nperm,
                                             data = df_regr)
              expect_that(imp_int, is_a("numeric"))
              expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
              expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))
                          
              imp_int <- variable_importance(fit_classif, c("X1", "X2"), "aggregate", TRUE, nperm = nperm,
                                             data = df_classif)
              expect_that(imp_int, is_a("numeric"))
              expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
              expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))

              imp_int_local <- variable_importance(fit_regr, c("X1", "X2"), "local", TRUE, nperm = nperm,
                                                   data = df_regr)
              expect_that(imp_int_local, is_a("data.frame"))
              expect_that(dim(imp_int_local), equals(c(nrow(df_regr), 4)))
              expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
              
              imp_int_local <- variable_importance(fit_classif, c("X1", "X2"), "local", TRUE, nperm = nperm,
                                                   data = df_classif)
              expect_that(imp_int_local, is_a("data.frame"))
              expect_that(dim(imp_int_local), equals(c(nrow(df_classif), 4)))
              expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
          })

## broken
## test_that("randomForest.default", {
##               fit_regr <- randomForest(as.matrix(df_regr[, 1:2]), df_regr$y)
##               fit_classif <- randomForest(as.matrix(df_classif[, 1:2]), df_classif$y)

##               imp <- variable_importance(fit_regr, "X1", "aggregate", nperm = nperm, data = df_regr)
##               expect_that(imp, is_a("numeric"))
##               expect_that(length(imp) == 1, is_true())
##               expect_that(imp > -1 * imp < 1, is_true())

##               imp <- variable_importance(fit_classif, "X1", "aggregate", nperm = nperm, data = df_classif)
##               expect_that(imp, is_a("numeric"))
##               expect_that(length(imp) == 1, is_true())
##               expect_that(imp < 1 & imp > -1, is_true())

##               imp_local <- variable_importance(fit_regr, "X1", "local", nperm = nperm, data = df_regr)
##               expect_that(imp_local, is_a("data.frame"))
##               expect_that(nrow(imp_local), equals(nrow(df_regr)))
##               expect_that(imp_local$X1, is_a("numeric"))

##               imp_local <- variable_importance(fit_classif, "X1", "local", nperm = nperm, data = df_classif)
##               expect_that(imp_local, is_a("data.frame"))
##               expect_that(nrow(imp_local), equals(nrow(df_classif)))
##               expect_that(imp_local$X1, is_a("numeric"))

##               imp_int <- variable_importance(fit_regr, c("X1", "X2"), "aggregate", TRUE, nperm = nperm,
##                                              data = df_regr)
##               expect_that(imp_int, is_a("numeric"))
##               expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
##               expect_that(all(imp_int[c("additive", "joint")] > imp_int[c("X1", "X2")]), is_true())

##               imp_int <- variable_importance(fit_classif, c("X1", "X2"), "aggregate", TRUE, nperm = nperm,
##                                              data = df_classif)
##               expect_that(imp_int, is_a("numeric"))
##               expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
##               expect_that(all(imp_int[c("additive", "joint")] > imp_int[c("X1", "X2")]), is_true())

##               imp_int_local <- variable_importance(fit_regr, c("X1", "X2"), "local", TRUE, nperm = nperm,
##                                                    data = df_regr)
##               expect_that(imp_int_local, is_a("data.frame"))
##               expect_that(dim(imp_int_local), equals(c(nrow(df_regr), 4)))
##               expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
              
##               imp_int_local <- variable_importance(fit_classif, c("X1", "X2"), "local", TRUE, nperm = nperm,
##                                                    data = df_classif)
##               expect_that(imp_int_local, is_a("data.frame"))
##               expect_that(dim(imp_int_local), equals(c(nrow(df_classif), 4)))
##               expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
##           })

test_that("cforest", {
              fit_regr <- cforest(y ~ ., df_regr, controls = cforest_control(mtry = 1))
              fit_classif <- cforest(y ~ ., df_classif, controls = cforest_control(mtry = 1))

              imp <- variable_importance(fit_regr, "X1", "aggregate", nperm = nperm)
              expect_that(imp, is_a("numeric"))
              expect_that(length(imp) == 1, is_true())
              expect_that(is.finite(imp), is_true())

              imp <- variable_importance(fit_classif, "X1", "aggregate", nperm = nperm)
              expect_that(imp, is_a("numeric"))
              expect_that(length(imp) == 1, is_true())
              expect_that(imp < 1 & imp > -1, is_true())

              imp_local <- variable_importance(fit_regr, "X1", "local", nperm = nperm)
              expect_that(imp_local, is_a("data.frame"))
              expect_that(nrow(imp_local), equals(nrow(df_regr)))
              expect_that(imp_local$X1, is_a("numeric"))

              imp_local <- variable_importance(fit_classif, "X1", "local", nperm = nperm)
              expect_that(imp_local, is_a("data.frame"))
              expect_that(nrow(imp_local), equals(nrow(df_classif)))
              expect_that(imp_local$X1, is_a("numeric"))

              imp_int <- variable_importance(fit_regr, c("X1", "X2"), "aggregate", TRUE, nperm = nperm)
              expect_that(imp_int, is_a("numeric"))
              expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
              expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))

              imp_int <- variable_importance(fit_classif, c("X1", "X2"), "aggregate", TRUE, nperm = nperm)
              expect_that(imp_int, is_a("numeric"))
              expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
              expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))

              imp_int_local <- variable_importance(fit_regr, c("X1", "X2"), "local", TRUE, nperm = nperm)
              expect_that(imp_int_local, is_a("data.frame"))
              expect_that(dim(imp_int_local), equals(c(nrow(df_regr), 4)))
              expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
              
              imp_int_local <- variable_importance(fit_classif, c("X1", "X2"), "local", TRUE, nperm = nperm)
              expect_that(imp_int_local, is_a("data.frame"))
              expect_that(dim(imp_int_local), equals(c(nrow(df_classif), 4)))
              expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
          })

test_that("rfsrc", {
              fit_regr <- rfsrc(y ~ ., df_regr)
              fit_classif <- rfsrc(y ~ ., df_classif)

              imp <- variable_importance(fit_regr, "X1", "aggregate", nperm = nperm)
              expect_that(imp, is_a("numeric"))
              expect_that(length(imp) == 1, is_true())
              expect_that(is.finite(imp), is_true())

              imp <- variable_importance(fit_classif, "X1", "aggregate", nperm = nperm)
              expect_that(imp, is_a("numeric"))
              expect_that(length(imp) == 1, is_true())
              expect_that(imp < 1 & imp > -1, is_true())

              imp_local <- variable_importance(fit_regr, "X1", "local", nperm = nperm)
              expect_that(imp_local, is_a("data.frame"))
              expect_that(nrow(imp_local), equals(nrow(df_regr)))
              expect_that(imp_local$X1, is_a("numeric"))

              imp_local <- variable_importance(fit_classif, "X1", "local", nperm = nperm)
              expect_that(imp_local, is_a("data.frame"))
              expect_that(nrow(imp_local), equals(nrow(df_classif)))
              expect_that(imp_local$X1, is_a("numeric"))

              imp_int <- variable_importance(fit_regr, c("X1", "X2"), "aggregate", TRUE, nperm = nperm)
              expect_that(imp_int, is_a("numeric"))
              expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
              expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))

              imp_int <- variable_importance(fit_classif, c("X1", "X2"), "aggregate", TRUE, nperm = nperm)
              expect_that(imp_int, is_a("numeric"))
              expect_that(names(imp_int), equals(c("X1", "X2", "additive", "joint")))
              expect_that(unname(imp_int["additive"]), equals(sum(imp_int[c("X1", "X2")])))

              imp_int_local <- variable_importance(fit_regr, c("X1", "X2"), "local", TRUE, nperm = nperm)
              expect_that(imp_int_local, is_a("data.frame"))
              expect_that(dim(imp_int_local), equals(c(nrow(df_regr), 4)))
              expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
              
              imp_int_local <- variable_importance(fit_classif, c("X1", "X2"), "local", TRUE, nperm = nperm)
              expect_that(imp_int_local, is_a("data.frame"))
              expect_that(dim(imp_int_local), equals(c(nrow(df_classif), 4)))
              expect_that(all(sapply(imp_int_local, class) == "numeric"), is_true())
          })
