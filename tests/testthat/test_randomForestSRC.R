library(randomForestSRC)
data("swiss")
data("iris")
data("veteran")

## regression
## regression, ci
## regression, vector var, ci
## regression, vector var
## regression, interaction
## regression, interaction, ci
## classification
## classification, vector var
## classification, interaction
## classification, prob
## classification, vector var, prob
## classification, interaction, prob
## survival
## survival vector var
## survival interaction

## test regression
fit <- rfsrc(Fertility ~ ., swiss)

test_that("regression", {
              pd <- partial_dependence(fit, var = "Education", ci = FALSE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Education", "Fertility")))
              expect_that(pd$Fertility, is_a("numeric"))
              ## expect_that(pd$Education, is_a("integer")) ## fixme, rfsrc casts integers to numerics
              expect_that(pd$Education, is_a("numeric"))
          })

test_that("regression and ci", {
              pd <- partial_dependence(fit, var = "Education", ci = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$variance, is_a("numeric"))
              expect_that(pd$low, is_a("numeric"))
              expect_that(pd$high, is_a("numeric"))
              ## expect_that(pd$Education, is_a("integer"))
              expect_that(pd$Education, is_a("numeric"))
          })

test_that("regression, ci, and vector input", {
              pd <- partial_dependence(fit, var = c("Education", "Agriculture"), interaction = FALSE, ci = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "Fertility", "variance", "variable", "low", "high")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$variance, is_a("numeric"))
              expect_that(pd$low, is_a("numeric"))
              expect_that(pd$high, is_a("numeric"))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("regression and vector input", {
              pd <- partial_dependence(fit, var = c("Education", "Agriculture"),
                                       interaction = FALSE, ci = FALSE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("regression and interaction", {
              pd <- partial_dependence(fit, var = c("Education", "Agriculture"),
                                       interaction = TRUE, ci = FALSE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
              expect_that(pd$Fertility, is_a("numeric"))
              ## expect_that(pd$Education, is_a("integer"))
              expect_that(pd$Education, is_a("numeric"))
              expect_that(pd$Agriculture, is_a("numeric"))
          })

test_that("regression, interactions, and ci", {
              pd <- partial_dependence(fit, var = c("Education", "Agriculture"),
                                       interaction = TRUE, ci = TRUE)
              expect_that(pd, is_a("data.frame"))
              ## expect_that(pd$Education, is_a("integer"))
              expect_that(pd$Education, is_a("numeric"))
              expect_that(pd$Agriculture, is_a("numeric"))
              expect_that(pd$variance, is_a("numeric"))
              expect_that(pd$low, is_a("numeric"))
              expect_that(pd$high, is_a("numeric"))
          })

## test classification
fit <- rfsrc(Species ~ ., iris)

test_that("classification", {
              pd <- partial_dependence(fit, var = "Petal.Width")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
              expect_that(pd$Species, is_a("factor"))
          })

test_that("classification and vector input", {
              pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"))
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "Species", "variable")))
              expect_that(pd$value, is_a("numeric"))
              ## expect_that(pd$Species, is_a("factor")) ## fixme?
              expect_that(pd$variable, is_a("character"))
          })

test_that("classification and interaction", {
              pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"), interaction = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$Petal.Length, is_a("numeric"))
              expect_that(pd$Species, is_a("factor"))
          })

test_that("classification and prob output", {
              pd <- partial_dependence(fit, var = "Petal.Width", type = "prob")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$setosa, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
          })

test_that("classification with prob output and drop_levels", {
              pd <- partial_dependence(fit, var = "Petal.Width", type = "prob", drop_levels = "setosa")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "versicolor", "virginica")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
          })

test_that("classification, vector input, and prob output", {
              pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"), type = "prob")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$setosa, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("classification, interaction, and prob output", {
              pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"), interaction = TRUE, type = "prob")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa",
                                                 "versicolor", "virginica")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$Petal.Length, is_a("numeric"))
              expect_that(pd$setosa, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
          })

## test survival
fit <- rfsrc(Surv(time, status) ~ ., veteran)

test_that("survival", {
              pd <- partial_dependence(fit, var = "age")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("age", "chf")))
              expect_that(pd$age, is_a("integer"))
              expect_that(pd$chf, is_a("numeric"))
          })

test_that("survival and vector input", {
              pd <- partial_dependence(fit, var = c("age", "diagtime"))
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "chf", "variable")))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$chf, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("survival and interaction", {
              pd <- partial_dependence(fit, var = c("age", "diagtime"), interaction = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("age", "diagtime", "chf")))
              expect_that(pd$age, is_a("integer"))
              expect_that(pd$diagtime, is_a("integer"))
              expect_that(pd$chf, is_a("numeric"))
          })

## permute
## random
## permute.ensemble
## random.ensemble

fit <- rfsrc(Species ~ ., iris, importance = "permute")

test_that("extractor works with permutation accuracy", {
              imp <- variable_importance(fit)
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
          })

test_that("extractor works with class levels", {
              imp <- variable_importance(fit, class_levels = TRUE)
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
              expect_that(ncol(imp), equals(4))
              expect_that(colnames(imp), equals(c("setosa", "versicolor", "virginica", "labels")))
          })

fit <- rfsrc(Species ~ ., iris, importance = "random")

test_that("extractor works with random accuracy", {
              imp <- variable_importance(fit, type = "random")
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
          })

fit <- rfsrc(Species ~ ., iris, importance = "permute.ensemble")

test_that("extractor works with permute.ensemble accuracy", {
              imp <- variable_importance(fit, type = "permute.ensemble")
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
          })

fit <- rfsrc(Species ~ ., iris, importance = "random.ensemble")

test_that("extractor works with permutation accuracy", {
              imp <- variable_importance(fit, type = "random.ensemble")
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
          })

## proximity
## proximity newdata
## proximity oob
## proximity oob newdata
## proximity all
## proximity all newdata

fit <- rfsrc(Species ~ ., iris, proximity = TRUE)

test_that("extractor works with proximity", {
              prox <- extract_proximity(fit)
              expect_that(prox, is_a("matrix"))
              expect_that(dim(prox), equals(c(150, 150)))
              expect_that(unname(diag(prox)), equals(rep(1, 150)))
          })

test_that("extractor works with proximity and newdata", {
              prox <- extract_proximity(fit, iris[1:10, ])
              expect_that(prox, is_a("matrix"))
              expect_that(dim(prox), equals(c(10, 10)))
              expect_that(unname(diag(prox)), equals(rep(1, 10)))
          })

fit <- rfsrc(Species ~ ., iris, proximity = "oob")

test_that("extractor works with proximity = \"oob\"", {
              prox <- extract_proximity(fit)
              expect_that(prox, is_a("matrix"))
              expect_that(dim(prox), equals(c(150, 150)))
              expect_that(unname(diag(prox)), equals(rep(1, 150)))
          })

test_that("extractor works with proximity = \"oob\" and newdata", {
              prox <- extract_proximity(fit, iris[1:10, ])
              expect_that(prox, is_a("matrix"))
              expect_that(dim(prox), equals(c(10, 10)))
              expect_that(unname(diag(prox)), equals(rep(1, 10)))
          })

fit <- rfsrc(Species ~ ., iris, proximity = "all")

test_that("extractor works with proximity = \"all\"", {
              prox <- extract_proximity(fit)
              expect_that(prox, is_a("matrix"))
              expect_that(dim(prox), equals(c(150, 150)))
              expect_that(unname(diag(prox)), equals(rep(1, 150)))
          })

test_that("extractor works with proximity = \"all\" and newdata", {
              prox <- extract_proximity(fit, iris[1:10, ])
              expect_that(prox, is_a("matrix"))
              expect_that(dim(prox), equals(c(10, 10)))
              expect_that(unname(diag(prox)), equals(rep(1, 10)))
          })
