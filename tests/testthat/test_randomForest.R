library(randomForest)
data("swiss")
data("iris")

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
## classification, prob, drop
## classification, vector var, prob
## classification, interaction, prob

## test regression
fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)

test_that("regression", {
              pd <- partial_dependence(fit, df = swiss, var = "Education", ci = FALSE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Education", "Fertility")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$Education, is_a("integer"))
          })

test_that("regression and ci", {
              pd <- partial_dependence(fit, df = swiss, var = "Education", ci = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$variance, is_a("numeric"))
              expect_that(pd$low, is_a("numeric"))
              expect_that(pd$high, is_a("numeric"))
              expect_that(pd$Education, is_a("integer"))
          })

test_that("regression, ci, and vector input", {
              pd <- partial_dependence(fit, df = swiss, var = c("Education", "Agriculture"),
                                       ci = TRUE, interaction = FALSE)
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
              pd <- partial_dependence(fit, df = swiss, var = c("Education", "Agriculture"),
                                       interaction = FALSE, ci = FALSE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("regression and interaction", {
              pd <- partial_dependence(fit, df = swiss, var = c("Education", "Agriculture"),
                                       interaction = TRUE, ci = FALSE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
              expect_that(pd$Fertility, is_a("numeric"))
              expect_that(pd$Education, is_a("integer"))
              expect_that(pd$Agriculture, is_a("numeric"))
          })

test_that("regression, interactions, and ci", {
              pd <- partial_dependence(fit, df = swiss, var = c("Education", "Agriculture"),
                                       interaction = TRUE, ci = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(pd$Education, is_a("integer"))
              expect_that(pd$Agriculture, is_a("numeric"))
              expect_that(pd$variance, is_a("numeric"))
              expect_that(pd$low, is_a("numeric"))
              expect_that(pd$high, is_a("numeric"))
          })

## test classification
fit <- randomForest(Species ~ ., iris)

test_that("classification", {
              pd <- partial_dependence(fit, df = iris, var = "Petal.Width")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
              expect_that(pd$Species, is_a("factor"))
              expect_that(pd$Petal.Width, is_a("numeric"))
          })

test_that("classification and vector input", {
              pd <- partial_dependence(fit, df = iris, var = c("Petal.Width", "Petal.Length"))
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "Species", "variable")))
              expect_that(pd$Species, is_a("factor"))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("classification and interaction", {
              pd <- partial_dependence(fit, df = iris, var = c("Petal.Width", "Petal.Length"), interaction = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
              expect_that(pd$Species, is_a("factor"))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$Petal.Length, is_a("numeric"))
          })

test_that("classification and probability output", {
              pd <- partial_dependence(fit, df = iris, var = "Petal.Width", type = "prob")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$setosa, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
          })

test_that("classification with probability output and drop_levels", {
              pd <- partial_dependence(fit, df = iris, var = "Petal.Width", type = "prob", drop_levels = "setosa")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "versicolor", "virginica")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
          })

test_that("classification, probability output, and vector input", {
              pd <- partial_dependence(fit, df = iris, var = c("Petal.Width", "Petal.Length"), type = "prob")
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
              expect_that(pd$value, is_a("numeric"))
              expect_that(pd$setosa, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
              expect_that(pd$variable, is_a("character"))
          })

test_that("classification, probability output, and interaction", {
              pd <- partial_dependence(fit, df = iris, var = c("Petal.Width", "Petal.Length"),
                                       type = "prob", interaction = TRUE)
              expect_that(pd, is_a("data.frame"))
              expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa",
                                                 "versicolor", "virginica")))
              expect_that(pd$Petal.Width, is_a("numeric"))
              expect_that(pd$Petal.Length, is_a("numeric"))
              expect_that(pd$setosa, is_a("numeric"))
              expect_that(pd$versicolor, is_a("numeric"))
              expect_that(pd$virginica, is_a("numeric"))
          })

## accuracy
## gini
## class
## local

fit <- randomForest(Species ~ ., iris, importance = TRUE)

test_that("extractor works with marginal accuracy", {
              imp <- variable_importance(fit, type = "accuracy")
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
          })

test_that("extractor works with marginal gini", {
              imp <- variable_importance(fit, type = "gini")
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
          })

test_that("extractor works with marginal class importance", {
              imp <- variable_importance(fit, type = "accuracy", class_levels = TRUE)
              expect_that(imp, is_a("data.frame"))
              expect_that(nrow(imp), equals(4))
              expect_that(ncol(imp), equals(4))
              expect_that(colnames(imp), equals(c("setosa", "versicolor", "virginica", "labels")))
          })

fit <- randomForest(Species ~ ., iris, localImp = TRUE)

test_that("extractor works with local importance", {
              imp <- variable_importance(fit, type = "local")
              expect_that(imp, is_a("data.frame"))
              expect_that(dim(imp), equals(c(150, 4)))
              expect_that(colnames(imp), equals(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")))
          })

## proximity
## proximity newdata

fit <- randomForest(Species ~ ., iris, proximity = TRUE)

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
