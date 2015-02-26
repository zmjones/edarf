library(testthat)
library(edarf)
library(randomForest)
library(party)
library(randomForestSRC)
data(swiss)
data(iris)
data(mtcars)
data(veteran)

## test functions
test_that("ivar_points works correctly", {
    df <- data.frame("x" = 1:20, "y" = rep(1, 20))
    expect_that(length(ivar_points(df, "x", 10)), equals(10))
    expect_that(length(unique(ivar_points(df, "x", 10))), equals(length(ivar_points(df, "x", 10))))
    expect_that(ivar_points(df, "x", nrow(df)), equals(df[, "x"]))
})

## test randomForest method
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

## test regression
fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)

test_that("partial_dependence works with randomForest regression", {
    pd <- partial_dependence(fit, swiss, "Education", ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
})

test_that("partial_dependence returns confidence interval for randomForest regression", {
    pd <- partial_dependence(fit, swiss, "Education", ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
})

test_that("partial_dependence works with a vector input with randomForest with ci", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"), interaction = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variance", "variable", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with a vector input with randomForest", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"),
                             interaction = FALSE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with a vector input with randomForest and interactions", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"),
                             interaction = TRUE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
    ## expect_that(pd$Agriculture, is_a("integer"))
})

test_that("partial_dependence works with a vector input with randomForest, interactions, and ci", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"),
                             interaction = TRUE, ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    ## expect_that(pd$Education, is_a("integer"))
    ## expect_that(pd$Agriculture, is_a("integer"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
})

## test classification
fit <- randomForest(Species ~ ., iris)

test_that("partial_dependence works with randomForest classification", {
    pd <- partial_dependence(fit, iris, "Petal.Width")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$Petal.Width, is_a("numeric"))
})

test_that("partial_dependence works with randomForest classification and vector input", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Species", "variable")))
    ## expect_that(pd$Species, is_a("factor"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with randomForest classification, and interaction", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
    ## expect_that(pd$Species, is_a("factor"))
    ## expect_that(pd$Petal.Width, is_a("numeric"))
    ## expect_that(pd$Petal.Length, is_a("numeric"))
})

test_that("partial_dependence works with randomForest classification w/ probability output", {
    pd <- partial_dependence(fit, iris, "Petal.Width", type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
})

test_that("partial_dependence works with randomForest classification w/ probability output and vector input", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"), type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with randomForest classification w/ probability output and interaction", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"), type = "prob", interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})

## test party method
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

## test regression
fit <- cforest(Fertility ~ ., swiss)

test_that("partial_dependence works with cforest regression", {
    pd <- partial_dependence(fit, "Education", ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
})

test_that("partial_dependence returns confidence interval for cforest regression", {
    pd <- partial_dependence(fit, "Education", ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
})

test_that("partial_dependence works with a vector input with cforest with ci", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"), interaction = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variance", "variable", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with a vector input with cforest", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = FALSE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with cforest and interactions", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = TRUE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
    ## expect_that(pd$Agriculture, is_a("integer"))
})

test_that("partial_dependence works with a vector input with cforest, interactions, and ci", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = TRUE, ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    ## expect_that(pd$Education, is_a("integer"))
    ## expect_that(pd$Agriculture, is_a("integer"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
})

## test classification
fit <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))

test_that("partial_dependence works with cforest classification", {
    pd <- partial_dependence(fit, "Petal.Width")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
    ## expect_that(pd$Species, is_a("factor"))
    ## expect_that(pd$Petal.Width, is_a("numeric"))
})

test_that("partial_dependence works with a vector input with cforest classification", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Species", "variable")))
    ## expect_that(pd$Species, is_a("factor"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with cforest classification and interaction", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
    ## expect_that(pd$Species, is_a("factor"))
    ## expect_that(pd$Petal.Width, is_a("numeric"))
    ## expect_that(pd$Petal.Length, is_a("numeric"))
})

test_that("partial_dependence works with cforest classification w/ probability output", {
    pd <- partial_dependence(fit, "Petal.Width", type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})

test_that("partial_dependence works with cforest classification, interaction, and probability output", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), interaction = TRUE, type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa", "versicolor", "virginica")))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
})

test_that("partial_dependence works with cforest classification w/ probability output and vector input", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

## test multivariate regression
fit <- cforest(hp + qsec ~ ., mtcars, controls = cforest_control(mtry = 2))

test_that("partial_dependence works with party multivariate regression", {
    pd <- partial_dependence(fit, "mpg")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("mpg", "hp", "qsec")))
    expect_that(pd$mpg, is_a("numeric"))
    ## expect_that(pd$hp, is_a("integer"))
    expect_that(pd$qsec, is_a("numeric"))
})

test_that("partial_dependence works with party multivariate regression and vector input", {
    pd <- partial_dependence(fit, c("mpg", "cyl"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "hp", "qsec", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$hp, is_a("numeric"))
    expect_that(pd$qsec, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with party multivariate regression and interaction", {
    pd <- partial_dependence(fit, c("mpg", "cyl"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("mpg", "cyl", "hp", "qsec")))
    expect_that(pd$mpg, is_a("numeric"))
    ## expect_that(pd$cyl, is_a("integer"))
    expect_that(pd$hp, is_a("numeric"))
    expect_that(pd$qsec, is_a("numeric"))
})

## test randomForestSRC method
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

test_that("partial_dependence works with rfsrc regression", {
    pd <- partial_dependence(fit, "Education", ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
})

test_that("partial_dependence returns confidence interval for rfsrc regression", {
    pd <- partial_dependence(fit, "Education", ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
})

test_that("partial_dependence works with a vector input with rfsrc with ci", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"), interaction = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variance", "variable", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with a vector input with rfsrc", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = FALSE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with rfsrc and interactions", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = TRUE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    ## expect_that(pd$Education, is_a("integer"))
    ## expect_that(pd$Agriculture, is_a("integer"))
})

test_that("partial_dependence works with rfsrc, interactions, and ci", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = TRUE, ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    ## expect_that(pd$Education, is_a("integer"))
    ## expect_that(pd$Agriculture, is_a("integer"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
})

## test classification
fit <- rfsrc(Species ~ ., iris)

test_that("partial_dependence works with rfsrc classification", {
    pd <- partial_dependence(fit, "Petal.Width")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
    expect_that(pd$Species, is_a("factor"))
})

test_that("partial_dependence works with rfsrc classification and vector input", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Species", "variable")))
    expect_that(pd$value, is_a("numeric"))
    ## expect_that(pd$Species, is_a("factor"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with rfsrc classification and interaction", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
    ## expect_that(pd$Species, is_a("factor"))
})

test_that("partial_dependence works with rfsrc classification with prob output", {
    pd <- partial_dependence(fit, "Petal.Width", type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})

test_that("partial_dependence works with rfsrc classification and vector input and prob output", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with rfsrc classification, interaction, and prob output", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), interaction = TRUE, type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})

## test survival
fit <- rfsrc(Surv(time, status) ~ ., veteran)

test_that("partial_dependence works with rfsrc survival", {
    pd <- partial_dependence(fit, "age")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("age", "chf")))
    ## expect_that(pd$age, is_a("integer"))
    expect_that(pd$chf, is_a("numeric"))
})

test_that("partial_dependence works with rfsrc survival and vector input", {
    pd <- partial_dependence(fit, c("age", "diagtime"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "chf", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$chf, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("partial_dependence works with rfsrc survival and interaction", {
    pd <- partial_dependence(fit, c("age", "diagtime"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("age", "diagtime", "chf")))
    ## expect_that(pd$age, is_a("integer"))
    expect_that(pd$diagtime, is_a("numeric"))
    expect_that(pd$chf, is_a("numeric"))
})


