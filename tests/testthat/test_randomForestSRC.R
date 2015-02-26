library(randomForestSRC)
data(swiss)
data(iris)
data(veteran)

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
    pd <- partial_dependence(fit, "Education", ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
})

test_that("regression and ci", {
    pd <- partial_dependence(fit, "Education", ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
})

test_that("regression, ci, and vector input", {
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

test_that("regression and vector input", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = FALSE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("regression and interaction", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = TRUE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
    expect_that(pd$Agriculture, is_a("integer"))
})

test_that("regression, interactions, and ci", {
    pd <- partial_dependence(fit, c("Education", "Agriculture"),
                             interaction = TRUE, ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(pd$Education, is_a("integer"))
    expect_that(pd$Agriculture, is_a("integer"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
})

## test classification
fit <- rfsrc(Species ~ ., iris)

test_that("classification", {
    pd <- partial_dependence(fit, "Petal.Width")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
    expect_that(pd$Species, is_a("factor"))
})

test_that("classification and vector input", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Species", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$variable, is_a("character"))
})

test_that("classification and interaction", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
    expect_that(pd$Species, is_a("factor"))
})

test_that("classification and prob output", {
    pd <- partial_dependence(fit, "Petal.Width", type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})

test_that("classification, vector input, and prob output", {
    pd <- partial_dependence(fit, c("Petal.Width", "Petal.Length"), type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("classification, interaction, and prob output", {
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

test_that("survival", {
    pd <- partial_dependence(fit, "age")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("age", "chf")))
    expect_that(pd$age, is_a("integer"))
    expect_that(pd$chf, is_a("numeric"))
})

test_that("survival and vector input", {
    pd <- partial_dependence(fit, c("age", "diagtime"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "chf", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$chf, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("survival and interaction", {
    pd <- partial_dependence(fit, c("age", "diagtime"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("age", "diagtime", "chf")))
    expect_that(pd$age, is_a("integer"))
    expect_that(pd$diagtime, is_a("numeric"))
    expect_that(pd$chf, is_a("numeric"))
})
