library(randomForest)
data(swiss)
data(iris)

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

test_that("regression", {
    pd <- partial_dependence(fit, swiss, "Education", ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
})

test_that("regression and ci", {
    pd <- partial_dependence(fit, swiss, "Education", ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
})

test_that("regression, ci, and vector input", {
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

test_that("regression and vector input", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"),
                             interaction = FALSE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("regression and interaction", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"),
                             interaction = TRUE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
    expect_that(pd$Agriculture, is_a("integer"))
})

test_that("regression, interactions, and ci", {
    pd <- partial_dependence(fit, swiss, c("Education", "Agriculture"),
                             interaction = TRUE, ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(pd$Education, is_a("integer"))
    expect_that(pd$Agriculture, is_a("integer"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
})

## test classification
fit <- randomForest(Species ~ ., iris)

test_that("classification", {
    pd <- partial_dependence(fit, iris, "Petal.Width")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$Petal.Width, is_a("numeric"))
})

test_that("classification and vector input", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Species", "variable")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("classification and interaction", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
})

test_that("classification and probability output", {
    pd <- partial_dependence(fit, iris, "Petal.Width", type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
})

test_that("classification, probability output, and vector input", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"), type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "setosa", "versicolor", "virginica", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("classification, probability output, and interaction", {
    pd <- partial_dependence(fit, iris, c("Petal.Width", "Petal.Length"), type = "prob", interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})
