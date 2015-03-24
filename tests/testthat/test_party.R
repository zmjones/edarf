library(party)
data("swiss")
data("iris")
data("mtcars")
data("readingSkills", package = "party")

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

test_that("regression", {
    pd <- partial_dependence(fit, var = "Education", ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
})

test_that("confidence interval for regression", {
    pd <- partial_dependence(fit, var = "Education", ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Fertility", "variance", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
})

test_that("regression, vector input, and ci", {
    pd <- partial_dependence(fit, var = c("Education", "Agriculture"), interaction = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variance", "variable", "low", "high")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("regression with vector input", {
    pd <- partial_dependence(fit, var = c("Education", "Agriculture"),
                             interaction = FALSE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Fertility", "variable")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("regression with interactions", {
    pd <- partial_dependence(fit, var = c("Education", "Agriculture"),
                             interaction = TRUE, ci = FALSE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Education", "Agriculture", "Fertility")))
    expect_that(pd$Fertility, is_a("numeric"))
    expect_that(pd$Education, is_a("integer"))
    expect_that(pd$Agriculture, is_a("numeric"))
})

test_that("regression with interactions and ci", {
    pd <- partial_dependence(fit, var = c("Education", "Agriculture"),
                             interaction = TRUE, ci = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(pd$Education, is_a("integer"))
    expect_that(pd$Agriculture, is_a("numeric"))
    expect_that(pd$variance, is_a("numeric"))
    expect_that(pd$low, is_a("numeric"))
    expect_that(pd$high, is_a("numeric"))
})

## test classification
fit <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))

test_that("classification", {
    pd <- partial_dependence(fit, var = "Petal.Width")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Species")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$Petal.Width, is_a("numeric"))
})

test_that("classification with vector input", {
    pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "Species", "variable")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("classification and interaction", {
    pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "Species")))
    expect_that(pd$Species, is_a("factor"))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
})

test_that("classification and probability output", {
    pd <- partial_dependence(fit, var = "Petal.Width", type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "setosa", "versicolor", "virginica")))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
})

test_that("classification, interaction, and probability output", {
    pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"), interaction = TRUE, type = "prob")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("Petal.Width", "Petal.Length", "setosa", "versicolor", "virginica")))
    expect_that(pd$setosa, is_a("numeric"))
    expect_that(pd$versicolor, is_a("numeric"))
    expect_that(pd$virginica, is_a("numeric"))
    expect_that(pd$Petal.Width, is_a("numeric"))
    expect_that(pd$Petal.Length, is_a("numeric"))
})

test_that("classification, probability output, and vector input", {
    pd <- partial_dependence(fit, var = c("Petal.Width", "Petal.Length"), type = "prob")
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

test_that("multivariate regression", {
    pd <- partial_dependence(fit, var = "mpg")
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("mpg", "hp", "qsec")))
    expect_that(pd$mpg, is_a("numeric"))
    expect_that(pd$hp, is_a("numeric"))
    expect_that(pd$qsec, is_a("numeric"))
})

test_that("multivariate regression and vector input", {
    pd <- partial_dependence(fit, var = c("mpg", "cyl"))
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("value", "hp", "qsec", "variable")))
    expect_that(pd$value, is_a("numeric"))
    expect_that(pd$hp, is_a("numeric"))
    expect_that(pd$qsec, is_a("numeric"))
    expect_that(pd$variable, is_a("character"))
})

test_that("multivariate regression and interaction", {
    pd <- partial_dependence(fit, var = c("mpg", "cyl"), interaction = TRUE)
    expect_that(pd, is_a("data.frame"))
    expect_that(colnames(pd), equals(c("mpg", "cyl", "hp", "qsec")))
    expect_that(pd$mpg, is_a("numeric"))
    expect_that(pd$cyl, is_a("numeric"))
    expect_that(pd$hp, is_a("numeric"))
    expect_that(pd$qsec, is_a("numeric"))
})

## marginal accuracy
## conditional  accuracy
## marginal auc
## conditional auc

fit <- cforest(score ~ ., data = readingSkills, control = cforest_unbiased(mtry = 2, ntree = 50))

test_that("extractor works with marginal accuracy", {
    imp <- variable_importance(fit)
    expect_that(imp, is_a("data.frame"))
    expect_that(nrow(imp), equals(3))
})

test_that("extractor works with conditional accuracy", {
    imp <- variable_importance(fit, type = "conditional")
    expect_that(imp, is_a("data.frame"))
    expect_that(nrow(imp), equals(3))
})

readingSkills$score <- ifelse(readingSkills$score > mean(readingSkills$score), 1, 0)
fit <- cforest(score ~ ., data = readingSkills, control = cforest_unbiased(mtry = 2, ntree = 50))

test_that("extractor works with marginal auc", {
    imp <- variable_importance(fit, type = "auc")
    expect_that(imp, is_a("data.frame"))
    expect_that(nrow(imp), equals(3))
})

test_that("extractor works with conditional auc", {
    imp <- variable_importance(fit, type = c("auc", "conditional"))
    expect_that(imp, is_a("data.frame"))
    expect_that(nrow(imp), equals(3))
})

## proximity
## proximity newdata

test_that("extractor works with proximity", {
    prox <- extract_proximity(fit)
    expect_that(prox, is_a("matrix"))
    expect_that(dim(prox), equals(c(200, 200)))
    expect_that(unname(diag(prox)), equals(rep(1, 200)))
})

test_that("extractor works with proximity and newdata", {
    prox <- extract_proximity(fit, readingSkills[1:10, ])
    expect_that(prox, is_a("matrix"))
    expect_that(dim(prox), equals(c(10, 10)))
    expect_that(unname(diag(prox)), equals(rep(1, 10)))
})
