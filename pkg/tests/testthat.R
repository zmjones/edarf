library(testthat)
library(edarf)

set.seed(1987)

nperm <- 2L
n <- 50

X1 <- rnorm(n)
X2 <- rnorm(n)
X3 <- as.ordered(sample(1:3, n, TRUE))
b3 <- runif(3, -1, 1)

df_regr <- data.frame(X1, X2, X3, "y" = rowSums(poly(X1)) * X2 + model.matrix( ~ -1 + as.factor(X3)) %*% b3)
df_classif <- data.frame(X1, X2, X3, "y" = as.factor(ifelse(df_regr$y > median(df_regr$y), 1, 0)))
df_multi <- data.frame("yr" = df_regr$y, "yc" = df_classif$y, X1, X2, X3)

library(randomForest)
library(randomForestSRC)
library(party)
library(ranger)

fits_regr <- list(
  randomForest(y ~ ., df_regr, proximity = TRUE),
  randomForest(df_regr[, -which(colnames(df_regr) == "y")], df_regr$y, proximity = TRUE),
  cforest(y ~ ., df_regr, controls = cforest_control(mtry = 1)),
  rfsrc(y ~ ., df_regr, proximity = "inbag"),
  ranger(y ~ ., df_regr)
)

fits_classif <- list(
  randomForest(y ~ ., df_classif, proximity = TRUE),
  randomForest(df_classif[, -which(colnames(df_classif) == "y")], df_classif$y, proximity = TRUE),
  cforest(y ~ ., df_classif, controls = cforest_control(mtry = 1)),
  rfsrc(y ~ ., df_classif, proximity = "inbag"),
  ranger(y ~ ., df_classif)
)

fits_multi <- list(
  cforest(yr + yc ~ ., df_multi, controls = cforest_control(mtry = 1))
)

test_check("edarf")
