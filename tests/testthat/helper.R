set.seed(1987)

library(randomForest)
library(party)
library(randomForestSRC)

cutoff <- 3L
nperm <- 2L
n <- 50

X1 <- rnorm(n)
X2 <- rnorm(n)

df_regr <- data.frame(X1, X2, "y" = rowSums(poly(X1)) * X2)
df_classif <- data.frame(X1, X2, "y" = as.factor(ifelse(df_regr$y / var(df_regr$y) > 1, 1, 0)))

fits_regr <- list(
  randomForest(y ~ ., df_regr, keep.inbag = TRUE),
  randomForest(df_regr[, -which(colnames(df_regr) == "y")], df_regr$y, keep.inbag = TRUE),
  cforest(y ~ ., df_regr, controls = cforest_control(mtry = 1)),
  rfsrc(y ~ ., df_regr)
)

fits_classif <- list(
  randomForest(y ~ ., df_classif, keep.inbag = TRUE),
  randomForest(df_classif[, -which(colnames(df_classif) == "y")], df_classif$y, keep.inbag = TRUE),
  cforest(y ~ ., df_classif, controls = cforest_control(mtry = 1)),
  rfsrc(y ~ ., df_classif)
)
