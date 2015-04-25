library(testthat)
library(edarf)

set.seed(1987)

n <- 50
k <- 2
X <- replicate(k, rnorm(n))
beta <- rep(1, k)
y <- as.numeric(X %*% beta + rnorm(n, .5))
df <- data.frame(X, y)
write.csv(df, "testthat/df_regr.csv")
df$y <- as.factor(ifelse(y > 0, 1, 0))
write.csv(df, "testthat/df_classif.csv")

test_check("edarf")
