library(randomForestSRC)
library(Rcpp)
library(microbenchmark)
data(swiss)

fit <- rfsrc(Fertility ~ ., swiss)

var_est.rfsrc_C <- function(fit, df) {
  if (is.null(fit$pd_membership) | is.null(fit$pd_predicted)) {
    pred <- predict(fit, newdata = df, outcome = "train")
    fit$pd_membership <- pred$membership
    fit$pd_predicted <- pred$predicted
  }
  out <- vestl(fit$n, fit$ntree, fit$pd_membership, fit$yvar, fit$inbag)
  data.frame("prediction" = fit$pd_predicted,
             "variance" = inf_jackknife(out, fit$ntree, fit$inbag))
}

outR <- matrix(NA, fit$n, fit$ntree)
for (i in 1:fit$n) {
  for (j in 1:fit$ntree) {
    idx <- which(fit$pd_membership[, j] == fit$pd_membership[i, j])
    outR[i, j] <- weighted.mean(fit$yvar[idx], fit$inbag[idx, j])
  }
}

outC <- vestl(fit$n, fit$ntree, fit$pd_membership, fit$yvar, fit$inbag)

outR[1:3, 1:3]
outC[1:3, 1:3]



microbenchmark(
  outC <- var_est.rfsrc_C(fit, swiss),
  outR <- var_est(fit,swiss),
  times = 2
  )

head(outC)
head(outR)
