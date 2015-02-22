var_est <- function(fit, ...) UseMethod("var_est", fit)

var_est.randomForest <- function(fit, df) {
    info <- installed.packages(fields = c("Package", "Version"))
    info <- info[, c("Package", "Version")]
    if (!"randomForestCI" %in% info)
        stop("install randomForestCI from http://github.com/swager/randomForestCI")
    if (!info[info[, 1] == "randomForest", "Version"] == "4.6-11")
        stop("install fixed randomForest from http://github.com/swager/randomForest")
    out <- randomForestCI::randomForestInfJack(fit, df)
    colnames(out) <- c("prediction", "variance")
    out
}

var_est.RandomForest <- function(fit, df) {
    new_df <- party:::newinputs(fit, df)
    pred <- sapply(1:length(fit@ensemble), function(i) {
        sapply(.Call("R_predictRF_weights",
                     fit@ensemble[i], fit@where[i], fit@weights[i], new_df, 0, FALSE, PACKAGE = "party"),
               function(w) w %*% fit@responses@predict_trafo / sum(w))
    })
    data.frame("predicton" = predict(fit, newdata = df),
               "variance" = inf_jackknife(pred, length(fit@ensemble),
                   Matrix::Matrix(do.call(cbind, fit@weights), sparse = TRUE)))
}

var_est.rfsrc <- function(fit, df) {
    pred <- matrix(NA, fit$n, fit$ntree)
    for (i in 1:fit$n) {
        for (j in 1:fit$ntree) {
            idx <- which(fit$membership[, j] == fit$membership[i, j])
            pred[i, j] <- weighted.mean(fit$yvar[idx], fit$inbag[idx, j])
        }
    }
    data.frame("prediction" = rowMeans(pred),
               "variance" = inf_jackknife(pred, fit$ntree, fit$inbag))
}

inf_jackknife <- function(pred, B, N) {
    pred_center <- pred - Matrix::rowMeans(pred)  ## difference between tree prediction
    ## and mean across trees
    N_avg <- Matrix::rowMeans(N) ## proportion of times i appears in B (all b)
    n <- sum(N) / B ## portion of obs. sampled at each b, same as sum(N_avg), equals no. obs. w/ bootstrap,
    ## and is < no. obs. w/ subsampling
    ## covariance between number of times obs. i appears in b and difference between tree
    ## and mean across trees (across in bag and out bag)
    C <- N %*% t(pred_center) - Matrix::Matrix(N_avg, nrow(N), 1) %*%
        Matrix::Matrix(rowSums(pred_center), 1, nrow(pred_center))
    raw_IJ <- Matrix::colSums(C^2) / B^2
    N_var <- mean(Matrix::rowMeans(N^2) - N_avg^2)
    boot_var <- Matrix::rowMeans(pred_center^2)
    bias_correct <- n * N_var * boot_var / B
    raw_IJ - bias_correct
}
