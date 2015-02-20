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

var_est.RandomForest <- function(fit, df, parallel = FALSE) {
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    pred <- foreach::foreach(i = 1:length(fit@ensemble), .inorder = FALSE,
                             .combine = "cbind", .packages = "party") %op% {
        pw <- .Call("R_predictRF_weights",
                    fit@ensemble[i], fit@where[i], fit@weights[i], party:::newinputs(fit, df),
                    0, FALSE, PACKAGE = "party")
        sapply(pw, function(w) w %*% fit@responses@predict_trafo / sum(w))
    }
    pred_center <- pred - Matrix::rowMeans(pred)  ## difference between tree prediction
    ## and mean across trees
    N <- Matrix::Matrix(do.call(cbind, fit@weights), sparse = TRUE) ## matrix where i,j is count of obs. i in b
    N_avg <- Matrix::rowMeans(N) ## proportion of times i appears in B (all b)
    B <- length(fit@ensemble) ## number of boostrap replicates
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
    vars <- raw_IJ - bias_correct
    data.frame("prediction" = predict(fit, newdata = df), "variance" = vars)
}

var_est.rfsrc <- function(fit, df) {
}
