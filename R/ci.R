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
    pred_center <- pred - Matrix::rowMeans(pred)  ## mean deviated tree pred
    N <- Matrix::Matrix(do.call(cbind, fit@weights)) ## matrix were i,j is obs i in bs_j
    N_avg <- Matrix::rowMeans(N) ## proportion of times i is in b_i
    B <- length(fit@ensemble) ## boostrap replicates
    n <- length(get("response", fit@data@env)[, 1])
    s <- sum(N) / B ## number of obs. sampled at each b_i
    ## bs weight - proportion i selected over B * sum of mean deviated predictions
    C <- N %*% t(pred_center) - Matrix::Matrix(N_avg, nrow(N), 1) %*%
        Matrix::Matrix(rowSums(pred_center), 1, nrow(pred_center))
    raw_IJ <- Matrix::colSums(C^2) / B^2
    N_var <- mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
    boot_var <- rowSums(pred_center^2) / B
    bias_correct <- n * N_var * boot_var / B
    vars <- raw_IJ - bias_correct
    data.frame("prediction" = predict(fit, newdata = df), "variance" = vars)
}

var_est.rfsrc <- function(fit, df) {
}
