#' Variance estimates for predicted values from random forests
#'
#' Calculates the variance of predictions from a random forest using the bias corrected infinitesimal
#' jackknife from Wager, Efron, and Tibsharani (2014) using a fitted random forest object from the
#' party, randomForest, or randomForestSRC packages
#'
#' @importFrom Matrix Matrix rowSums rowMeans colSums
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc' (must be regression)
#' @param df dataframe to be used for prediction
#'
#' @export
var_est <- function(fit, df) UseMethod("var_est", fit)
#' Variance estimation for randomForest objects from package \code{randomForest}
#'
#' Calculates the variance of predictions from regression using randomForest by calling randomForestCI (\url{https://github.com/swager/randomForestCI})
#'
#' @param fit an object of class 'randomForest' returned from \code{randomForest} with \code{keep.inbag = TRUE}
#' @param df dataframe to be used for prediction
#' @param ... additional arguments to be passed to predict.randomForest
#'
#' @return a dataframe with two columns: 'prediction' and 'variance', where the former is the prediction calculated using the inbag data, and the variance is calculated using the bias corrected infinitesimal bootstrap from Wager, Efron, and Tibsharani (2014).
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' data(swiss)
#'
#' fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)
#' var_est(fit, swiss)
#' }
#' @export
var_est.randomForest <- function(fit, df, ...) {
    info <- installed.packages(fields = c("Package", "Version"))
    info <- info[, c("Package", "Version")]
    if (!"randomForestCI" %in% info)
        stop("install randomForestCI from http://github.com/swager/randomForestCI")
    if (!info[info[, 1] == "randomForest", "Version"] == "4.6-11")
        stop("install fixed randomForest from http://github.com/swager/randomForest")
    pred <- predict(fit, newdata = df, predict.all = TRUE, ...)
    data.frame("prediction" = pred$aggregate,
               "variance" = inf_jackknife(pred$individual, fit$ntree, fit$inbag))
    
}
#' Variance estimation for RandomForest objects from package \code{party}
#'
#' Calculates the variance of predictions from regression using RandomForest using a slightly modified version of the code from randomForestCI (\url{https://github.com/swager/randomForestCI})
#'
#' @import party
#' @param fit an object of class 'RandomForest' returned from \code{cforest}
#' @param df dataframe to be used for prediction
#'
#' @return a dataframe with two columns: 'prediction' and 'variance', where the former is the prediction calculated using the inbag data, and the variance is calculated using the bias corrected infinitesimal bootstrap from Wager, Efron, and Tibsharani (2014).
#'
#' @examples
#' \dontrun{
#' library(party)
#' data(swiss)
#'
#' fit <- cforest(Fertility ~ ., swiss, controls = cforest_control(mtry = 2))
#' var_est(fit, swiss)
#' }
#' @export
var_est.RandomForest <- function(fit, df) {
    new_df <- initVariableFrame(df)
    pred <- sapply(1:length(fit@ensemble), function(i) {
        sapply(.Call("R_predictRF_weights",
                     fit@ensemble[i], fit@where[i], fit@weights[i], new_df, 0, FALSE, PACKAGE = "party"),
               function(w) w %*% fit@responses@predict_trafo / sum(w))
    })
    data.frame("prediction" = predict(fit, newdata = df),
               "variance" = inf_jackknife(pred, length(fit@ensemble),
                   Matrix(do.call(cbind, fit@weights), sparse = TRUE)))
}
#' Variance estimation for rfsrc objects from package \code{randomForestSRC}
#'
#' Calculates the variance of predictions from regression using RandomForest using a slightly modified version of the code from randomForestCI (\url{https://github.com/swager/randomForestCI})
#'
#' @param fit an predict object of class 'rfsrc' returned from \code{rfsrc}
#' @param df dataframe to be used for prediction
#' @param ... additional arguments to be passed to predict.rfsrc
#'
#' @return a dataframe with two columns: 'prediction' and 'variance', where the former is the prediction calculated using the inbag data, and the variance is calculated using the bias corrected infinitesimal bootstrap from Wager, Efron, and Tibsharani (2014).
#'
#' @examples
#' \dontrun{
#' library(randomForestSRC)
#' data(swiss)
#'
#' fit <- rfsrc(Fertility ~ ., swiss)
#' var_est(fit, swiss)
#' }
#' @export
var_est.rfsrc <- function(fit, df, ...) {
  if (is.null(fit$pd_membership) | is.null(fit$pd_predicted)) {
    pred <- predict(fit, newdata = df, outcome = "train", ...)
    fit$pd_membership <- pred$membership
    fit$pd_predicted <- pred$predicted
  }
  out <- vestl(fit$n, fit$ntree, fit$pd_membership, fit$yvar, fit$inbag)
  data.frame("prediction" = fit$pd_predicted,
             "variance" = inf_jackknife(out, fit$ntree, fit$inbag))
}
#' Bias corrected infinitesimal jackknife variance estimator for predictions given a matrix of tree predictions
#'
#' Essentially code from randomForestCI (\url{https://github.com/swager/randomForestCI}) with some modifications, and factored out so that it can be made more generic
#'
#' @param pred matrix with n rows and B columns, where n is the number of observations, and B the number of trees in the forest
#' @param B number of trees
#' @param N matrix with n rows and B columns, where each entry gives the number of times observation i appears in tree j
#'
#' @return a vector of variance estimates for each observation
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' data(swiss)
#'
#' fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)
#' pred <- predict(fit, newdata = swiss, predict.all = TRUE)
#' inf_jackknife(pred$individual, fit$ntree, fit$inbag)
#' }
#' @export
inf_jackknife <- function(pred, B, N) {
    pred_center <- pred - rowMeans(pred)  ## difference between tree prediction
    ## and mean across trees
    N_avg <- rowMeans(N) ## proportion of times i appears in B (all b)
    n <- sum(N) / B ## portion of obs. sampled at each b, same as sum(N_avg), equals no. obs. w/ bootstrap,
    ## and is < no. obs. w/ subsampling
    ## covariance between number of times obs. i appears in b and difference between tree
    ## and mean across trees (across in bag and out bag)
    C <- N %*% t(pred_center) - Matrix(N_avg, nrow(N), 1) %*%
        Matrix(rowSums(pred_center), 1, nrow(pred_center))
    raw_IJ <- colSums(C^2) / B^2
    N_var <- mean(rowMeans(N^2) - N_avg^2)
    boot_var <- rowMeans(pred_center^2)
    bias_correct <- n * N_var * boot_var / B
    raw_IJ - bias_correct
}
