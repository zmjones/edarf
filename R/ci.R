#' Variance estimates for predicted values from random forests
#'
#' Calculates the variance of predictions from a random forest using the bias corrected infinitesimal
#' jackknife from Wager, Efron, and Tibsharani (2014) using a fitted random forest object from the
#' party, randomForest, or randomForestSRC packages
#'
#' @references Wager, Hastie, and Efron, "Confidence intervals for random forests: the jackknife and the infinitesimal jackknife," \emph{Journal of Machine Learning Research}, 2014.
#' @references https://github.com/swager/randomForestCI
#'
#' @importFrom stats predict
#' 
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc' (must be regression)
#' @param data dataframe to be used for prediction
#'
#' @return a data.frame of length n with one column 'prediction' which contains the ensemble prediction
#' and a column 'variance' which contains the estimated variance
#'
#' @export
var_est <- function(fit, data) UseMethod("var_est", fit)
#' @export
var_est.randomForest <- function(fit, data, ...) {
  info <- installed.packages(fields = c("Package", "Version"))
  info <- info[, c("Package", "Version")]
  if (!info[info[, 1] == "randomForest", "Version"] == "4.6-11")
    stop("install fixed randomForest from http://github.com/swager/randomForest")
  if (is.null(fit$inbag))
    stop("keep.inbag must be true in call to randomForest")
  pred <- predict(fit, newdata = data, predict.all = TRUE, ...)
  data.frame("prediction" = pred$aggregate,
             "variance" = inf_jackknife(nrow(data), fit$ntree, pred$individual, fit$inbag))
  
}
#' @export
var_est.RandomForest <- function(fit, data) {
  check <- !("subset" %in% names(as.list(args(fit@predict_response))))
  if (check)
    stop("Install party from http://github.com/zmjones/party/pkg.")
  pred <- sapply(1:length(fit@ensemble), function(idx) predict(fit, newdata = data, subset = idx))
  data.frame("prediction" = predict(fit, newdata = data),
             "variance" = inf_jackknife(nrow(data), length(fit@ensemble), pred,
                                        as.matrix(do.call(cbind, fit@weights), sparse = TRUE)))
}
#' @export
var_est.rfsrc <- function(fit, data, ...) {
  pred <- predict(fit, newdata = data, outcome = "train", ...)
  tree_pred <- get_tree_pred(fit$n, fit$ntree, pred$membership, fit$yvar, fit$inbag)
  data.frame("prediction" = pred$predicted, "variance" = inf_jackknife(fit$n, fit$ntree, tree_pred, fit$inbag))
}
