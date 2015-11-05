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
#' @param calibrate whether to calibrate the variance estimates using empirical bayes via randomForestCI, default is TRUE
#' @param ... additional arguments to be passed to the predict method
#'
#' @return a data.frame of length n with one column 'prediction' which contains the ensemble prediction
#' and a column 'variance' which contains the estimated variance
#'
#' @export
var_est <- function(fit, data, ...) UseMethod("var_est", fit)
#' @export
var_est.randomForest <- function(fit, data, calibrate = TRUE, ...) {
  pred <- predict(fit, newdata = data, predict.all = TRUE, ...)$individual
  out <- randomForestCI::infJack(pred, fit$inbag, calibrate)
  colnames(out) <- c("prediction", "variance")
  out
}
#' @export
var_est.RandomForest <- function(fit, data, calibrate = TRUE, ...) {
  check <- !("subset" %in% names(as.list(args(fit@predict_response))))
  if (check)
    stop("Install party from http://github.com/zmjones/party/pkg.")
  pred <- sapply(1:length(fit@ensemble), function(idx) predict(fit, newdata = data, subset = idx, ...))
  out <- randomForestCI::infJack(pred, as.matrix(do.call(cbind, fit@weights), sparse = TRUE), calibrate)
  out <- as.data.frame(out)
  colnames(out) <- c("prediction", "variance")
  out
}
#' @export
var_est.rfsrc <- function(fit, data, calibrate = TRUE, ...) {
  args <- list(...)
  if (is.null(args$outcome))
    args$outcome <- "train"
  pred <- do.call(predict, c(args, list(object = fit, newdata = data)))
  tree_pred <- get_tree_pred(pred$n, pred$ntree, pred$membership, pred$yvar, pred$inbag)
  out <- randomForestCI::infJack(tree_pred, fit$inbag, calibrate)
  out <- as.data.frame(out)
  colnames(out) <- c("prediction", "variance")
  out
}
