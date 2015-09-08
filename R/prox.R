#' Methods to extract proximity matrices from random forests
#'
#' Extracts proximity matrices from random forest objects from the party, randomForest or randomForestSRC packages
#'
#' @importFrom party proximity
#' @importFrom stats predict
#'
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param newdata new data with the same columns as the data used for \code{fit}
#' @param ... arguments to be passed to \code{extract_proximity}
#'
#' @return an n x n matrix where position i, j gives the proportion of times observation i and j are in the same teriminal node across all trees
#'
#' 
#' @export
extract_proximity <- function(fit, newdata) UseMethod("extract_proximity")
#' @export
extract_proximity.randomForest <- function(fit, newdata = NULL, ...) {
  if (!is.null(newdata)) {
    pred <- predict(fit, newdata = newdata, proximity = TRUE, ...)
    if (!is.null(pred$oob.prox))
      out <- pred$oob.prox
    else if (!is.null(pred$prox))
      out <- pred$prox
    else stop("not sure what is up") 
  } else {
    if (is.null(fit$proximity))
      stop("call randomForest with proximity or oob.prox = TRUE")
    fit$proximity
  }
}
#' @export
extract_proximity.RandomForest <- function(fit, newdata = NULL, ...) {
  proximity(fit, newdata, ...)
}
#' @export
extract_proximity.rfsrc <- function(fit, newdata = NULL, ...) {
  if (!is.null(newdata)) {
    pred <- predict(fit, newdata = newdata, proximity = TRUE, ...)
    out <- pred$prox
  } else {
    if (is.null(fit$proximity))
      stop("call rfsrc with proximity equal to TRUE, \"inbag\", \"oob\", or \"all\"")
    fit$proximity
  }
}
