#' Methods to extract proximity matrices from random forests
#'
#' Extracts proximity matrices from random forest objects from the party, randomForest or randomForestSRC packages
#'
#' @importFrom stats predict
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param newdata new data with the same columns as the data used for \code{fit}
#' @param ... arguments to be passed to \code{extract_proximity}
#'
#' @rdname extract_proximity
#' @export
extract_proximity <- function(fit, newdata) UseMethod("extract_proximity")
#' Extract proximity matrix from \code{randomForest} objects
#'
#' @param fit object of class randomForest called with proximity or oob.prox = TRUE
#' @param newdata new data with the same columns as the data used for \code{fit}
#' @param ... additional arguments to pass to predict.randomForest
#'
#' @return an n by n matrix
#'
#' @rdname extract_proximity
#' @method extract_proximity randomForest
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
#' Extract proximity matrix from objects returned from \code{cforest}
#'
#' @importFrom party proximity
#' @param fit object of class 'RandomForest' from \code{cforest}
#' @param newdata a data.frame with the same columns as the training data
#' @param ... additional arguments to pass to proximity
#'
#' @return an n by n matrix
#'
#' @rdname extract_proximity
#' @method extract_proximity RandomForest
#' @export
extract_proximity.RandomForest <- function(fit, newdata = NULL, ...) {
    proximity(fit, newdata, ...)
}
#' Extract proximity matrix from objects returned from \code{rfsrc}
#'
#' @param fit object of class 'rfsrc' from \code{rfsrc}
#' @param newdata new data with the same columns as the data used for \code{fit}
#' @param ... additional arguments to pass to predict.rfsrc
#'
#' @return an n by n matirx
#'
#' @rdname extract_proximity
#' @method extract_proximity rfsrc
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
