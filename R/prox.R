#' Methods to extract proximity matrices from random forests
#'
#' Extracts proximity matrices from random forest objects from the party, randomForest or randomForestSRC packages
#'
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param ... arguments to be passed to \code{extract_proximity}
#'
#' @export
extract_proximity <- function(fit, ...) UseMethod("extract_proximity")
#' Extract proximity matrix from \code{randomForest} objects
#'
#' @param fit object of class randomForest called with proximity or oob.prox = TRUE
#'
#' @return an n by n matrix
#' @export
extract_proximity.randomForest <- function(fit) {
    if (is.null(fit$proximity))
        stop("call randomForest with proximity or oob.prox = TRUE")
    fit$proximity
}
#' Extract proximity matrix from objects returned from \code{cforest}
#'
#' @importFrom party proximity
#' @param fit object of class 'RandomForest' from \code{cforest}
#' @param newdata a data.frame with the same columns as the training data
#'
#' @return an n by n matrix
#' @export
extract_proximity.RandomForest <- function(fit, newdata = NULL) {
    proximity(out, newdata)
}
#' Extract proximity matrix from objects returned from \code{rfsrc}
#'
#' @param fit object of class 'rfsrc' from \code{rfsrc}
#'
#' @return an n by n matirx
#' @export
extract_proximity.rfsrc <- function(fit) {
    if (is.null(fit$proximity))
        stop("call rfsrc with proximity equal to TRUE, \"inbag\", \"oob\", or \"all\"")
    fit$proximity
}
