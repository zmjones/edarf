#' Variable importance using random forests
#'
#' Extracts variable importance for a set of predictors from a fitted random forest object from the party, randomForest, or randomForestSRC package
#'
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param type character, for randomForest equal to "accuracy", "gini", or "local"
#' if type is "accuracy" importance must be set to TRUE in the call to randomForest
#' for RandomForest (class for cforest) may be equal to "conditional" and/or "auc" (type may have length two), which will return the conditional/or auc importance
#' @param class_levels logical if possible return class specific importance, i.e. how much does the loss increase for each class from perturbing each feature in turn
#' @param ... arguments to be passed to \code{variable_importance}
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' data(iris)
#'
#' fit <- randomForest(Species ~ ., iris, importance = TRUE)
#' imp <- variable_importance(fit, "accuracy", TRUE)
#' plot_imp(imp)
#' }
#' @export
variable_importance <- function(fit, type, class_levels = FALSE, ...) UseMethod("variable_importance")
#' @export
variable_importance.randomForest <- function(fit, type = "accuracy", class_levels = FALSE, ...) {
    if (ncol(fit$importance) == 1 & type != "gini")
        stop("set importance = TRUE in call to randomForest")
    if (is.null(fit$localImportance) & type == "local")
        stop("set localImp = TRUE in call to randomForest")
    
    if (class_levels & class(fit$y) == "factor" & type == "accuracy")
        out <- fit$importance[, levels(fit$y)]
    else if (type == "accuracy")
        out <- fit$importance[, "MeanDecreaseAccuracy"]
    else if (type == "gini")
        out <- fit$importance[, "MeanDecreaseGini"]
    else if (type == "local") {
        out <- t(fit$localImportance)
    } else
        stop("Invalid type or fit input combination")

    if (is.matrix(out)) {
        out <- as.data.frame(out)
        if (type != "local") {
            out$labels <- row.names(out)
            row.names(out) <- NULL
        }
    } else 
        out <- data.frame(value = unname(out), labels = names(out))
    
    attr(out, "class") <- c("importance", "data.frame")
    attr(out, "type") <- type
    attr(out, "class_levels") <- class_levels
    out
}
#' @export
variable_importance.RandomForest <- function(fit, type = "", ...) {
    if ("auc" %in% type & !(nrow(unique(fit@responses@variables)) == 2))
        stop("auc only applicable to binary classification")
    
    if ("auc" %in% type)
        out <- party::varimpAUC(fit, conditional = "conditional" %in% type, ...)
    else
        out <- party::varimp(fit, conditional = "conditional" %in% type, ...)

    out <- data.frame("value" = out, "labels" = names(out), row.names = 1:length(out))
    
    attr(out, "class") <- c("importance", "data.frame")
    attr(out, "type") <- type
    attr(out, "class_levels") <- FALSE
    out
}
#' @export
variable_importance.rfsrc <- function(fit, type = "permute", class_levels = FALSE, ...) {
    if (!type %in% as.character(fit$call))
        stop(paste("call rfsrc with importance =", type))
    
    if (class_levels) {
        out <- fit$importance[, levels(fit$yvar)]
        out <- as.data.frame(out)
        out$labels <- row.names(out)
        row.names(out) <- NULL
    } else {
        out <- fit$importance[, "all"]
        out <- data.frame(value = unname(out), labels = names(out))
    }
    
    attr(out, "class") <- c("importance", "data.frame")
    attr(out, "type") <- type
    attr(out, "auc") <- FALSE
    attr(out, "class_levels") <- class_levels
    out
}
