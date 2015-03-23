#' Variable importance using random forests
#'
#' Extracts variable importance for a set of predictors from a fitted random forest object from the party, randomForest, or randomForestSRC package
#'
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param ... arguments to be passed to \code{variable_importance}
#'
#' @export
variable_importance <- function(fit, ...) UseMethod("variable_importance")
#' Variable importance for randomForest objects
#'
#' Extracts variable importances from a fitted \code{randomForest}
#'
#' @param fit an object of class 'randomForest' returned from \code{randomForest}
#' @param type character equal to "accuracy", "gini", or "local"
#' if type is "accuracy" importance must be set to TRUE in the call to randomForest
#' @param class_levels logical, when TRUE class level specific importances are returned
#' response variable must be a factor and importance = TRUE in the call to randomForest
#' @param ... further arguments to be passed to nothing
#'
#'
#' @return a data.frame of class "importance"
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
    attr(out, "auc") <- FALSE
    attr(out, "class_levels") <- class_levels
    out
}
#' Variable importance for RandomForest objects
#'
#' Extracts variable importances from a fitted \code{cforest}
#'
#' @importFrom party varimp varimpAUC
#' @param fit an object of class 'randomForest' returned from \code{randomForest}
#' @param conditional logical, if true the conditional permutation importance is estimated, if not the marginal
#' @param auc logical, whether to use varimpAUC instead of varimp
#' @param ... further arguments to be passed to varimp or varimpAUC
#'
#' @return a data.frame of class "importance"
#'
#' @examples
#' \dontrun{
#' library(party)
#' data(iris)
#'
#' fit <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
#' imp <- variable_importance(fit)
#' plot_imp(imp)
#' }
#' @export
variable_importance.RandomForest <- function(fit, conditional = FALSE, auc = FALSE, ...) {
    if (auc & !(nrow(unique(fit@responses@variables)) == 2))
        stop("auc only applicable to binary classification")
    
    if (conditional)
        conditional <- TRUE
    else conditional <- FALSE

    if (auc)
        out <- party::varimpAUC(fit, conditional = conditional, ...)
    else
        out <- party::varimp(fit, conditional = conditional, ...)

    out <- data.frame("value" = out, "labels" = names(out), row.names = 1:length(out))
    
    attr(out, "class") <- c("importance", "data.frame")
    attr(out, "type") <- ""
    attr(out, "auc") <- auc
    attr(out, "class_levels") <- FALSE
    attr(out, "conditional") <- conditional
    out
}
#' Variable importance for rfsrc objects
#'
#' Extracts variable importances from a fitted \code{rfsrc}
#'
#' @param fit an object of class 'rfsrc' returned from \code{rfsrc}
#' @param type character equal to "permute", "random", "permute.ensemble", or "random.ensemble"
#' this the \code{permute} argument must equal this value in the call to rfsrc
#' @param class_levels logical, when TRUE class level specific importances are returned otherwise the overal importance is returned
#' @param ... further arguments to be passed to nothing
#'
#' @return a data.frame of class "importance"
#'
#' @examples
#' \dontrun{
#' library(randomForestSRC)
#' data(iris)
#'
#' fit <- rfsrc(Species ~ ., iris, importance = "random")
#' variable_importance(fit, "random", TRUE)
#' }
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
