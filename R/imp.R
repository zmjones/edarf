#' Variable importance using random forests
#'
#' Computes local or aggregate variable importance for a set of predictors from a fitted random forest object from the party, randomForest, or randomForestSRC package
#'
#' @importFrom iterators icount
#' @importFrom foreach foreach %dopar% %do% %:% getDoParWorkers
#' @importFrom stats predict
#'
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param var character, variables to find the importance of
#' @param type character, either "aggregate," (default) which gives the average loss under permutation or "local" which gives the residual across all observations
#' @param oob logical, use the out of bag data data
#' @param nperm positive integer giving the number of times to permute the indicated variables (default 10)
#' @param parallel logical whether to run in parallel using the registered backend (default FALSE)
#' @param data optional (unless using randomForest) data.frame with which to calculate importance
#'
#' @return a named numeric vector for type = "aggregate" or a data.frame for type = "local"
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' data(iris)
#'
#' fit <- randomForest(Species ~ ., iris, importance = TRUE)
#' variable_importance(fit, var = colnames(iris)[-5], type = "aggregate", nperm = 10, data = iris)
#'
#' library(party)
#' data(swiss)
#' fit <- cforest(Fertility ~ ., swiss)
#' variable_importance(fit, var = colnames(swiss)[-1], type = "local", nperm = 10)
#'
#' data(mtcars)
#' library(randomForestSRC)
#' fit <- rfsrc(mpg ~ ., mtcars)
#' variable_importance(fit, var = colnames(mtcars)[-1], type = "aggregate", nperm = 10, oob = TRUE)
#' }
#' @export
variable_importance <- function(fit, var, type, oob, nperm, parallel, data) UseMethod("variable_importance")
#' @export
variable_importance.randomForest <- function(fit, var, type = "aggregate", oob = TRUE,
                                             nperm = 100, parallel = FALSE,
                                             data = NULL) {
    out <- .variable_importance(fit, var, type, nperm, parallel, data,
                                y = fit$y, list(object = fit, type = "response", OOB = oob),
                                "randomForest")
    return(out)
}
#' @export
variable_importance.RandomForest <- function(fit, var, type = "aggregate", oob = TRUE,
                                             nperm = 100, parallel = FALSE) {
    out <- .variable_importance(fit, var, type, nperm, parallel,
                                get("input", fit@data@env),
                                get("response", fit@data@env)[, 1],
                                list(object = fit, type = "response", OOB = oob),
                                "party")
    return(out)
}
#' @export
variable_importance.rfsrc <- function(fit, var, type = "aggregate", oob = TRUE, nperm = 100, parallel = FALSE) {
    out <- .variable_importance(fit, var, type, nperm, parallel,
                                fit$xvar, fit$yvar, list(object = fit), "randomForestSRC")
    return(out)
}

permute_data <- function(data, x) {
    n <- nrow(data)
    pidx <- sample(1:n, size = n, replace = FALSE)
    pdata <- data
    pdata[, x] <- pdata[pidx, x]
    return(pdata)
}

.variable_importance <- function(fit, var, type, nperm, parallel,
                                 data, y, predict_options, pkg, oob = NULL) {
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    ensemble_pred <- do.call("predict", c(predict_options, list(newdata = data)))

    if (pkg == "randomForestSRC") ensemble_pred <- ensemble_pred$predicted

    if (type == "local") {
        if (is.factor(y) & !is.ordered(y)) loss <- function(x, y) ifelse(x != y, 1, 0)
        else if (is.ordered(y)) loss <- function(x, y) as.integer(x) - as.integer(y)
        else loss <- function(x, y) x - y
        ensemble_resid <- loss(ensemble_pred, y)
        comb <- function(...) rowMeans(do.call("cbind", list(...)))

        out <- foreach::foreach(x = var, .combine = "cbind", .packages = pkg) %:%
            foreach::foreach(iterators::icount(nperm), .combine = comb, .packages = pkg) %op% {
                pdata <- permute_data(data, x)
                p <- do.call("predict", c(predict_options, list(newdata = pdata)))
                if (pkg == "randomForestSRC") p <- p$predicted
                loss(p, y) - ensemble_resid
            }
        out <- as.data.frame(out)
        colnames(out) <- var
    } else if (type == "aggregate") {
        if (is.factor(y) & !is.ordered(y)) loss <- function(x, y) mean(x != y)
        else loss <- function(x, y) mean((x - y)^2)
        ensemble_loss <- loss(ensemble_pred, y)
        comb <- function(...) mean(do.call("c", list(...)))
        
        out <- foreach::foreach(x = var, .combine = "c", .packages = pkg) %:%
            foreach::foreach(iterators::icount(nperm), .combine = comb, .packages = pkg) %op% {
                pdata <- permute_data(data, x)
                p <- do.call("predict", c(predict_options, list(newdata = pdata)))
                if (pkg == "randomForestSRC") p <- p$predicted
                loss(p, y) - ensemble_loss
            }
        names(out) <- var
    } else {
        stop("unsupported type argument")
    }

    attr(out, "class") <- c("importance", ifelse(type == "aggregate", "numeric", "data.frame"))
    attr(out, "type") <- type
    attr(out, "var") <- var
    attr(out, "oob") <- oob
    out
}
