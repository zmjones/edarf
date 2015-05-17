#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the party, randomForest, or randomForestSRC packages
#'
#' @importFrom foreach foreach %dopar% %do% %:% getDoParWorkers
#' @importFrom stats predict
#' @importFrom plyr ldply
#' @importFrom reshape2 melt
#' 
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param data the data.frame used to fit the model, only needed for randomForest
#' @param var a character vector of the predictors of interest, which must match the input matrix
#' @param cutoff the maximal number of unique points in each element of 'var' used in the
#' partial dependence calculation
#' @param interaction logical, if 'var' is a vector, does this specify an interaction or a list of bivariate partial dependence
#' @param oob logical, use the out-of-bag data to compute predictions at each step
#' @param ci use the bias corrected infinitesimal jackknife from Wager, Hastie, and Efron (2014), only works with regression
#' @param confidence desired confidence for the returned interval (ignored if ci is false)
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#' @param parallel logical indicator of whether a parallel backend should be used if registered
#' @param type with classification, default is "prob" which gives class probabilities estimated by the norm of the votes cast for each class, alternatively "class" gives the most probably class
#' @param drop_levels character vector specifying levels of a factor to drop, spaces in levels should be replaced by dots
#'
#' @return a data.frame with the partial dependence of 'var'
#' if 'var' has length = 1 then the output will be a data.frame with a column for the predicted value at each value of 'var', averaged over the values of all other predictors.
#' if 'var' has length > 1 and interaction is false the out is returned in melted form, i.e. there is a column 'value''variable', and the predicted value for that combination.
#' if 'var' has length > 1 and interaction is true then the output will be a data.frame with a column for each element of 'var' and the predicted value for each combination.
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' library(edarf)
#' 
#' data(iris)
#' data(swiss)
#' 
#' ## classification
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"))
#' pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"), interaction = TRUE)
#'
#' ## Regression
#' fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)
#' pd <- partial_dependence(fit, swiss, "Education", ci = TRUE)
#' pd_int <- partial_dependence(fit, swiss, c("Education", "Catholic"), interaction = TRUE, ci = TRUE)
#' }
#'
#' @export
partial_dependence <- function(fit, data, var, cutoff = 10L, interaction = FALSE, oob = TRUE,
                               ci = FALSE, confidence = .95, empirical = TRUE, parallel = FALSE,
                               type = "prob", drop_levels = NULL) UseMethod("partial_dependence", fit)
#' @export
partial_dependence.randomForest <- function(fit, data, var, cutoff = 10L, interaction = FALSE,
                                            oob = TRUE, ci = FALSE, confidence = .95,
                                            empirical = TRUE, parallel = FALSE, type = "prob",
                                            drop_levels = NULL) {
    pkg <- "randomForest"
    check <- lapply(1:ncol(data), function(x)
        all.equal(data[[x]], fit$y, use.names = FALSE, check.names = FALSE))
    check <- sapply(check, function(x) any(is.logical(x)))
    if (all(check == FALSE)) {
        data$target <- fit$y
        target <- "target"
    }
    target <- colnames(data)[check]
    
    if (class(data[[target]]) == "factor")  {
        if (type == "class" | type == "response") {
            predict_options <- list(object = fit, OOB = oob, type = "class")
        } else if (type == "prob") {
            predict_options <- list(object = fit, OOB = oob, type = "prob")
        } else {
            stop("invalid type argument")
        }
    } else if (class(data[[target]]) %in% c("numeric", "integer")) {
        predict_options <- list(object = fit, type = "response")
    } else {
        stop("invalid target type or unknown error")
    }
    
    .partial_dependence(pkg, data, target, var, cutoff, interaction, ci, confidence,
                        empirical, parallel, type, drop_levels, predict_options)
}
#' @export
partial_dependence.RandomForest <- function(fit, data = NULL, var, cutoff = 10L, interaction = FALSE,
                                            oob = TRUE, ci = FALSE, confidence = .95,
                                            empirical = TRUE, parallel = FALSE, type = "prob",
                                            drop_levels = NULL) {
    pkg <- "party"
    y <- get("response", fit@data@env)
    data <- data.frame(get("input", fit@data@env), y)
    target <- names(y)
    if (ncol(y) == 1) y <- y[[target]]
    
    if (!is.data.frame(y)) {
        if (class(data[[target]]) == "factor")  {
            if (type == "" | type == "class" | type == "response") {
                predict_options <- list(object = fit, OOB = oob, type = "response")
            } else if (type == "prob") {
                predict_options <- list(object = fit, OOB = oob, type = "prob")
            } else {
                stop("invalid type argument")
            }
        } else if (class(data[[target]]) %in% c("numeric", "integer")) {
            predict_options <- list(object = fit, type = "response")
        } else {
            stop("invalid target type or unknown error")
        }
    } else {
        predict_options <- list(object = fit, type = "response")
    }

    .partial_dependence(pkg, data, target, var, cutoff, interaction, ci, confidence,
                        empirical, parallel, type, drop_levels, predict_options)
}
#' @export
partial_dependence.rfsrc <- function(fit, data = NULL, var, cutoff = 10L, interaction = FALSE,
                                     oob = TRUE, ci = FALSE, confidence = .95,
                                     empirical = TRUE, parallel = FALSE,
                                     type = "prob", drop_levels = NULL) {
    pkg <- "randomForestSRC"
    target <- fit$yvar.names
    data <- data.frame(fit$xvar, fit$yvar) ## rfsrc casts integers to numerics
    colnames(data)[ncol(data)] <- target

    if (class(data[[target]]) == "factor")  {
        if (type == "" | type == "class" | type == "response") {
            predict_options <- list(object = fit, type = "response")
        } else if (type == "prob") {
            predict_options <- list(object = fit, type = "prob")
        } else {
            stop("invalid type argument")
        }
    } else if (class(data[[target]]) %in% c("numeric", "integer")) {
        predict_options <- list(object = fit, type = "response")
    } else {
        stop("invalid target type or unknown error")
    }
    
    .partial_dependence(pkg, data, target, var, cutoff, interaction, ci, confidence,
                        empirical, parallel, type, drop_levels, predict_options)
}

.partial_dependence <- function(pkg, data, target, var, cutoff, interaction,
                                ci, confidence,
                                empirical, parallel, type, drop_levels, predict_options) {
    if (length(target) == 1) y <- data[[target]]
    else y <- data[, target]
    if (type == "prob" & class(y) == "factor") target <- levels(y)
    if (!(class(y) %in% c("integer", "numeric"))) ci <- FALSE
    if (length(var) == 1) interaction <- FALSE

    if (interaction) {
        rng <- expand.grid(lapply(var, function(x) .ivar_points(data, x, cutoff, empirical)))
    } else if (length(var) > 1 & !interaction) {
        rng <- lapply(var, function(x) data.frame(.ivar_points(data, x, cutoff, empirical)))
        names(rng) <- var
    } else rng <- data.frame(.ivar_points(data, var, cutoff, empirical))
    ## check to see if parallel backend registered
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    inner_loop <- function(data, rng, idx, var, var_class) {
        ## fix var predictiors
        data[, var] <- rng[idx, ]
        ## check to see if we are doing a multivariate fit
        if (class(y) != "data.frame") {
            if (class(y) %in% c("numeric", "integer")) {
                ## if doing regression and standard errors requested use
                ## the var_est method to use the bias corrected infinitesimal jackknife
                ## from Wager, Efron, and Tibsharani (2014). the implementation is a modified
                ## version of the code from randomForestCI
                ## then take column means (if we have a variance estimate) or just the mean,
                ## across observations
                if (ci) {
                    pred <- colMeans(var_est(predict_options$object, data))
                } else {
                    pred <- do.call("predict", c(predict_options, list(newdata = data)))
                    if (pkg == "randomForestSRC") pred <- pred$predicted
                    pred <- mean(pred)
                }
            } else if (class(y) == "factor") {
                ## if y is a factor and we want class probs return the mean prob across
                ## obs. for each observation. predict.cforest returns a list, which is row binded
                ## and then the means are computed, i also remove the name of the factor (y)
                ## and just use the level labels as the column names (same as randomForest)
                if (type == "prob") {
                    pred <- do.call("predict", c(predict_options, list(newdata = data)))
                    if (pkg == "randomForestSRC") pred <- pred$predicted
                    if (is.list(pred)) pred <- do.call("rbind", pred)
                    pred <- colMeans(pred)
                    if (!all(names(pred) == levels(y)))
                        names(pred) <- gsub("^.*\\.", "", names(pred))
                    if (!is.null(drop_levels)) {
                        if (!drop_levels %in% names(pred))
                            stop("drop_levels not a level of y")
                        pred <- pred[!names(pred) %in% drop_levels]
                    }
                } else if (type == "class") {
                    ## if no class probs requested just find the name of the maximal class
                    ## and randomly pick one if there are ties
                    pred <- do.call("predict", c(predict_options, list(newdata = data)))
                    if (pkg == "randomForestSRC") pred <- pred$class
                    pred <- names(which.max(table(pred)))
                    if (length(pred) != 1) pred <- sample(pred, 1)
                } else stop("invalid type parameter")
            } else stop("invalid response type")
        } else {
            pred <- do.call("predict", c(predict_options, list(newdata = data)))
            pred <- colMeans(do.call("rbind", pred))
            facts <- names(y)[sapply(data[ names(y)], class) %in% c("character", "factor")]
            matched <- grepl(paste("^", facts, "*.", sep = "", collapse = "|"), names(pred))
            pattern <- paste(paste0("^", facts, "\\."), collapse = "|")
            names(pred)[matched] <- gsub(pattern, "", names(pred)[matched])
            if (!is.null(drop_levels)) {
                if (any(grepl("\\s+", drop_levels)))
                    stop("predict.RandomForest replaces spaces with periods in level names")
                if (!drop_levels %in% names(pred))
                    stop("drop_levels not a level of y")
                pred <- pred[!names(pred) %in% drop_levels]
            }
        }
        out <- unlist(c(rng[idx, ], pred))
        if (is.null(drop_levels))
            names(out)[1:(length(var) + length(target))] <- c(var, target)
        else
            names(out) <- c(var, target[!target %in% drop_levels])
        if (ci) {
            cl <- qnorm((1 - confidence) / 2, lower.tail = FALSE)
            se <- unname(sqrt(out["variance"]))
            out["low"]<- out[target] - cl * se
            out["high"] <- out[target] + cl * se
        }
        out
    }
    i <- x <- idx <- out <- NULL ## initialize to avoid check errors
    comb <- function(...) as.data.frame(do.call("rbind", list(...)), stringsAsFactors = FALSE)
    if (is.data.frame(rng)) {
        ## fixme
        ## should figure out what is generating the coercion
        ## that necessitates the below code
        if (length(var) > 1) {
            var_class <- sapply(data[, var], class)
        } else var_class <- class(data[, var])
        pred <- foreach::foreach(idx = 1:nrow(rng), .packages = pkg, .combine = comb) %op%
            inner_loop(data, rng, idx, var, var_class)
    } else {
        pred <- foreach::foreach(x = var, .packages = pkg) %:%
            foreach::foreach(idx = 1:nrow(rng[[x]]), .combine = comb) %op%
                inner_loop(data, rng[[x]], idx, x, class(data[, x]))
        pred <- reshape2::melt(plyr::ldply(pred), id.vars = target, na.rm = TRUE)
    }
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "target") <- target
    attr(pred, "prob") <- type == "prob" & class(y) == "factor"
    attr(pred, "interaction") <- length(var) > 1 & interaction
    attr(pred, "multivariate") <- class(y) == "data.frame"
    attr(pred, "var") <- var
    attr(pred, "ci") <- ci
    pred <- .fix_classes(data, pred)
    if (length(var) > 1 & !interaction & "value" %in% colnames(pred)) pred$value <- as.numeric(pred$value)
    pred
}

.ivar_points <- function(data, x, cutoff = 10, empirical = TRUE) {
    rng <- unique(data[[x]])
    rng <- rng[!is.na(rng)]
    if (length(rng) > cutoff & !is.factor(data[[x]])) {
        if (empirical == TRUE & cutoff < length(rng)) {
            rng_s <- sort(rng)[-c(1, length(rng))]
            rng <- c(sample(rng_s, (cutoff - 2)), range(rng))
        } else if (empirical == FALSE)
              rng <- seq(min(rng), max(rng), length.out = cutoff)
    }
    class(rng) <- class(data[[x]])
    return(rng)
}

.fix_classes <- function(data, pred) {
    for (x in colnames(pred)) {
        if (x %in% colnames(data)) {
            if (class(data[[x]]) == "factor")
                pred[, x] <- factor(pred[, x])
            else if (class(data[[x]]) == "numeric") {
                pred[, x] <- as.numeric(pred[, x])
            } else if (class(data[[x]]) == "integer") {
                pred[, x] <- as.integer(pred[, x])
            } else if (class(data[[x]]) == "character") {
                pred[, x] <- as.character(pred[, x])
            } else  {
                stop("column of unsupported type input")
            }
        }
    }
    pred
}
