#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the party, randomForest, or randomForestSRC packages
#'
#' @importFrom foreach foreach %dopar% %do% %:% getDoParWorkers
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param ... arguments to be passed to \code{partial_dependence}
#'
#' @export
partial_dependence <- function(fit, ...) UseMethod("partial_dependence", fit)
#' Partial dependence for randomForest objects
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the randomForest packages
#'
#' @param fit an object of class 'randomForest' returned from \code{randomForest}
#' @param df the dataframe used to fit the model
#' @param var a character vector of the predictors of interest, which must match the input matrix in the call to \code{randomForest}
#' @param cutoff the maximal number of unique points in each element of 'var' used in the
#' partial dependence calculation
#' @param ci use the bias corrected infinitesimal jackknife from Wager, Hastie, and Efron (2014) implemented in randomForestCI, currently only works with regression
#' @param confidence desired confidence for the returned interval (ignored if ci is false)
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#' @param parallel logical indicator of whether a parallel backend should be used if registered
#' @param type with classification, default "" gives most probable class for classification and "prob" gives class probabilities
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`. The dataframe also has class "pd" with attributes "class", "prob", "multivariate", and "interaction", which are used by the plot method.
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' library(edarf)
#' ## library(doParallel)
#' ## library(parallel)
#' ## registerDoParallel(makeCluster(detectCores()))
#'
#' ## Classification
#' 
#' data(iris)
#' 
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"))
#' pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"), interaction = TRUE)
#'
#' ## Regression
#'
#' data(swiss)
#'
#' fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)
#' pd <- partial_dependence(fit, swiss, "Education", ci = TRUE)
#' pd_int <- partial_dependence(fit, swiss, c("Education", "Catholic"), interaction = TRUE, ci = TRUE)
#' }
#' @export
partial_dependence.randomForest <- function(fit, df, var, cutoff = 10, interaction = FALSE,
                                            ci = TRUE, confidence = .95,
                                            empirical = TRUE, parallel = FALSE, type = "") {
    pkg <- "randomForest"
    y_class <- attr(fit$terms, "dataClasses")[1] ## what type is y
    if (!y_class %in% c("integer", "numeric") & ci) ci <- FALSE
    ## get the prediction grid for whatever var is
    if (length(var) == 1 | interaction)
        rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    else
        rng <- lapply(var, function(x) data.frame(ivar_points(df, x, cutoff, empirical)))
    ## run the pd algo in parallel?
    '%op%' <- ifelse(getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    inner_loop <- function(df, rng, fit, idx) {
        ## fix var to points in prediction grid
        df[, var] <- rng[idx, ]
        ## if numeric outcome predict and take the mean, if standard errors requested
        ## use the bias-corrected infinitesimal jackknife implemented in randomForestCI
        ## which is called using the var_est method
        if (y_class %in% c("numeric", "integer")) {
            if (ci) pred <- colMeans(var_est(fit, df))
            else pred <- mean(predict(fit, newdata = df))
        } else if (y_class == "factor") {
            ## if y is a factor and class probs are requested (soft voting), get the probs
            ## and take their mean across all obs.
            if (type == "prob") pred <- colMeans(predict(fit, newdata = df, type = type))
            else if (type == "class" | type == "") {
                ## if no class probs, then just find the maximal class
                ## and if there are ties randomly pick one
                pred <- names(which.max(table(predict(fit, newdata = df))))
                if (length(pred) != 1) pred <- sample(pred, 1)
            } else stop("invalid type parameter passed to predict.randomForest*")
        } else stop("invalid response type")
        c(rng[idx, ], pred)
    }
    ## loop over points in prediction grid (rng)
    if (is.data.frame(rng)) {
        pred <- foreach(i = 1:nrow(rng), .packages = pkg) %op%
                     inner_loop(df, rng, fit, i)
        pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)), stringsAsFactors = FALSE)
        colnames(pred)[1:length(var)] <- var
        if (type != "prob" & (!ci | !(y_class %in% c("numeric", "integer"))))
            colnames(pred)[ncol(pred)] <- names(y_class)
    } else {
        pred <- foreach(x = rng, .packages = pkg) %:%
        foreach(idx = 1:nrow(x), .combine = rbind) %op% inner_loop(df, x, fit, idx)
        ## op not appropriate here, too much overhead, for some reason referencing foreach doesn't work
        ## e.g. foreach::'%do%' fails
        pred <- foreach(i = 1:length(pred), .combine = rbind) %do% {
            out <- data.frame(pred[[i]], "variable" = var[i], stringsAsFactors = FALSE)
            colnames(out)[1:2] <- c("value", names(y_class))
            out$value <- as.numeric(out$value)
            out
        }
        row.names(pred) <- NULL
        ## probably the value column needs to be restricted to be a numeric or integer vector
        ## should check to see what is up. not sure what to do with categorical predictors
    }
    if (ci & y_class %in% c("integer", "numeric")) {
        if (length(var) == 1 | interaction) colnames(pred)[ncol(pred) - 1] <- names(y_class)
        ## compute 1 - confidence intervals
        cl <- qnorm((1 - confidence) / 2, lower.tail = FALSE)
        se <- sqrt(pred$variance)
        pred$low <- pred[, names(y_class)] - cl * se
        pred$high <- pred[, names(y_class)] + cl * se
    } else if (length(var) == 1)
          pred <- fix_classes(c(var, names(y_class)), df, pred)
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "prob") <- type == "prob"
    attr(pred, "interaction") <- length(var) > 1
    attr(pred, "multivariate") <- FALSE
    attr(pred, "var") <- var
    attr(pred, "ci") <- ci
    pred
}
#' Partial dependence for RandomForest objects from package \code{party}
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the \code{party} package
#'
#' @param fit an object of class 'RandomForest' returned from \code{cforest}
#' @param var a character vector of the predictors of interest, which must match the input matrix in the call to \code{randomForest}
#' @param cutoff the maximal number of unique points in each element of 'var' used in the
#' partial dependence calculation
#' @param ci use the bias corrected infinitesimal jackknife from Wager, Hastie, and Efron (2014)
#' @param confidence desired confidence for the returned interval (ignored if ci is false)
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#' @param parallel logical indicator of whether a parallel backend should be used if registered
#' @param type with classification, default "" gives most probable class for classification and "prob" gives class probabilities
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`. The dataframe also has class "pd" with attributes "class", "prob", "multivariate", and "interaction", which are used by the plot method.
#'
#' @examples
#' \dontrun{
#' library(party)
#' library(edarf)
#' ## library(doParallel)
#' ## library(parallel)
#' ## registerDoParallel(makeCluster(detectCores()))
#'
#' ## Classification
#' 
#' data(iris)
#' 
#' fit <- cforest(Species ~ ., iris, controls = cforest_unbiased(mtry = 2))
#' pd <- partial_dependence(fit, "Petal.Width")
#' pd_int <- partial_dependence(fit, c("Petal.Width", "Sepal.Length"))
#'
#' ## Regression
#'
#' data(swiss)
#'
#' fit <- cforest(Fertility ~ ., swiss, controls = cforest_control(mtry = 2))
#' pd <- partial_dependence(fit, "Education")
#' pd_int <- partial_dependence(fit, c("Education", "Catholic"))
#'
#' ## Multivariate
#' 
#' data(mtcars)
#'
#' fit <- cforest(hp + qsec ~ ., mtcars, controls = cforest_control(mtry = 2))
#' pd <- partial_dependence(fit, "mpg")
#' pd_int <- partial_dependence(fit, c("mpg", "cyl"))
#' }
#' @export
partial_dependence.RandomForest <- function(fit, var, cutoff = 10, ci = TRUE, confidence = .95,
                                            empirical = TRUE, parallel = FALSE, type = "") {
    ## get y from the fit object
    y <- get("response", fit@data@env)
    if (ncol(y) > 1 | !(class(y[, 1]) %in% c("integer", "numeric"))) ci <- FALSE
    ## get input data and combine into model data.frame
    df <- data.frame(get("input", fit@data@env), y)
    ## get prediction grid for whatever is in var
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    ## check to see if parallel backend registered
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "party") %op% {
        ## fix var predictiors
        df[, var] <- rng[i, ]
        ## check to see if we are doing a multivariate fit
        if (dim(y)[2] == 1) {
            if (class(y[, 1]) == "numeric" | class(y[, 1]) == "integer") {
                ## if doing regression and standard errors requested use
                ## the var_est method to use the bias corrected infinitesimal jackknife
                ## from Wager, Efron, and Tibsharani (2014). the implementation is a modified
                ## version of the code from randomForestCI
                ## then take column means (if we have a variance estimate) or just the mean,
                ## across observations
                if (ci) pred <- colMeans(var_est(fit, df))
                else pred <- mean(predict(fit, newdata = df))
            } else if (class(y[, 1]) == "factor") {
                ## if y is a factor and we want class probs return the mean prob across
                ## obs. for each observation. predict.cforest returns a list, which is row binded
                ## and then the means are computed, i also remove the name of the factor (y)
                ## and just use the level labels as the column names (same as randomForest)
                if (type == "prob") {
                    pred <- colMeans(do.call(rbind, predict(fit, newdata = df, type = type)))
                    names(pred) <- gsub(paste0(names(y), "\\."), "", names(pred))
                } else if (type == "class" | type == "") {
                    ## if no class probs requested just find the name of the maximal class
                    ## and randomly pick one if there are ties
                    pred <- names(which.max(table(predict(fit, newdata = df))))
                    if (length(pred) != 1) pred <- sample(pred, 1)
                } else stop("invalid type parameter passed to predict.RandomForest*")
            } else stop("invalid response type")
        } else pred <- colMeans(do.call(rbind, predict(fit, newdata = df)))
        c(rng[i, ], pred)
    }
    if (length(var) > 1)
        pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)))
    else pred <- as.data.frame(do.call(rbind, pred))
    colnames(pred)[1:length(var)] <- var
    if (type != "prob" & !ci) {
        ## make sure types agree with the input
        ## not really sure now where the coercion comes from
        colnames(pred)[(length(var) + 1):ncol(pred)] <- colnames(y)
        pred <- fix_classes(c(var, colnames(y)), df, pred)
    } else if (ci) {
        ## compute 1 - confidence intervals
        cl <- qnorm((1 - confidence) / 2, lower.tail = FALSE)
        se <- sqrt(pred$variance)
        pred$low <- pred[, names(y)] - cl * se
        pred$high <- pred[, names(y)] + cl * se
    }
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "prob") <- type == "prob"
    attr(pred, "interaction") <- length(var) > 1
    attr(pred, "multivariate") <- dim(y)[2] != 1
    attr(pred, "var") <- var
    attr(pred, "ci") <- ci
    pred
}
#' Partial dependence for rfsrc objects from package \code{randomForestSRC}
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the \code{randomForestSRC} package
#'
#' @param fit an object of class 'rfsrc' returned from \code{rfsrc}
#' @param var a character vector of the predictors of interest, which must match 
#' the input matrix in the call to \code{rfsrc}
#' @param cutoff the maximal number of unique points in each element of 'var' used in the
#' partial dependence calculation
#' @param ci use the bias corrected infinitesimal jackknife from Wager, Hastie, and Efron (2014)
#' @param confidence desired confidence for the returned interval (ignored if ci is false)
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#' @param parallel logical indicator of whether a parallel backend should be used if registered
#' @param type with classification, default "" gives most probable class for classification and "prob" gives class probabilities
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`. The dataframe also has class "pd" with attributes "class", "prob", "multivariate", and "interaction", which are used by the plot method.
#'
#' @examples
#' \dontrun{
#' library(randomForestSRC)
#' library(edarf)
#' ## library(doParallel)
#' ## library(parallel)
#' ## registerDoParallel(makeCluster(detectCores()))
#'
#' ## Classification
#' data(iris)
#' fit <- rfsrc(Species ~ ., iris)
#' pd <- partial_dependence(fit, "Petal.Width")
#' pd_int <- partial_dependence(fit, c("Petal.Width", "Sepal.Length"))
#'
#' ## Regression
#' data(swiss)
#' fit <- rfsrc(Fertility ~ ., swiss)
#' pd <- partial_dependence(fit, "Education")
#' pd_int <- partial_dependence(fit, c("Education", "Catholic"))
#'
#' ## Survival
#' data(veteran)
#' fit <- rfsrc(Surv(time, status) ~ ., veteran)
#' pd <- partial_dependence(fit_rfsrc, "age")
#' pd_int <- partial_dependence(fit_rfsrc, c("age", "diagtime"))
#' }
#' @export
partial_dependence.rfsrc <- function(fit, var, cutoff = 10, ci = TRUE, confidence = .95,
                                     empirical = TRUE, parallel = FALSE, type = "") {
    y <- fit$yvar
    if (!(class(y) %in% c("numeric", "integer"))) ci <- FALSE
    df <- data.frame(fit$xvar, y)
    if (!is.data.frame(y))
        colnames(df)[ncol(df)] <- fit$yvar.names
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "randomForestSRC") %op% {
        df[, var] <- rng[i, ]
        pred <- predict(fit, newdata = df, outcome = "train")
        fit$pd_membership <- pred$membership
        fit$pd_predicted <- pred$predicted
        if (class(y) == "factor") {
            if (type == "prob")
                pred <- colMeans(pred$predicted)
            else if (type == "class" | type == "") {
                pred <- names(which.max(table(pred$class)))
                if (length(pred) != 1) pred <- sample(pred, 1)
            }
        } else if (class(y) == "numeric" | class(y) == "data.frame") {
            if (class(y) == "numeric" & ci) pred <- colMeans(var_est(fit, df))
            else pred <- mean(pred$predicted)
        } else stop("invalid response type")
        c(rng[i, ], pred)
    }
    if (length(var) > 1)
        pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)))
    else pred <- as.data.frame(do.call(rbind, pred))
    colnames(pred)[1:length(var)] <- var
    if (is.data.frame(fit$yvar)) {
        colnames(pred)[ncol(pred)] <- "chf"
        pred[, -ncol(pred)] <- fix_classes(var, df, pred[, -ncol(pred)])
    } else if (type != "prob" & !ci) {
        colnames(pred)[ncol(pred)] <- fit$yvar.names
        pred <- fix_classes(c(var, fit$yvar.names), df, pred)
    } else if (ci & class(y) == "numeric") {
        colnames(pred)[ncol(pred) - 1] <- fit$yvar.names
        ## compute 1 - confidence intervals
        cl <- qnorm((1 - confidence) / 2, lower.tail = FALSE)
        se <- sqrt(pred$variance)
        pred$low <- pred[, fit$yvar.names] - cl * se
        pred$high <- pred[, fit$yvar.names] + cl * se
    }
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "prob") <- type == "prob"
    attr(pred, "interaction") <- length(var) > 1
    attr(pred, "multivariate") <- FALSE
    attr(pred, "var") <- var
    attr(pred, "ci") <- ci
    pred
}
#' Creates a prediction vector for variables to decrease computation time
#'
#' @param df the dataframe used to fit the random forest, extracted from the fitted object
#' @param x a character vector of length 1 indicating the variable in `df` to be extracted
#' @param cutoff an integer indicating the maximal length of the vector to be used for prediction
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#'  
#' @return a vector of unique values taken by \code{x} of length < `cutoff`
#'
#' @export
ivar_points <- function(df, x, cutoff = 10, empirical = TRUE) {
    rng <- unique(df[, x])
    rng <- rng[!is.na(rng)]
    if (length(rng) > cutoff & !is.factor(df[, x])) {
        if (empirical == TRUE & cutoff < length(rng)) {
            rng_s <- sort(rng)[-c(1, length(rng))]
            rng <- c(sample(rng_s, (cutoff - 2)), range(rng))
        } else if (empirical == FALSE)
              rng <- seq(min(rng), max(rng), length.out = cutoff)
    }
    class(rng) <- class(df[, x])
    return(rng)
}
#' Matches column classes of the input data frame to the output
#'
#' @param var character vector of column names to match
#' @param df imput dataframe
#' @param pred output dataframe
#'
#' @return dataframe \code{pred} with \code{var} column classes matched to those in \code{df}
#'
#' @export
fix_classes <- function(var, df, pred) {
    for (x in var) {
        if (class(df[, x]) == "factor")
            pred[, x] <- factor(pred[, x])
        else if (class(df[, x]) == "numeric") {
            if (any(df[, x] %% 1 != 0))
                pred[, x] <- as.numeric(pred[, x])
            else pred[, x] <- as.integer(pred[, x])
        } 
    }
    pred
}
