#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the Party, randomForest, or randomForestSRC packages
#'
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom assertthat assert_that is.count is.flag noNA
#' 
#' @param fit an object of class 'RandomForest-class' returned from \code{cforest}, an object
#' of class 'randomForest' returned from \code{randomForest}, or an object of class 'rfsrc'
#' returned from \code{rfsrc}
#' @param df the dataframe used to fit the model, if the model is a party object of class 'RandomForest'
#' this option can be omitted and the dataframe will be extracted from the object
#' @param var a character vector of the predictors of interest, which must match the input
#' matrix in the call to \code{cforest}, \code{randomForest}, or \code{randomForestSRC}
#' @param cutoff the maximal number of unique points in each element of 'var' used in the
#' partial dependence calculation
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#' @param parallel logical indicator of whether a parallel backend should be used if registered
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' library(party)
#' library(randomForestSRC)
#' ## library(doParallel)
#' ## library(parallel)
#' ## registerDoParallel(makeCluster(detectCores()))
#'
#' ## Classification
#' 
#' data(iris)
#' 
#' fit_rf <- randomForest(Species ~ ., iris)
#' fit_pt <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Species ~ ., iris)
#'
#' pd_rf <- partial_dependence(fit_rf, iris, "Petal.Width")
#' pd_pt <- partial_dependence(fit_pt, "Petal.Width")
#' pd_rfsrc <- partial_dependence(fit_rfsrc, "Petal.Width")
#'
#' pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"))
#' pd_int_pt <- partial_dependence(fit_pt, c("Petal.Width", "Sepal.Length"))
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, c("Petal.Width", "Sepal.Length"))
#'
#' ## Regression
#'
#' data(swiss)
#'
#' fit_rf <- randomForest(Fertility ~ ., swiss)
#' fit_pt <- cforest(Fertility ~ ., swiss, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Fertility ~ ., swiss)
#'
#' pd_rf <- partial_dependence(fit_rf, swiss, "Education")
#' pd_pt <- partial_dependence(fit_pt, "Education")
#' pd_rfsrc <- partial_dependence(fit_rfsrc, "Education")
#'
#' pd_int_rf <- partial_dependence(fit_rf, swiss, c("Education", "Catholic"))
#' pd_int_pt <- partial_dependence(fit_pt, c("Education", "Catholic"))
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, c("Education", "Catholic"))
#'
#' ## Survival
#'
#' data(veteran)
#'
#' fit_rfsrc <- rfsrc(Surv(time, status) ~ ., veteran)
#'
#' pd_rfsrc <- partial_dependence(fit_rfsrc, "age")
#'
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, c("age", "diagtime"))
#' }
#' @export
#' Creates a prediction vector for variables to decrease computation time
#'
#' @param df the dataframe used to fit the random forest, extracted from the fitted object
#' @param x a character vector of length 1 indicating the variable in `df` to be extracted
#' @param cutoff an integer indicating the maximal length of the vector to be used for prediction
#' @param empirical logical indicator of whether or not only values in the data should be sampled
#'  
#' @return a vector of unique values taken by `x` of length < `cutoff`
#' 
#' @export
ivar_points <- function(df, x, cutoff = 10, empirical = TRUE) {
    rng <- unique(df[, x])
    rng <- rng[!is.na(rng)]
    if (length(rng) > cutoff & !is.factor(df[, x])) {
        if (empirical == TRUE & cutoff < length(rng))
            rng <- sample(rng, cutoff)
        else if (empirical == FALSE)
            rng <- seq(min(rng), max(rng), length.out = cutoff)
    }
    class(rng) <- class(df[, x])
    return(rng)
}

partial_dependence <- function(fit, ...) UseMethod("partial_dependence")

partial_dependence.randomForest <- function(fit, df, var, cutoff = 10,
                                            empirical = TRUE, parallel = FALSE, ...) {
    args <- list(...)
    y_class <- attr(fit$terms, "dataClasses")[1]
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    type <- ifelse(is.null(args[["type"]]), "", args[["type"]])
    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "randomForest") %op% {
        df[, var] <- rng[i, ]
        if (y_class == "numeric" | y_class == "integer") {
            pred <- predict(fit, newdata = df)
            pred <- mean(pred)
        } else if (y_class == "factor") {
            if (type == "prob") {
                pred <- predict(fit, newdata = df, type = "prob")
                pred <- colMeans(pred)
            } else if (type == "class" | type == "") {
                pred <- predict(fit, newdata = df)
                pred <- table(pred)
                pred <- names(pred)[pred == max(pred)]
                if (length(pred) != 1) pred <- sample(pred, 1)
            } else stop("invalid type parameter passed to predict.randomForest*")
        } else stop("invalid response type")
        c(rng[i, ], pred)
    }
    if (length(var) > 1)
        pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)))
    else pred <- as.data.frame(do.call(rbind, pred))
    colnames(pred)[1:length(var)] <- var
    if (type != "prob")
        colnames(pred)[(length(var) + 1):ncol(pred)] <- names(y_class)
    pred
}

partial_dependence.RandomForest <- function(fit, var, cutoff = 10,
                                            empirical = TRUE, parallel = FALSE, ...) {
    args <- list(...)
    y <- get("response", fit@data@env)
    df <- data.frame(get("input", fit@data@env), y)
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    type <- ifelse(is.null(args[["type"]]), "", args[["type"]])
    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "party") %op% {
        df[, var] <- rng[i, ]
        if (dim(y)[2] == 1) {
            if (class(y[, 1]) == "numeric" | class(y[, 1]) == "integer") {
                    pred <- predict(fit, newdata = df)
                    pred <- mean(pred)
            } else if (class(y[, 1]) == "factor") {
                if (type == "prob") {
                    pred <- predict(fit, newdata = df, type = "prob")
                    pred <- do.call(rbind, pred)
                    pred <- colMeans(pred)
                } else if (type == "class" | type == "") {
                    pred <- predict(fit, newdata = df)
                    pred <- table(pred)
                    pred <- names(pred)[pred == max(pred)]
                    if (length(pred) != 1) pred <- sample(pred, 1)
                } else stop("invalid type parameter passed to predict.RandomForest*")
            } else stop("invalid response type")
        } else {
            pred <- predict(fit, newdata = df)
            pred <- do.call(rbind, pred)
            pred <- colMeans(pred)
        }
        c(rng[i, ], pred)
    }
    if (length(var) > 1)
        pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)))
    else pred <- as.data.frame(do.call(rbind, pred))
    colnames(pred)[1:length(var)] <- var
    colnames(pred)[(length(var) + 1):ncol(pred)] <- colnames(y)
    for (x in c(var, colnames(y))) {
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

partial_dependence.rfsrc <- function(fit, var, cutoff = 10, empirical = TRUE, parallel = FALSE, ...) {
    args <- list(...)
    y <- fit$yvar
    df <- data.frame(fit$xvar, y)
    if (!is.data.frame(y))
        colnames(df)[ncol(df)] <- fit$yvar.names
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    type <- ifelse(is.null(args[["type"]]), "", args[["type"]])
    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "party") %op% {
        df[, var] <- rng[i, ]
        pred <- predict(fit, newdata = df, outcome = "train")
        if (class(y) == "factor") {
            if (type == "prob") {
                pred <- pred$predicted
                pred <- colMeans(pred)
            } else if (type == "class" | type == "") {
                pred <- pred$class
                pred <- table(pred)
                pred <- names(pred)[pred == max(pred)]
                if (length(pred) != 1) pred <- sample(pred, 1)
            }
        } else if (class(y) == "numeric" | class(y) == "data.frame") {
            pred <- pred$predicted
            pred <- mean(pred)
        } else stop("invalid response type")
        c(rng[i, ], pred)
    }
    if (length(var) > 1)
        pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)))
    else pred <- as.data.frame(do.call(rbind, pred))
    colnames(pred)[1:length(var)] <- var
    if (is.data.frame(fit$yvar)) {
        colnames(pred)[ncol(pred)] <- "chf"
    else colnames(pred)[(length(var) + 1):ncol(pred)] <- fit$yvar.names
    pred
}
