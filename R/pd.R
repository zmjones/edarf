#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the Party, randomForest, or randomForestSRC packages
#'
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom assertthat assert_that on_failure is.string is.count is.flag noNA
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
#' pd_pt <- partial_dependence(fit_pt, iris, "Petal.Width")
#' pd_rfsrc <- partial_dependence(fit_rfsrc, iris, "Petal.Width")
#'
#' pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"))
#' pd_int_pt <- partial_dependence(fit_pt, iris, c("Petal.Width", "Sepal.Length"))
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, iris, c("Petal.Width", "Sepal.Length"))
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
#' pd_pt <- partial_dependence(fit_pt, swiss, "Education")
#' pd_rfsrc <- partial_dependence(fit_rfsrc, swiss, "Education")
#'
#' pd_int_rf <- partial_dependence(fit_rf, swiss, c("Education", "Catholic"))
#' pd_int_pt <- partial_dependence(fit_pt, swiss, c("Education", "Catholic"))
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, swiss, c("Education", "Catholic"))
#'
#'
#' ## Survival
#'
#' data(veteran, package = "randomForestSRC")
#'
#' fit_rfsrc <- rfsrc(Surv(time, status) ~ ., veteran)
#'
#' pd_rfsrc <- partial_dependence(fit_rfsrc, veteran, "age")
#'
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, veteran, c("age", "diagtime"))
#' }
#' @export
partial_dependence <- function(fit, df, var, cutoff = 10, empirical = TRUE) {
    assert_that(any(class(fit) %in% c("RandomForest", "randomForest", "randomForestSRC")))
    assert_that(is.data.frame(df))
    assert_that(is.string(var))
    assert_that(is.count(cutoff))
    assert_that(is.flag(empirical))
    assert_that(cutoff > nrow)
    
    if (any(class(fit) == "RandomForest")) {
        df <- data.frame(get("input", fit@data@env), get("response", fit@data@env))
        type <- class(df[, ncol(df)])
        y <- colnames(df[, ncol(df)])
        pkg <- "party"
    } else if (any(class(fit) == "randomForest")) {
        assert_that(noNA(df))
        type <- attr(fit$terms, "dataClasses")[1]
        y <- attr(attr(fit$terms, "dataClasses"), "names")[1]
        pkg <- "randomForest"
        if (is.null(type)) {
            type <- ifelse(fit$type == "regression", "numeric", "factor")
            y <- fit$y
        }
    } else if (any(class(fit) == "rfsrc")) {
        y <- fit$yvar.names
        pkg <- "randomForestSRC"
        if (length(y) > 1)
            type <- "survival"
        else
            type <- class(df[, y])
    }
    
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))

    `%op%` <- if (foreach::getDoParWorkers() > 1) `%dopar%` else `%do%`

    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = pkg) %op% {
        df[, var] <- rng[i, 1:ncol(rng)]
        if (type == "numeric") {
            if (any(class(fit) == "rfsrc"))
                pred <- predict(fit, newdata = df, outcome = "train")$predicted
            else
                pred <- predict(fit, newdata = df)
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else if (type == "survival") {
            if (any(class(fit) == "rfsrc"))
                pred <- predict(fit, newdata = df, outcome = "train")$predicted
            ## not messing with party survival for now
            ## else {
            ##     pred <- predict(fit, type = "prob")
            ##     df[, ncol(df)] <- get("response", fit@data@env)[[1]][, 1]
            ##     pred <- sapply(weights(fit), function(w) median(df[, ncol(df)][rep(1:nrow(df), w)]))
            ## }
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else if (type == "factor") {
            if (any(class(fit) == "rfsrc"))
                pred <- table(predict(fit, newdata = df, outcome = "train")$class)
            else
                pred <- table(predict(fit, newdata = df))
            pred <- names(pred)[pred == max(pred)]
            if (length(pred) > 1)
                pred <- sample(pred, 1) ## guess if class proportions are all equal
            c(rng[i, 1:ncol(rng)], pred)
        }
    }

    if (length(var) == 1)
        pred <- as.data.frame(do.call("rbind", pred))
    else
        pred <- as.data.frame(do.call("rbind", lapply(pred, unlist)))
    colnames(pred)[1:length(var)] <- var
    colnames(pred)[ncol(pred)] <- "pred"
    pred
}
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
