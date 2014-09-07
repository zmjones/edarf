#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the Party, randomForest, or randomForestSRC packages
#'
#' @author Zachary M. Jones, \email{zmj@@zmjones.com}
#'
#' @importFrom parallel mclapply parLapply makePSOCKcluster
#' @importFrom party cforest cforest_control
#' @importFrom randomForest randomForest
#' @importFrom randomForestSRC rfsrc
#' 
#' @param fit an object of class 'RandomForest-class' returned from \code{cforest}, an object
#' of class 'randomForest' returned from \code{randomForest}, or an object of class 'rfsrc'
#' returned from \code{rfsrc}
#' @param df the dataframe used to fit the model, if the model is a party object of class 'RandomForest'
#' this option can be omitted and the dataframe will be extracted from the object
#' @param var a character vector of the predictors of interest, which must match the input
#' matrix in the call to \code{cforest}, \code{randomForest}, or \code{randomForestSRC}
#' @param cores indicates the number of cores to use. parallelization occurs in the prediction
#' on the grid of possible values taken by all combinations of `var`
#' @param cutoff the maximal number of unique points in each element of 'var' used in the
#' partial dependence calculation
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`
#'
#' @examples
#' require(randomForest)
#' require(party)
#' require(randomForestSRC)
#'
#' ## Classification
#' 
#' data(iris)
#' 
#' fit_rf <- randomForest(Species ~ ., iris)
#' fit_pt <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Species ~ ., iris)
#'
#' pd_rf <- partial_dependence(fit_rf, iris, "Petal.Width", 1)
#' pd_pt <- partial_dependence(fit_pt, iris, "Petal.Width", 1)
#' pd_rfsrc <- partial_dependence(fit_rfsrc, iris, "Petal.Width", 1)
#'
#' pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"), 1)
#' pd_int_pt <- partial_dependence(fit_pt, iris, c("Petal.Width", "Sepal.Length"), 1)
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, iris, c("Petal.Width", "Sepal.Length"), 1)
#'
#' ## Regression
#'
#' data(swiss)
#'
#' fit_rf <- randomForest(Fertility ~ ., swiss)
#' fit_pt <- cforest(Fertility ~ ., swiss, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Fertility ~ ., swiss)
#'
#' pd_rf <- partial_dependence(fit_rf, swiss, "Education", 1)
#' pd_pt <- partial_dependence(fit_pt, swiss, "Education", 1)
#' pd_rfsrc <- partial_dependence(fit_rfsrc, swiss, "Education", 1)
#'
#' pd_int_rf <- partial_dependence(fit_rf, swiss, c("Education", "Catholic"), 1)
#' pd_int_pt <- partial_dependence(fit_pt, swiss, c("Education", "Catholic"), 1)
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, swiss, c("Education", "Catholic"), 1)
#'
#'
#' ## Survival
#'
#' data(veteran, package = "randomForestSRC")
#'
#' fit_rfsrc <- rfsrc(Surv(time, status) ~ ., veteran)
#'
#' pd_rfsrc <- partial_dependence(fit_rfsrc, veteran, "age", 1)
#'
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, veteran, c("age", "diagtime"), 1)
#' 
#' @export
partial_dependence <- function(fit, df, var, cores = 1, cutoff = 10) {
    if (any(class(fit) == "RandomForest")) {
        df <- data.frame(get("input", fit@data@env), get("response", fit@data@env))
        type <- class(df[, ncol(df)])
        y <- colnames(df[, ncol(df)])
    }
    else if (any(class(fit) == "randomForest")) {
        type <- attr(fit$terms, "dataClasses")[1]
        y <- attr(attr(fit$terms, "dataClasses"), "names")[1]
    } else if (any(class(fit) == "rfsrc")) {
        y <- fit$yvar.names
        if (length(y) > 1)
            type <- "survival"
        else
            type <- class(df[ ,y])
    } else stop("Unsupported fit object class")
    
    rng <- lapply(var, function(x) ivar_points(df, x, cutoff))
    rng <- expand.grid(rng)

    if (cores == 1)
        pred <- lapply(1:nrow(rng), function(i) pd_inner(fit, df, var, rng, type, i))
    else if (.Platform$OS.type == "windows") {
        cl <- parallel::makePSOCKcluster(cores)
        parallel::clusterEvalQ(cl, library(edarf))
        parallel::clusterExport(cl, c("pd_inner","mclapply"))
        pred <- parallel::parLapply(cl, 1:nrow(rng), function(i) pd_inner(fit, df, var, rng, type, i))
        stopCluster(cl)
    } else
        pred <- parallel::mclapply(1:nrow(rng), function(i) pd_inner(fit, df, var, rng, type, i), mc.cores = cores)
    
    pred <- as.data.frame(do.call("rbind", pred))
    colnames(pred)[1:length(var)] <- var
    colnames(pred)[ncol(pred)] <- "pred"
    return(pred)
}
#' Calculates the partial dependence of 'var' on the response for a particular value of 'var'
#'
#' Calculates the fitted values of observations with one or more variables set to a particular value
#' and returns a summary of these fitted values: means for regression and survival analysis and
#' the modal category for classification,
#' 
#' @param fit an object of class 'RandomForest-class' returned from \code{cforest}, an object
#' of class 'randomForest' returned from \code{randomForest}, or an object of class 'rfsrc'
#' returned from \code{rfsrc}
#' @param df the dataframe used to fit the model, if the model is a party object of class 'RandomForest'
#' this option can be omitted and the dataframe will be extracted from the object
#' @param var a character vector of the predictors of interest, which must match the input
#' matrix in the call to \code{cforest}, \code{randomForest}, or \code{randomForestSRC}
#' @param rng a dataframe with columns equal to the number of variables in 'var' and rows less
#' than or equal to 'cutoff' as defined in the call to 'ivar_points'
#' @param type the class of the dependent variable, generated by 'partial_dependence.'
#' can equal "numeric", "factor," or "survival"
#' @param i the index position in 'rng' for which the variables in 'var' are set
#'
#' @return a vector with the values the variables specified by 'var' are set at and the summary fitted value
#' 
#' @export
pd_inner <- function(fit, df, var, rng, type, i) {
        df[, var] <- rng[i, 1:ncol(rng)]
        if (type == "numeric") {
            if (any(class(fit) == "rfsrc"))
                pred <- predict(fit, newdata = df, outcome = "test")$predicted.oob
            else
                pred <- predict(fit, newdata = df)
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else if (type == "survival") {
            if (any(class(fit) == "rfsrc"))
                pred <- predict(fit, newdata = df, outcome = "test")$predicted.oob
            ## not messing with party survival for now
            ## else {
            ##     pred <- predict(fit, type = "prob")
            ##     df[, ncol(df)] <- get("response", fit@data@env)[[1]][, 1]
            ##     pred <- sapply(weights(fit), function(w) median(df[, ncol(df)][rep(1:nrow(df), w)]))
            ## }
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else if (type == "factor") {
            if (any(class(fit) == "rfsrc"))
                pred <- table(predict(fit, newdata = df, outcome = "test")$class.oob)
            else
                pred <- table(predict(fit, newdata = df))
            pred <- names(pred)[pred == max(pred)]
            if (length(pred) > 1)
                pred <- sample(pred, 1) ## guess if class proportions are all equal
            c(rng[i, 1:ncol(rng)], pred)
        }
}
#'
#' Creates a prediction vector for variables to decrease computation time
#'
#' @param df the dataframe used to fit the random forest, extracted from the fitted object
#' @param x a character vector of length 1 indicating the variable in `df` to be extracted
#' @param cutoff an integer indicating the maximal length of the vector to be used for prediction
#' 
#' @return a vector of unique values taken by `x` of length < `cutoff`
#' 
#' @export
ivar_points <- function(df, x, cutoff = 10) {
    rng <- unique(df[, x])
    rng <- rng[!is.na(rng)]
    if (length(rng) > cutoff & !is.factor(df[, x]))
        rng <- seq(min(rng), max(rng), length.out = cutoff)
    class(rng) <- class(df[, x])
    return(rng)
}
