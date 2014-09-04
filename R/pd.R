#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the Party, randomForest, or randomForestSRC packages
#'
#' @author Zachary M. Jones, \email{zmj@@zmjones.com}
#'
#' @importFrom parallel mclapply
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
#' @param ... arguments to be passed to 'ivar_points'
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`
#'
#' @examples
#' require(randomForest)
#' require(party)
#' require(randomForestSRC)
#' require(parallel)
#' CORES <- detectCores()
#'
#' ## Classification
#' 
#' data(iris)
#' 
#' fit_rf <- randomForest(Species ~ ., iris)
#' fit_pt <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Species ~ ., iris)
#'
#' pd_rf <- partial_dependence(fit_rf, iris, "Petal.Width", CORES)
#' pd_pt <- partial_dependence(fit_pt, iris, "Petal.Width", CORES)
#' pd_rfsrc <- partial_dependence(fit_rfsrc, iris, "Petal.Width", CORES)
#'
#' pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"), CORES)
#' pd_int_pt <- partial_dependence(fit_pt, iris, c("Petal.Width", "Sepal.Length"), CORES)
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, iris, c("Petal.Width", "Sepal.Length"), CORES)
#'
#' ## Regression
#'
#' data(swiss)
#'
#' fit_rf <- randomForest(Fertility ~ ., swiss)
#' fit_pt <- cforest(Fertility ~ ., swiss, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Fertility ~ ., swiss)
#'
#' pd_rf <- partial_dependence(fit_rf, swiss, "Education", CORES)
#' pd_pt <- partial_dependence(fit_pt, swiss, "Education", CORES)
#' pd_rfsrc <- partial_dependence(fit_rfsrc, swiss, "Education", CORES)
#'
#' pd_int_rf <- partial_dependence(fit_rf, swiss, c("Education", "Catholic"), CORES)
#' pd_int_pt <- partial_dependence(fit_pt, swiss, c("Education", "Catholic"), CORES)
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, swiss, c("Education", "Catholic"), CORES)
#'
#'
#' ## Survival
#'
#' data(veteran, package = "randomForestSRC")
#'
#' fit_rfsrc <- rfsrc(Surv(time, status) ~ ., veteran)
#'
#' pd_rfsrc <- partial_dependence(fit_rfsrc, veteran, "age", CORES)
#'
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, veteran, c("age", "diagtime"), CORES)
#' 
#' @export
partial_dependence <- function(fit, df, var, cores = 1, ...) {
    if (any(class(fit) == "RandomForest")) {
        df <- data.frame(get("input", fit@data@env), get("response", fit@data@env))
        type <- class(df[, ncol(df)])
        y <- colnames(df[, ncol(df)])
    }
    if (any(class(fit) == "randomForest"))  {
        type <- attr(fit$terms, "dataClasses")[1]
        y <- attr(attr(fit$terms, "dataClasses"), "names")[1]
    }
    if (any(class(fit) == "rfsrc")) {
        y <- fit$yvar.names
        if (length(y) > 1)
            type <- "survival"
        else
            type <- class(df[ ,y])
    }
    
    rng <- lapply(var, function(x) ivar_points(df, x))
    rng <- expand.grid(rng)
    pred <- mclapply(1:nrow(rng), function(i) {
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
    }, mc.cores = cores)
    pred <- as.data.frame(do.call("rbind", pred))
    colnames(pred)[1:length(var)] <- var
    colnames(pred)[ncol(pred)] <- "pred"
    return(pred)
}
