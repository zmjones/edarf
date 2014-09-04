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
#' @param fit an object of class 'RandomForest-class' returned from \code{cforest}, or an object
#' of class 'randomForest' returned from \code{randomForest}
#' @param df the dataframe used to fit the model, if the model is a party object of class 'RandomForest'
#' this option can be omitted and the dataframe will be extracted from the object
#' @param var a character vector of the predictors of interest, which must match the input
#' matrix in the call to \code{cforest}, \code{randomForest}, or \code{randomForestSRC}
#' @param surv logical, indicates whether or not the response is right-censored
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
#' data(iris)

#' fit_rf <- randomForest(Species ~ ., iris)
#' fit_pt <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
#' fit_rfsrc <- rfsrc(Species ~ ., iris)
#' 
#' pd_rf <- partial_dependence(fit_rf, iris, "Petal.Width", detectCores())
#' pd_pt <- partial_dependence(fit_pt, iris, "Petal.Width", detectCores())
#' pd_rfsrc <- partial_dependence(fit_rfsrc, iris, "Petal.Width", detectCores())
#' 
#' pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"), detectCores())
#' pd_int_pt <- partial_dependence(fit_pt, iris, c("Petal.Width", "Sepal.Length"), detectCores())
#' pd_int_rfsrc <- partial_dependence(fit_rfsrc, iris, c("Petal.Width", "Sepal.Length"), detectCores())
#'
#' @export
partial_dependence <- function(fit, df, var, y, surv = FALSE, cores = 1, ...) {
    if (any(class(fit) == "RandomForest"))
        df <- data.frame(get("input", fit@data@env), get("response", fit@data@env))
    rng <- lapply(var, function(x) ivar_points(df, x))
    rng <- expand.grid(rng)
    pred <- mclapply(1:nrow(rng), function(i) {
        df[, var] <- rng[i, 1:ncol(rng)]
        if (is.numeric(df[, y]) & surv == FALSE) {
            if (any(class(fit) == "rfsrc"))
                pred <- predict(fit, newdata = df, outcome = "test")$predicted.oob
            else
                pred <- predict(fit, newdata = df)
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else if (surv == TRUE) {
            pred <- predict(fit, type = "prob")
            df[, ncol(df)] <- get("response", fit@data@env)[[1]][, 1]
            pred <- sapply(weights(fit), function(w) median(df[, ncol(df)][rep(1:nrow(df), w)]))
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else {
            if (any(class(fit) == "rfsrc"))
                pred <- table(predict(fit, newdata = df, outcome = "test")$class.oob)
            else
                pred <- table(predict(fit, newdata = df))
            c(rng[i, 1:ncol(rng)], names(pred)[pred == max(pred)])
        }
    }, mc.cores = cores)
    pred <- as.data.frame(do.call("rbind", pred))
    colnames(pred)[1:length(var)] <- var
    colnames(pred)[ncol(pred)] <- "pred"
    return(pred)
}
