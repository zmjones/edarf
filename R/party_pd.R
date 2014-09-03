#' Partial dependence using Party random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the Party package
#'
#' @author Zachary M. Jones, \email{zmj@@zmjones.com}
#'
#' @importFrom parallel mclapply
#' 
#' @param fit an object of class 'RandomForest-class' returned from \code{cforest}
#' @param var a character vector of the predictors of interest, which must match the input
#' matrix in the call to \code{cforest}
#' @param surv logical, indicates whether or not the response is right-censored
#' @param cores indicates the number of cores to use. parallelization occurs in the prediction
#' on the grid of possible values taken by all combinations of `var`
#'
#' @return a dataframe with columns for each predictor in `var` and the fitted value for
#' each set of values taken by the values of 'var' averaged within the values of predictors
#' in the model but not in `var`
#'
#' @examples
#' data(iris)
#' fit <- cforest(Species ~ ., data = iris, control = cforest_unbiased(mtry = 2))
#' pd <- partial_dependence(fit, "Petal.Width", detectCores())
#' pd_int <- partial_dependence(fit, c("Petal.Width", "Sepal.Length"), detectCores())
#'
#' @export
party_partial_dependence <- function(fit, var, surv = FALSE, cores = 1, ...) {
    df <- data.frame(get("input", fit@data@env), get("response", fit@data@env))
    rng <- lapply(var, function(x) ivar_points(df, x))
    rng <- expand.grid(rng)
    pred <- mclapply(1:nrow(rng), function(i) {
        df[, var] <- rng[i, 1:ncol(rng)]
        if (is.numeric(df[, ncol(df)]) & surv == FALSE) {
            c(rng[i, 1:ncol(rng)], mean(predict(fit, newdata = df)))
        } else if (surv == TRUE) {
            pred <- predict(fit, type = "prob")
            df[, ncol(df)] <- get("response", fit@data@env)[[1]][, 1]
            pred <- sapply(weights(fit), function(w) median(df[, ncol(df)][rep(1:nrow(df), w)]))
            c(rng[i, 1:ncol(rng)], mean(pred))
        } else {
            pred <- table(predict(fit, newdata = df))
            c(rng[i, 1:ncol(rng)], names(pred)[pred == max(pred)])
        }
    }, mc.cores = cores)
    pred <- as.data.frame(do.call("rbind", pred))
    colnames(pred)[1:length(var)] <- var
    colnames(pred)[ncol(pred)] <- "pred"
    return(pred)
}
