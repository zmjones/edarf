#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the party, randomForest, or randomForestSRC packages
#'
#' @importFrom foreach foreach %dopar% %do%
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
#' pd <- partial_dependence(fit, iris, "Petal.Width")
#' pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"))
#'
#' ## Regression
#'
#' data(swiss)
#'
#' fit <- randomForest(Fertility ~ ., swiss)
#' pd <- partial_dependence(fit, swiss, "Education")
#' pd_int <- partial_dependence(fit, swiss, c("Education", "Catholic"))
#' }
#' @export
partial_dependence.randomForest <- function(fit, df, var, cutoff = 10, bootstrap = FALSE, bootstrap_iter = 100,
                                            empirical = TRUE, parallel = FALSE, type = "") {
    y_class <- attr(fit$terms, "dataClasses")[1]
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    if (bootstrap) {
        idx <- lapply(1:bootstrap_iter, function(x) sample(1:fit$ntree, fit$ntree, TRUE))
        out_rng <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE) %op% {
            df[, var] <- rng[i, ]
            pred <- predict(fit, newdata = df, predict.all = TRUE)$individual
            out_bs <- foreach::foreach(j = 1:bootstrap_iter, .inorder = FALSE) %op% {
                if (y_class %in% c("numeric", "integer")) {
                    out <- mean(apply(pred[, idx[[j]]], function(x) mean(x[which(x != 0)])))
                    c(rng[i, ], out)
                } else if (y_class == "factor") {
                    out <- t(apply(pred[, idx[[j]]], 1, function(x) {
                        tab <- table(factor(x, levels = levels(df[, names(y_class)])))
                        tab / sum(tab)
                    }))
                    c(rng[i, ], colMeans(out))
                } else stop("invalid response type")
            }
            out_bs <- as.data.frame(do.call(rbind, out_bs))
            if (y_class %in% c("numeric", "integer")) {
                colnames(out_bs)[1:length(var)] <- var
                colnames(out_bs)[(length(var) + 1):ncol(out_bs)] <- names(y_class)
                out_bs$point <- as.factor(1:nrow(out_bs))
                out_bs
            } else if (y_class == "factor") {
                colnames(out_bs)[1:length(var)] <- var
                colnames(out_bs)[(length(var) + 1):ncol(out_bs)] <- levels(df[, names(y_class)])
                out_bs <- reshape2::melt(out_bs, id.vars = 1:length(var))
                colnames(out_bs)[(length(var) + 1):ncol(out_bs)] <- c("level", "prediction")
                out_bs$point <- as.factor(1:nrow(out_bs))
                out_bs
            } else stop("invalid response type")
        }
        pred <- as.data.frame(do.call(rbind, out_rng))
    } else {
        pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "randomForest") %op% {
            df[, var] <- rng[i, ]
            if (y_class == "numeric" | y_class == "integer") {
                pred <- predict(fit, newdata = df)
                pred <- mean(pred)
            } else if (y_class == "factor") {
                if (type == "prob") {
                    pred <- predict(fit, newdata = df, type = type)
                    pred <- colMeans(pred)
                } else if (type == "class" | type == "") {
                    pred <- table(predict(fit, newdata = df))
                    pred <- names(pred)[pred == max(pred)]
                    if (length(pred) != 1) pred <- sample(pred, 1)
                } else stop("invalid type parameter passed to predict.randomForest*")
            } else stop("invalid response type")
            c(rng[i, ], pred)
        }
        if (length(var) > 1)
            pred <- as.data.frame(do.call(rbind, lapply(pred, unlist)), stringsAsFactors = FALSE)
        else pred <- as.data.frame(do.call(rbind, pred), stringsAsFactors = FALSE)
        colnames(pred)[1:length(var)] <- var
        if (type != "prob") {
            colnames(pred)[ncol(pred)] <- names(y_class)
            pred <- fix_classes(colnames(pred), df, pred)
        }
    }
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "prob") <- type == "prob"
    attr(pred, "interaction") <- length(var) > 1
    attr(pred, "multivariate") <- FALSE
    attr(pred, "bootstrap") <- bootstrap
    attr(pred, "var") <- var
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
#' fit <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
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
partial_dependence.RandomForest <- function(fit, var, cutoff = 10, bootstrap = FALSE, bootstrap_iter = 100,
                                            empirical = TRUE, parallel = FALSE, type = "") {
    y <- get("response", fit@data@env)
    if (dim(y)[2] != 1 & bootstrap)
        stop("bootstrapping the ensemble is not supported for multivariate responses (yet)")
    df <- data.frame(get("input", fit@data@env), y)
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    get_new_weights <- function(fit, df, idx, mincriterion = 0, OOB = FALSE) {
        .Call("R_predictRF_weights",
              fit@ensemble[idx], fit@where[idx], fit@weights[idx],
              party:::newinputs(fit, df),
              mincriterion, OOB && is.null(df), PACKAGE = "party")
    }
    if (bootstrap) {
        idx <- lapply(1:bootstrap_iter, function(x) sample(seq_along(fit@ensemble), length(fit@ensemble), TRUE))
        out_rng <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "party") %op% {
            df[, var] <- rng[i, ]
            out_bs <- foreach::foreach(j = 1:bootstrap_iter, .inorder = FALSE) %op% {
                pw <- get_new_weights(fit, df, idx[[j]])
                out <- sapply(pw, function(w) w %*% fit@responses@predict_trafo / sum(w))
                if (class(y[, 1]) %in% c("numeric", "integer"))
                    c(rng[i, ], mean(out))
                else if (class(y[, 1]) == "factor")
                    c(rng[i, ], rowMeans(out))
                else stop("invalid response type")
            }
            if (class(y[, 1]) %in% c("numeric", "integer")) {
                out_bs <- as.data.frame(do.call(rbind, out_bs))
                colnames(out_bs)[1:length(var)] <- var
                colnames(out_bs)[(length(var) + 1):ncol(out_bs)] <- names(y[, 1])
                out_bs$point <- as.factor(1:nrow(out_bs))
                out_bs
            } else if (class(y[, 1]) == "factor") {
                out_bs <- as.data.frame(do.call(rbind, out_bs))
                colnames(out_bs)[1:length(var)] <- var
                colnames(out_bs)[(length(var) + 1):ncol(out_bs)] <- levels(y[, 1])
                out_bs <- reshape2::melt(out_bs, id.vars = 1:length(var))
                colnames(out_bs)[(length(var) + 1):ncol(out_bs)] <- c("level", "prediction")
                out_bs$point <- as.factor(1:nrow(out_bs))
                out_bs
            } else stop("invalid response type")
        }
        pred <- as.data.frame(do.call(rbind, out_rng))
    } else {
        pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "party") %op% {
            df[, var] <- rng[i, ]
            if (dim(y)[2] == 1) {
                if (class(y[, 1]) == "numeric" | class(y[, 1]) == "integer") {
                    pred <- predict(fit, newdata = df)
                    pred <- mean(pred)
                } else if (class(y[, 1]) == "factor") {
                    if (type == "prob") {
                        pred <- colMeans(do.call(rbind, predict(fit, newdata = df, type = type)))
                        names(pred) <- gsub(paste0(names(y), "\\."), "", names(pred))
                    } else if (type == "class" | type == "") {
                        pred <- table(predict(fit, newdata = df))
                        pred <- names(pred)[pred == max(pred)]
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
        if (type != "prob") {
            colnames(pred)[(length(var) + 1):ncol(pred)] <- colnames(y)
            pred <- fix_classes(c(var, colnames(y)), df, pred)
        }
    }
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "prob") <- type == "prob"
    attr(pred, "interaction") <- length(var) > 1
    attr(pred, "multivariate") <- dim(y)[2] != 1
    attr(pred, "var") <- var
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
partial_dependence.rfsrc <- function(fit, var, cutoff = 10, empirical = TRUE, parallel = FALSE, type = "") {
    y <- fit$yvar
    df <- data.frame(fit$xvar, y)
    if (!is.data.frame(y))
        colnames(df)[ncol(df)] <- fit$yvar.names
    rng <- expand.grid(lapply(var, function(x) ivar_points(df, x, cutoff, empirical)))
    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    pred <- foreach::foreach(i = 1:nrow(rng), .inorder = FALSE, .packages = "randomForestSRC") %op% {
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
        pred[, -ncol(pred)] <- fix_classes(var, df, pred[, -ncol(pred)])
    } else if (type != "prob"){
        colnames(pred)[ncol(pred)] <- fit$yvar.names
        pred <- fix_classes(c(var, fit$yvar.names), df, pred)
    }
    attr(pred, "class") <- c("pd", "data.frame")
    attr(pred, "prob") <- type == "prob"
    attr(pred, "interaction") <- length(var) > 1
    attr(pred, "multivariate") <- FALSE
    attr(pred, "var") <- var
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
