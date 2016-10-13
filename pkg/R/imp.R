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
#' @param interaction logcal, compute the joint and additive importance for observations (\code{type = "local"}) or variables \code{type = "aggregate"}
#' @param oob logical, use the out of bag data data
#' @param nperm positive integer giving the number of times to permute the indicated variables (default 10)
#' @param parallel logical whether to run in parallel using the registered backend (default FALSE)
#' @param data optional (unless using randomForest) data.frame with which to calculate importance
#'
#' @return a named numeric vector for type = "aggregate" or a data.frame for type = "local"
#'
#' @examples
#' library(randomForest)
#' data(iris)
#'
#' fit <- randomForest(Species ~ ., iris)
#' variable_importance(fit, var = colnames(iris)[-5], type = "aggregate", nperm = 2, data = iris)
#'
#' library(party)
#' data(swiss)
#' fit <- cforest(Fertility ~ ., swiss)
#' variable_importance(fit, var = colnames(swiss)[-1], type = "local", nperm = 2)
#'
#' data(mtcars)
#' library(randomForestSRC)
#' fit <- rfsrc(mpg ~ ., mtcars)
#' variable_importance(fit, var = colnames(mtcars)[-1], type = "aggregate", nperm = 2, oob = TRUE)
#' @export
variable_importance <- function(fit, var, type, interaction, oob, nperm, parallel, data)
  UseMethod("variable_importance")
#' @export
variable_importance.randomForest <- function(fit, var, type = "aggregate", interaction = FALSE,
                                             oob = TRUE, nperm = 100, parallel = FALSE,
                                             data = NULL) {
  out <- .variable_importance(fit, var, type, interaction, nperm, parallel, data,
                              y = fit$y, list(object = fit, type = "response", OOB = oob),
                              "randomForest")
  return(out)
}
#' @export
variable_importance.RandomForest <- function(fit, var, type = "aggregate", interaction = FALSE,
                                             oob = TRUE, nperm = 100, parallel = FALSE, data = NULL) {
  out <- .variable_importance(fit, var, type, interaction, nperm, parallel,
                              get("input", fit@data@env),
                              get("response", fit@data@env)[, 1],
                              list(object = fit, type = "response", OOB = oob),
                              "party")
  return(out)
}
#' @export
variable_importance.rfsrc <- function(fit, var, type = "aggregate", interaction = FALSE,
                                      oob = TRUE, nperm = 100, parallel = FALSE, data = NULL) {
  out <- .variable_importance(fit, var, type, interaction, nperm, parallel,
                              fit$xvar, fit$yvar, list(object = fit), "randomForestSRC")
  return(out)
}

.permute_data <- function(data, x) {
  pdata <- data
  n <- nrow(data)
  pidx <- sapply(x, function(z) sample(1:n, size = n, replace = FALSE))
  if (length(x) == 1) {
    pidx <- pidx[, 1]
    pdata[, x] <- pdata[pidx, x]
  } else {
    for (i in 1:length(x))
      pdata[, x[i]] <- pdata[pidx[, i], x[i]]
  }
  return(pdata)
}

.variable_importance <- function(fit, var, type, interaction = FALSE, nperm, parallel,
                                 data, y, predict_options, pkg, oob = NULL) {
  '%op%' <- ifelse(getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
  ensemble_pred <- do.call("predict", c(predict_options, list(newdata = data)))

  x <- NULL ## initialize global variables

  if (pkg == "randomForestSRC") ensemble_pred <- ensemble_pred$predicted

  inner_loop <- function(data, x, predict_options, y, ensemble_output) {
    pdata <- .permute_data(data, x)
    p <- do.call("predict", c(predict_options, list(newdata = pdata)))
    if (pkg == "randomForestSRC") p <- p$predicted
    loss(p, y) - ensemble_output
  }

  if (type == "local") {
    if (is.factor(y) & !is.ordered(y)) loss <- function(x, y) ifelse(x != y, 1, 0)
    else if (is.ordered(y)) loss <- function(x, y) as.integer(x) - as.integer(y)
    else loss <- function(x, y) x - y
    ensemble_resid <- loss(ensemble_pred, y)
    comb <- function(...) rowMeans(do.call("cbind", list(...)))

    out <- foreach(x = var, .combine = "cbind", .packages = pkg) %:%
      foreach(icount(nperm), .combine = comb, .packages = pkg) %op%
      inner_loop(data, x, predict_options, y, ensemble_resid)
    out <- as.data.frame(out)
    colnames(out) <- var
    if (interaction) {
      int <- foreach(icount(nperm), .combine = comb, .packages = pkg) %op%
        inner_loop(data, var, predict_options, y, ensemble_resid)
      out$additive <- rowSums(out[, var])
      out$joint <- int
    }
  } else if (type == "aggregate") {
    if (is.factor(y) & !is.ordered(y)) loss <- function(x, y) mean(x != y)
    else loss <- function(x, y) mean((x - y)^2)
    ensemble_loss <- loss(ensemble_pred, y)
    comb <- function(...) mean(do.call("c", list(...)))

    out <- foreach(x = var, .combine = "c", .packages = pkg) %:%
      foreach(icount(nperm), .combine = comb, .packages = pkg) %op% {
        inner_loop(data, x, predict_options, y, ensemble_loss)
      }
    names(out) <- var
    if (interaction) {
      int <- foreach(icount(nperm), .combine = comb) %op% {
        inner_loop(data, var, predict_options, y, ensemble_loss)
      }
      additive <- sum(out)
      out <- c(out, additive, int)
      names(out) <- c(var, "additive", "joint")
    }
  } else {
    stop("unsupported type argument")
  }

  attr(out, "class") <- c("importance", ifelse(type == "aggregate", "numeric", "data.frame"))
  attr(out, "type") <- type
  attr(out, "var") <- var
  attr(out, "oob") <- oob
  attr(out, "interaction") <- interaction
  attr(out, "target") <- y
  out
}
