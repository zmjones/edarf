#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the party, randomForest, or randomForestSRC packages
#'
#' @importFrom foreach foreach %dopar% %do% %:% getDoParWorkers
#' @importFrom plyr ldply
#' @importFrom stats predict
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
#' @param parallel logical indicator of whether a parallel backend should be used if registered
#' @param clean_names logical indicator of whether to clean factor names in output i.e. "level" instead of "factorname.level."
#' @return a data.frame with the partial dependence of 'var'
#' if 'var' has length = 1 then the output will be a data.frame with a column for the predicted value at each value of 'var', averaged over the values of all other predictors.
#' if 'var' has length > 1 and interaction is true or false then the output will be a data.frame with a column for each element of 'var' and the predicted value for each combination.
#'
#' @examples
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
#' @export
partial_dependence <- function(fit, data, var, cutoff = 10L, interaction = FALSE, oob = TRUE,
                               ci = FALSE, confidence = .95, parallel = FALSE,
                               clean_names = TRUE) UseMethod("partial_dependence", fit)
#' @export
partial_dependence.randomForest <- function(fit, data, var, cutoff = 10L, interaction = FALSE,
                                            oob = TRUE, ci = FALSE, confidence = .95,
                                            parallel = FALSE, clean_names = TRUE) {
  pkg <- "randomForest"
  ## find the target feature in the data.frame
  check <- lapply(1:ncol(data), function(x)
    all.equal(data[[x]], fit$y, use.names = FALSE, check.names = FALSE))
  check <- sapply(check, function(x) any(is.logical(x)))
  if (all(check == FALSE)) {
    data$target <- fit$y
    target <- "target"
  }
  target <- colnames(data)[check]
  
  if (class(data[[target]]) == "factor")  {
    predict_options <- list(object = fit, OOB = oob, type = "prob")
  } else if (class(data[[target]]) %in% c("numeric", "integer")) {
    predict_options <- list(object = fit, type = "response")
  } else {
    stop("invalid target type or unknown error")
  }

  .partial_dependence(data, target, var, cutoff, interaction,
                      ci, confidence, parallel, predict_options, pkg, clean_names)
}
#' @export
partial_dependence.RandomForest <- function(fit, data = NULL, var, cutoff = 10L, interaction = FALSE,
                                            oob = TRUE, ci = FALSE, confidence = .95,
                                            parallel = FALSE, clean_names = TRUE) {
  pkg <- "party"
  y <- get("response", fit@data@env)
  data <- data.frame(get("input", fit@data@env), y)
  target <- names(y)
  if (ncol(y) == 1) y <- y[[target]]
  
  if (!is.data.frame(y)) {
    if (class(data[[target]]) == "factor")  {
      predict_options <- list(object = fit, OOB = oob, type = "prob")
    } else if (class(data[[target]]) %in% c("numeric", "integer")) {
      predict_options <- list(object = fit, type = "response")
    } else {
      stop("invalid target type or unknown error")
    }
  } else {
    predict_options <- list(object = fit, type = "response")
  }

  .partial_dependence(data, target, var, cutoff, interaction,
                      ci, confidence, parallel, predict_options, pkg, clean_names)
}
#' @export
partial_dependence.rfsrc <- function(fit, data = NULL, var, cutoff = 10L, interaction = FALSE,
                                     oob = TRUE, ci = FALSE, confidence = .95,
                                     parallel = FALSE, clean_names = TRUE) {
  pkg <- "randomForestSRC"
  target <- fit$yvar.names
  data <- data.frame(fit$xvar, fit$yvar) ## rfsrc casts integers to numerics
  colnames(data)[ncol(data)] <- target

  if (class(data[[target]]) == "factor")  {
    predict_options <- list(object = fit, type = "prob")
  } else if (class(data[[target]]) %in% c("numeric", "integer")) {
    predict_options <- list(object = fit, type = "response")
  } else {
    stop("invalid target type or unknown error")
  }

  .partial_dependence(data, target, var, cutoff, interaction, ci, confidence,
                      parallel, predict_options, pkg, clean_names)
}

.partial_dependence <- function(data, target, var, cutoff, interaction,
                                ci, confidence, parallel, predict_options, pkg, clean_names) {
  if (length(target) == 1)
    y <- data[[target]]
  else
    y <- data[, target]
  if (class(y) == "factor")
    target <- levels(y)
  if (!(class(y) %in% c("integer", "numeric")))
    ci <- FALSE
  if (length(var) == 1)
    interaction <- FALSE
  if (class(y) == "data.frame")
    target <- unname(unlist(sapply(target, function(y) if (is.factor(data[[y]])) levels(data[[y]]) else y)))
  
  rng <- vector("list", length(var))
  names(rng) <- var
  for (i in 1:length(var))
    rng[[i]] <- .ivar_points(var[i], data,
                             fmin = ifelse(!is.factor(data[[var[i]]]), min(data[[var[i]]], na.rm = TRUE), NA),
                             fmax = ifelse(!is.factor(data[[var[i]]]), max(data[[var[i]]], na.rm = TRUE), NA),
                             cutoff = cutoff)
  rng <- as.data.frame(rng)
  if (length(var) > 1L & interaction)
    rng <- expand.grid(rng)
  rng <- rng[!duplicated(rng), , drop = FALSE]
  
  ## check to see if parallel backend registered
  '%op%' <- ifelse(getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
  comb <- function(...) do.call("rbind", list(...))
  
  if (length(var) > 1 & !interaction) {
    out <- foreach(x = var) %:% foreach(i = seq_len(nrow(rng)), .combine = "rbind") %op%
    .inner_loop(data, y, rng[, x, drop = FALSE], i, x, ci, confidence,
                predict_options, pkg, clean_names)
    names(out) <- var
    out <- ldply(out)
    for (x in var) {
      idx <- which(out$.id %in% x)
      out[idx, x] <- rng[[x]]
      out[!idx, x] <- NA
    }
    out$.id <- NULL
    
    if (ci)
      colnames(out) <- c("lower", target, "upper", var)
    else
      colnames(out) <- c(target, var)
  } else {
    pred <- foreach(i = seq_len(nrow(rng)), .combine = comb) %op%
    .inner_loop(data, y, rng, i, var, ci, confidence, predict_options, pkg, clean_names)

    if (ci)
      colnames(pred) <- c("lower", target, "upper")
    if (!is.data.frame(y) & !ci)
      colnames(pred) <- target

    out <- cbind(pred, rng)
  }
  
  attr(out, "class") <- c("pd", "data.frame")
  attr(out, "target") <- target
  attr(out, "prob") <- class(y) == "factor"
  attr(out, "interaction") <- length(var) > 1 & interaction
  attr(out, "multivariate") <- class(y) == "data.frame"
  attr(out, "var") <- var
  attr(out, "ci") <- ci
  out
}

.ivar_points <- function(x, data,
                         fmin = ifelse(!is.factor(data[[x]]), min(data[[x]], na.rm = TRUE), NA),
                         fmax = ifelse(!is.factor(data[[x]]), max(data[[x]], na.rm = TRUE), NA),
                         cutoff = 10) {
  if (is.factor(data[[x]])) {
    factor(rep(levels(data[[x]]), length.out = cutoff),
           levels = levels(data[[x]]), ordered = is.ordered(data[[x]]))
  } else {
    if (is.integer(data[[x]]))
      as.integer(seq(fmin, fmax, length.out = cutoff))
    else
      seq(fmin, fmax, length.out = cutoff)
  }
}

.inner_loop <- function(data, y, rng, idx, var, ci, confidence, predict_options, pkg, clean_names) {
  data[, var] <- rng[idx, ]
  if (class(y) %in% c("numeric", "integer")) {
    if (ci)
      pred <- colMeans(var_est(predict_options$object, data))
    else {
      pred <- do.call("predict", c(predict_options, list(newdata = data)))
      if (pkg == "randomForestSRC")
        pred <- pred$predicted
      pred <- mean(pred)
    }
  } else if (class(y) == "factor") {
    ## if y is a factor and we want class probs return the mean prob across
    ## obs. for each observation. predict.cforest returns a list, which is row binded
    ## and then the means are computed, i also remove the name of the factor (y)
    ## and just use the level labels as the column names (same as randomForest)
    pred <- do.call("predict", c(predict_options, list(newdata = data)))
    if (pkg == "randomForestSRC")
      pred <- pred$predicted
    if (is.list(pred))
      pred <- do.call("rbind", pred)
    pred <- colMeans(pred)
    if (!all(names(pred) == levels(y)) & clean_names)
      names(pred) <- gsub("^.*\\.", "", names(pred))
  } else {
    pred <- do.call("predict", c(predict_options, list(newdata = data)))
    pred <- colMeans(do.call("rbind", pred))
    if (clean_names) {
      facts <- names(y)[sapply(data[ names(y)], class) %in% c("character", "factor")]
      matched <- grepl(paste("^", facts, "*.", sep = "", collapse = "|"), names(pred))
      pattern <- paste(paste0("^", facts, "\\."), collapse = "|")
      names(pred)[matched] <- gsub(pattern, "", names(pred)[matched])
    }
  }
  out <- unlist(pred)
  if (ci) {
    cl <- qnorm((1 - confidence) / 2, lower.tail = FALSE)
    se <- unname(sqrt(out["variance"]))
    out["low"]<- out["prediction"] - cl * se
    out["high"] <- out["prediction"] + cl * se
    out <- out[c("low", "prediction", "high")]
  }
  out
}
