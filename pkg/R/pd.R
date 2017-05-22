#' Partial dependence using random forests
#'
#' Calculates the partial dependence of the response on an arbitrary dimensional set of predictors
#' from a fitted random forest object from the party, randomForest, randomForestSRC, or ranger packages
#'
#' @importFrom data.table rbindlist setcolorder
#' @importFrom stats predict
#' @importFrom mmpf marginalPrediction
#' 
#' @param fit object of class 'RandomForest', 'randomForest', 'rfsrc', or `ranger`
#' @param vars a character vector of the predictors of interest
#' @param n two dimensional integer vector giving the resolution of the grid. the first element gives the grid on \code{vars} and the second on the other columns, which are subsampled.
#' @param interaction logical, if 'vars' is a vector, does this specify an interaction or a list of bivariate partial dependence
#' @param uniform logical, indicates whether a uniform or random grid is to be construct
#' partial dependence calculation
#' @param data the data.frame used to fit the model. must only include the covariates in the model. only needed for 'randomForest' and 'ranger.'
#' @param ... additional arguments to be passed to \code{marginalPrediction}
#' @return a data.frame with the partial dependence of 'vars'
#' if 'vars' has length = 1 then the output will be a data.frame with a column for the predicted value at each value of 'vars', averaged over the values of all other predictors.
#' if 'vars' has length > 1 and interaction is true or false then the output will be a data.frame with a column for each element of 'vars' and the predicted value for each combination.
#'
#' @seealso \code{\link{plot_pd}} for plotting \code{partial_dependence}.
#' @references
#'
#' Friedman, Jerome H. "Greedy function approximation: a gradient boosting machine." Annals of statistics (2001): 1189-1232.
#'
#' @examples
#' library(randomForest)
#' library(edarf)
#' 
#' data(iris)
#' data(swiss)
#' 
#' ## classification
#' fit = randomForest(Species ~ ., iris)
#' pd = partial_dependence(fit, c("Sepal.Width", "Sepal.Length"),
#'   data = iris[, -ncol(iris)])
#' pd_int = partial_dependence(fit, c("Petal.Width", "Sepal.Length"), 
#'   interaction = TRUE, data = iris[, -ncol(iris)])
#'
#' ## Regression
#' fit = randomForest(Fertility ~ ., swiss)
#' pd = partial_dependence(fit, c("Education", "Examination"), data = swiss[, -1])
#' pd_int = partial_dependence(fit, c("Education", "Examination"),
#'   interaction = TRUE, data = swiss[, -1])
#' @export
partial_dependence = function(fit, vars, n, interaction, uniform, data, ...)
  UseMethod("partial_dependence", fit)
#' @export
partial_dependence.randomForest = function(fit, vars = colnames(data),
   n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), 
   interaction = FALSE, uniform = TRUE, data, ...) {

  ## remove target if included
  data = data[, !apply(data, 2, function(x) all(x == fit$y))]
  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "uniform" = uniform,
    "predict.fun" = if (is.factor(fit$y)) function(object, newdata)
      predict(object, newdata, type = "prob") else function(object, newdata)
        predict(object, newdata),
    ...
  )
  
  if (length(vars) > 1L & !interaction) {
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      renameColumns(fit, do.call("marginalPrediction", args))
    }, simplify = FALSE), fill = TRUE)
    setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  }
  
  else
    pd = renameColumns(fit, do.call(marginalPrediction, args))

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "target") = if (is.factor(fit$y)) levels(fit$y) else ifelse(!is.null(fit$terms), deparse(attr(fit$terms, "variables")[[2]]), "prediction")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "vars") = vars
  pd
}
#' @export
partial_dependence.RandomForest = function(fit, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), 
  interaction = FALSE, uniform = TRUE, data, ...) {

  target = names(get("response", fit@data@env))
  data = data.frame(get("input", fit@data@env))

  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "uniform" = uniform,
    "predict.fun" = if (ncol(fit@responses@variables) > 1L | is.factor(fit@responses@variables[,, drop = TRUE]))
      function(object, newdata)
        do.call("rbind", object@predict_response(newdata, type = "prob")) else function(object, newdata)
          object@predict_response(newdata, type = "response"),
    ...
  )
  
  if (length(vars) > 1L & !interaction) {
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      renameColumns(fit, do.call("marginalPrediction", args))
    }, simplify = FALSE), fill = TRUE)
    setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else
    pd = renameColumns(fit, do.call(marginalPrediction, args))

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "target") = if (ncol(fit@responses@predict_trafo) > 1) gsub(paste0(target, "\\.", collapse = "|"), "", colnames(fit@responses@predict_trafo)) else target
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "vars") = vars
  pd
}
#' @export
partial_dependence.rfsrc = function(fit, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), 
  interaction = FALSE, uniform = TRUE, data, ...) {

  target = fit$yvar.names

  args = list(
    "data" = fit$xvar,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "uniform" = uniform,
    "predict.fun" = if (is.factor(fit$y)) function(object, newdata)
      predict(object, newdata, type = "prob")[["predicted"]] else
        function(object, newdata)
          predict(object, newdata)[["predicted"]],
    ...
  )
  
  if (length(vars) > 1L & !interaction) {
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      renameColumns(fit, do.call("marginalPrediction", args))
    }, simplify = FALSE), fill = TRUE)
    setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else
    pd = renameColumns(fit, do.call(marginalPrediction, args))

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "target") = if (is.factor(fit$yvar)) levels(fit$yvar) else fit$yvar.names
  attr(pd, "vars") = vars
  pd
}

#' @export
partial_dependence.ranger = function(fit, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)),
  interaction = FALSE, uniform = TRUE, data, ...) {

  target = strsplit(strsplit(as.character(fit$call), "formula")[[2]], " ~")[[1]][[1]]

  predict.fun = function(object, newdata) {
    if (object$treetype != "Classification") {
      predict(object, data = newdata)$predictions
    } else {
      t(apply(predict(object, data = newdata, predict.all = TRUE)$predictions, 1,
        function(x) table(factor(x, seq_len(length(unique(data[[target]]))),
          levels(data[[target]]))) / length(x)))
      }
  }

  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "uniform" = uniform,
    "predict.fun" = predict.fun,
    ...
  )
  
  if (length(vars) > 1L & !interaction) {
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      mp = do.call("marginalPrediction", args)
      if (fit$treetype == "Regression")
        names(mp)[ncol(mp)] = target
      mp
    }, simplify = FALSE), fill = TRUE)
    setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd = do.call("marginalPrediction", args)
    if (fit$treetype == "Regression")
      names(pd)[ncol(pd)] = target
  }

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "target") = if (fit$treetype != "Classification") target else levels(fit$predictions)
  attr(pd, "vars") = vars
  pd
}
