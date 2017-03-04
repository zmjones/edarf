#' Variable importance using random forests
#'
#' Computes local or aggregate variable importance for a set of predictors from a fitted random forest object from the party, randomForest, randomForestSRC, or ranger package
#'
#' @importFrom stats predict
#' @importFrom mmpf permutationImportance
#'
#' @param fit object of class 'RandomForest', 'randomForest', 'rfsrc', or `ranger`
#' @param vars character, variables to find the importance of
#' @param interaction logcal, compute the joint and additive importance for observations (\code{type = "local"}) or variables \code{type = "aggregate"}
#' @param nperm positive integer giving the number of times to permute the indicated variables (default 10)
#' @param data optional (unless using randomForest) data.frame with which to calculate importance
#' @param ... additional arguments to be passed to \code{permutationImportance}.
#'
#' @return a named list of \code{vars} with the return from \code{permutationImportance} for each.
#' @seealso \code{\link{plot_imp}} for plotting the results of \code{variable_importance}.
#' @references
#' Breiman, Leo. "Random forests." Machine learning 45.1 (2001): 5-32.
#' 
#' @examples
#' library(randomForest)
#' data(iris)
#' fit = randomForest(Species ~ ., iris)
#' variable_importance(fit, nperm = 2, data = iris)
#' @export
variable_importance = function(fit, vars, interaction, nperm, data, ...)
  UseMethod("variable_importance")
#' @export
variable_importance.randomForest = function(fit, vars,
  interaction = FALSE, nperm = 100L, data, ...) {

  vars = if (missing(vars)) attributes(fit$terms)$term.labels else vars
  args = list(
    "data" = data,
    "vars" = vars,
    "y" =  if (!is.null(fit$terms)) deparse(attr(fit$terms, "variables")[[2]]) else colnames(data)[which(sapply(data, function(x) all.equal(x, fit$y, check.attributes = FALSE)) == "TRUE")],
    "nperm" = nperm,
    "model" = fit,
    "predict.fun" = function(object, newdata) predict(object, newdata),
    ...
  )
  
  if (length(vars) > 1L & !interaction)
    imp = sapply(vars, function(x) {    
      args$vars = x
      do.call("permutationImportance", args)
    }, simplify = FALSE)
  else
    imp = do.call("permutationImportance", args)

  attr(imp, "class") = c("imp", class(imp))
  attr(imp, "target") = args$y
  attr(imp, "nperm") = nperm
  imp
}
#' @export
variable_importance.RandomForest = function(fit, vars,
  interaction = FALSE, nperm = 100L, data = NULL, ...) {

  target = names(get("response", fit@data@env))
  data = data.frame(get("input", fit@data@env),
    get("response", fit@data@env))
  vars = if (missing(vars)) colnames(get("input", fit@data@env)) else vars

  args = list(
    "data" = data,
    "y" = target,
    "vars" = vars,
    "nperm" = nperm,
    "model" = fit,
    "predict.fun" = function(object, newdata) object@predict_response(newdata),
    ...
  )
  
  if (length(vars) > 1L & !interaction)
    imp = sapply(vars, function(x) {
      args$vars = x
      do.call("permutationImportance", args)
    }, simplify = FALSE)
  else
    imp = do.call("permutationImportance", args)

  attr(imp, "class") = c("imp", class(imp))
  attr(imp, "target") = args$y
  attr(imp, "nperm") = nperm
  imp
}
#' @export
variable_importance.rfsrc = function(fit, vars,
  interaction = FALSE, nperm = 100, data = NULL, ...) {

  data = data.frame(fit$xvar, fit$yvar)
  names(data)[(ncol(fit$xvar) + 1):ncol(data)] = fit$yvar.names
  vars = if (missing(vars)) colnames(fit$xvar) else vars
  
  args = list(
    "data" = data,
    "vars" = vars,
    "y" = fit$yvar.names,
    "nperm" = nperm,
    "model" = fit,
    "predict.fun" = function(object, newdata) if (is.factor(object$yvar)) predict(object, newdata)[["class"]] else predict(object, newdata)[["predicted"]],
    ...
  )

  if (length(vars) > 1L & !interaction)
    imp = sapply(vars, function(x) {
      args$vars = x
      do.call("permutationImportance", args)
    }, simplify = FALSE)
  else
    imp = do.call("permutationImportance", args)

  attr(imp, "class") = c("imp", class(imp))
  attr(imp, "target") = args$y
  attr(imp, "nperm") = nperm
  imp
}

#' @export
variable_importance.ranger = function(fit, vars, interaction = FALSE, nperm = 100,
  data, ...) {

  args = list(
    "data" = data,
    "vars" = vars,
    "y" = names(data)[!names(data) %in% fit$forest$independent.variable.names],
    "nperm" = nperm,
    "model" = fit,
    "predict.fun" = function(object, newdata) predict(object, newdata)[["predictions"]],
    ...
  )
  
  if (length(vars) > 1L & !interaction)
    imp = sapply(vars, function(x) {
      args$vars = x
      do.call("permutationImportance", args)
    }, simplify = FALSE)
  else
    imp = do.call("permutationImportance", args)

  attr(imp, "class") = c("imp", class(imp))
  attr(imp, "target") = args$y
  attr(imp, "nperm") = nperm
  imp  
}
