# extract target names from fit objects and rename columnns if needed
renameColumns = function(fit, pd)
  UseMethod("renameColumns")

renameColumns.ranger = function(fit, pd) {
  pd
}

renameColumns.randomForest = function(fit, pd) {
  if (fit$type == "regression" & any(grepl("formula", class(fit)))) {
    target = deparse(attr(fit$terms, "variables")[[2]])
  } else if (fit$type == "classification") {
    target = colnames(fit$err.rate)[-1]
  } else {
    target = "prediction"
  }
  
  colnames(pd)[(ncol(pd) - length(target) + 1):ncol(pd)] = target
  pd
}

renameColumns.RandomForest = function(fit, pd) {
  target = colnames(fit@responses@variables)
  if (ncol(fit@responses@variables) > 1L |
        (ncol(fit@responses@variables) == 1L & is.factor(fit@responses@variables[, 1]))) {
    target = gsub(paste0(target, "\\.", collapse = "|"), "",
      colnames(fit@responses@predict_trafo))
  }

  colnames(pd)[(ncol(pd) - length(target) + 1):ncol(pd)] = target
  pd
}

renameColumns.rfsrc = function(fit, pd) {
  if (is.factor(fit$yvar)) {
    target = levels(fit$yvar)
  } else {
    target = fit$yvar.names
  }

  colnames(pd)[(ncol(pd) - length(target) + 1):ncol(pd)] = target
  pd
}
