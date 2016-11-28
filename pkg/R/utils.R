## rename columns of marginal predictions using fit objects
renameColumns <- function(fit, pd)
  UseMethod("renameColumns")

renameColumns.ranger <- function(fit, pd) {
  if (is.matrix(pd$prediction)) {
    names(pd)[1] <- ""
  }

  if (ncol(pd$points) > 1L)
    names(pd)[2] <- ""
  
  as.data.frame(do.call(cbind, pd), stringsAsFactors = FALSE, check.names = FALSE)
}

renameColumns.randomForest <- function(fit, pd) {
  if (is.matrix(pd$prediction)) {
    target <- colnames(fit$err.rate)[-1]
    colnames(pd$prediction) <- target
    pd$prediction <- as.data.frame(pd$prediction, check.names = FALSE)
    names(pd)[1] <- ""
  } else {
    if (!is.null(fit$terms)) {
      target <- deparse(attr(fit$terms, "variables")[[2]])
    } else {
      ## with randomForest.default it is annoying/imposible to
      ## extract the target variable name from the object
      target <- "prediction"
    }
    names(pd)[1] <- target
  }

  if (ncol(pd$points) > 1L)
    names(pd)[2] <- ""

  as.data.frame(do.call(cbind, pd), stringsAsFactors = FALSE, check.names = FALSE)
}

renameColumns.RandomForest <- function(fit, pd) {
  target <- colnames(fit@responses@variables)
  if (ncol(pd$prediction) > 1L) {
    target <- gsub(paste0(target, "\\.", collapse = "|"), "",
      colnames(fit@responses@predict_trafo))
    colnames(pd$prediction) <- target
    names(pd)[1] <- ""
  } else {
    names(pd)[1] <- target
  }

  if (ncol(pd$points) > 1L)
    names(pd)[2] <- ""

  as.data.frame(do.call(cbind, pd), stringsAsFactors = FALSE, check.names = FALSE)
}

renameColumns.rfsrc <- function(fit, pd) {
  if (is.matrix(pd$prediction)) {
    colnames(pd$prediction) <- levels(fit$yvar)
    names(pd)[1] <- ""
  } else {
    names(pd)[1] <- fit$yvar.names
  }

  if (ncol(pd$points) > 1L)
    names(pd)[2] <- ""
  
  as.data.frame(do.call(cbind, pd), stringsAsFactors = FALSE, check.names = FALSE)
}
