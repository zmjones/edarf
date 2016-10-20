#' Plot partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#' @importFrom stats as.formula
#' 
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param facet a character vector indicating the variable that should be used
#' to facet on if interaction is plotted. If not specified the variable with less 
#' unique values is chosen.
#' @param to_plot a character vector indicating what variables to plot. If not
#' specified then all variables are plotted
#' @return a ggplot2 object
#' 
#' @examples
#' library(randomForest)
#' library(edarf)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, iris, "Petal.Width")
#' plot_pd(pd)
#' @export
plot_pd <- function(pd, facet = NULL, to_plot = NULL) {
  atts <- attributes(pd)
  if (atts$multivariate)
    stop("multivariate plots not supported.")
  if (is.null(facet) & atts$interaction)
    facet <- names(which.min(apply(pd[, atts$var], 2, function(x) length(unique(x)))))
  if (!is.null(facet)) {
    if (!any(facet %in% atts$var))
      stop("facet must be one of the variables in the pd argument.")
    ordering <- unique(sort(pd[[facet]]))
    if (is.factor(pd[[facet]]))
      labels <- paste0(facet, " = ", as.character(ordering))
    else
      labels <- paste0(facet, " = ", as.character(signif(ordering, 3)))
    pd[[facet]] <- factor(pd[[facet]], levels = ordering, labels = labels)
    var <- atts$var[atts$var != facet]
  } else {
    var <- atts$var
  }
  
  if(!is.null(to_plot)){
    if(!all(to_plot %in% atts$target)) stop("to_plot contains variables that are not in the partial dependencies")
    keep <- atts$names[!(atts$names %in% atts$target)]
    atts$target <- to_plot
    pd <- pd[,c(to_plot, keep)]
    rm(keep)
  }

  dat <- melt(pd, id.vars = c(atts$target, facet), na.rm = TRUE)
  if (is.character(dat$value)) ## casting factors to integers, ack!
    dat$value <- as.integer(dat$value)
  
  #### Not sure if below if statement is right. 
  if (length(atts$target) > 1 | (length(atts$target) == 1 & atts$prob)) 
    dat <- melt(dat, id.vars = c("variable", "value", facet), value.name = "Probability",
                variable.name = "Class", na.rm = TRUE)

  if (length(var) == 1 & !atts$prob) {
    p <- ggplot(dat, aes_string("value", atts$target))
  } else if (!atts$prob) {
    p <- ggplot(dat, aes_string("value", atts$target, group = "variable"))
  } else {
    p <- ggplot(dat, aes_string("value", "Probability", colour = "Class"))
  }

  p <- p + geom_point()

  if (!(is.factor(pd[, var]) & !is.ordered(pd[, var])))
    p <- p + geom_line()
  
  if (length(var) == 1)
    p <- p + labs(x = var)

  if (length(atts$var) > 1) {
    if (!atts$interaction) {
      p <- p + facet_wrap(~ variable, scales = "free_x")
    } else {
      p <- p + facet_wrap(as.formula(paste0("~ ", facet)))
    }
  }
  p + theme_bw()
}
#' Plot variable importance from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#' 
#' @param imp object of class \code{c("importance", "data.frame")} as returned by
#' \code{\link{variable_importance}}
#' @param geom character describing type of plot desired: "point" or "bar"
#' @param sort character indicating if sorting of the output is to be done.
#' can be "none", "ascending", or "descending." only applicable when the importance type is "aggregate," or "local" and and the outcome variable is a "factor"
#' @param labels character vector giving labels to variables,
#' must have length equal to the number of rows or length of \code{imp}
#' @param scales character, "free", "fixed", "free_x", or "free_y", only applicable when \code{type = "local"} and the outcome is an ordered factor or numeric
#' @param se logical, plot a standard error ribbon around the estimated residual density under permutation, only applicable when \code{type = "local"} and the outcome is an ordered factor or numeric
#' @return a ggplot2 object
#' @examples
#' library(randomForest)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#'
#' ## class-specific importance for all variables
#' imp <- variable_importance(fit, var = colnames(iris)[-5],
#'                            type = "local", interaction = TRUE, nperm = 2, data = iris)
#' plot_imp(imp)
#'
#' imp <- variable_importance(fit, var = colnames(iris)[-5], type = "aggregate",
#'                            interaction = FALSE, nperm = 2, data = iris)
#' plot_imp(imp)
#'
#' data(swiss)
#' fit <- randomForest(Fertility ~ ., swiss)
#' imp <- variable_importance(fit, var = colnames(swiss)[-1],
#'                            type = "local", interaction = TRUE, nperm = 2, data = swiss)
#' plot_imp(imp)
#' @export
plot_imp <- function(imp, geom = "point", sort = "decreasing", labels = NULL, scales = "free_y", se = TRUE) {
  atts <- attributes(imp)
  if (atts$type == "local") {
    imp$target <- atts$target
    if (class(atts$target) == "factor") {
      imp <- sapply(levels(imp$target), function(x)
        colMeans(imp[imp$target == x, -ncol(imp), drop = FALSE]), simplify = FALSE)
      imp <- do.call("cbind", imp)
      imp <- as.data.frame(imp)
      if (!is.null(labels)) {
        if (length(labels) == nrow(imp)) {
          imp$labels <- labels
        } else {
          stop("length of labels does not match length of importance output")
        }
      } else {
        imp$labels <- row.names(imp)
      }
      row.names(imp) <- NULL
      imp <- melt(imp, id.vars = "labels", variable.name = "Class")
      if (geom == "point") {
        p <- ggplot(imp, aes_string("value", "labels", color = "Class")) +
          geom_point()
      } else if (geom == "bar") {
        p <- ggplot(imp, aes_string("labels", "value", fill = "Class")) +
          geom_bar(stat = "identity", position = "dodge") + coord_flip()
      } else {
        stop("invalid input to geom argument")
      }
    } else if (is.ordered(atts$target) | is.numeric(atts$target)) {
      imp <- melt(imp, id.vars = "target")
      p <- ggplot(imp, aes_string("target", "value")) +
        stat_smooth(method = "loess", se = se) +
        facet_wrap(~ variable, scales = scales)
    } else {
      stop("")
    }
    if (is.factor(atts$target)) {
      xlab <- "Permutation Importance"
      ylab <- ""
    } else {
      xlab <- "Feature Value"
      ylab <- "Residual Under Permutation"
    }
  } else { ## type == "aggregate"
    imp <- data.frame(value = imp)
    if (!is.null(labels)) {
      if (length(labels) == length(imp)) {
        imp$labels <- labels
      } else {
        stop("length of labels does not match length of importance output")
      }
    } else {
      if (atts$interaction) {
        imp$labels <- c(atts$var, "additive", "joint")
      } else {
        imp$labels <- atts$var
      }
    }
    if (sort == "increasing") {
      imp$labels <- factor(imp$labels, levels = imp$labels[order(imp$value, decreasing = FALSE)])
    } else if (sort == "decreasing") {
      imp$labels <- factor(imp$labels, levels = imp$labels[order(imp$value, decreasing = TRUE)])
    } else {
      stop("invalid input to sort argument")
    }
    if (geom == "point") {
      p <- ggplot(imp, aes_string("value", "labels")) + geom_point()
    } else if (geom == "bar") {
      p <- ggplot(imp, aes_string("labels", "value")) + geom_bar(stat = "identity") + coord_flip()
    } else {
      stop("invalid input to geom argument")
    }
    if (geom == "bar") {
      ylab <- "Permutation Importance"
      xlab <- ""
    } else {
      xlab <- "Permutation Importance"
      ylab <- ""
    }
  }
  p <- p + labs(x = xlab, y = ylab)
  p + theme_bw()
}
#' Plot principle components of the proximity matrix
#'
#' @references https://github.com/vqv/ggbiplot
#' @references Gabriel, "The biplot graphic display of matrices with application to principal component analysis," \emph{Biometrika}, 1971
#'
#' @import ggplot2
#'
#' @param pca a prcomp object, pca of an n x n matrix giving the proportion of times across all trees that observation i,j are in the same terminal node
#' @param dims integer vector of length 2 giving indices for the dimensions of \code{pca} to be plotted
#' @param labels length n character vector giving observation labels
#' @param alpha optional continuous vector of length n make points/labels transparent or
#' a numeric of length 1 giving the alpha of all points/labels
#' @param alpha_label character legend title if alpha parameter used
#' @param color optional discrete vector of length n which colors the points/labels or
#' a character vector giving the color of all points/labels
#' @param color_label character legend title if color parameter is used
#' @param shape optional discrete vector of length n which shapes points (not applicable if labels used) or
#' a character vector of length 1 which gives the shape of all points
#' @param shape_label character legend title if shape parameter is used
#' @param size optional continuous vector of length n which sizes points or labels or
#' a numeric of length 1 which gives the sizes of all the points
#' @param size_label character legend title if size parameter used
#' @param xlab character x-axis label
#' @param ylab character y-axis label
#' @param title character plot title
#' 
#' @return a ggplot object
#'
#' @examples
#' library(randomForest)
#' 
#' fit <- randomForest(hp ~ ., mtcars, proximity = TRUE)
#' prox <- extract_proximity(fit)
#' pca <- prcomp(prox, scale = TRUE)
#' plot_prox(pca, labels = row.names(mtcars))
#' 
#' fit <- randomForest(Species ~ ., iris, proximity = TRUE)
#' prox <- extract_proximity(fit)
#' pca <- prcomp(prox, scale = TRUE)
#' plot_prox(pca, color = iris$Species, color_label = "Species", size = 2)
#' @export
plot_prox <- function(pca, dims = 1:2, labels = NULL,
                      alpha = 1, alpha_label = NULL,
                      color = "black", color_label = NULL,
                      shape = "1", shape_label = NULL,
                      size = 2, size_label = NULL,
                      xlab = NULL, ylab = NULL, title = "") {
  if (is.numeric(color))
    stop("gradient coloring not supported. add this outside of this function.")
  nobs_factor <- sqrt(nrow(pca$x) - 1)
  d <- pca$sdev
  u <- sweep(pca$x, 2, 1 / (d * nobs_factor), FUN = "*")
  prop_var <- 100 * pca$sdev[dims]^2 / sum(pca$sdev^2)
  plt <- as.data.frame(sweep(u[, dims], 2, d[dims], FUN = "*"))
  plt <- plt * nobs_factor
  plt$ymax <- max(plt[, 2])

  plt$alpha <- alpha
  plt$color <- color
  plt$shape <- shape
  plt$size <- size

  if (!is.null(labels)) {
    plt$labels <- labels
    p <- ggplot(plt, aes_string("PC1", "PC2", color = "color", shape = "shape",
                                alpha = "alpha", size = "size", ymax = "ymax", label = "labels"))
  } else {
    p <- ggplot(plt, aes_string("PC1", "PC2", color = "color", shape = "shape",
                                alpha = "alpha", size = "size", ymax = "ymax"))
  }
  
  if (!is.null(color)) {
    if (length(color) > 1)
      p <- p + scale_color_discrete(name = color_label)
    else
      p <- p + scale_colour_discrete(guide = FALSE)
  }
  if (!is.null(size)) {
    if (length(size) > 1)
      p <- p + scale_size(name = size_label)
    else
      p <- p + scale_size(guide = FALSE)
  }
  if (!is.null(shape)) {
    if (length(shape) > 1)
      p <- p + scale_shape(name = shape_label)
    else
      p <- p + scale_shape(guide = FALSE)
  }
  if (!is.null(alpha)) {
    if (length(alpha) > 1)
      p <- p + scale_alpha(name = alpha_label)
    else
      p <- p + scale_alpha(guide = FALSE)
  }
  if (is.null(labels))
    p <- p + geom_point()
  else
    p <- p + geom_text()

  if (is.null(xlab))
    xlab <- paste0("PC1 (", round(prop_var[1], 0), "% explained var.)")
  if (is.null(ylab))
    ylab <- paste0("PC2 (", round(prop_var[2], 0), "% explained var.)")
  p <- p + labs(x = xlab, y = ylab, title = title)
  p + theme_bw()
}
#' Plot predicted versus observed values
#'
#' @param predicted numeric vector of predictions
#' @param observed numeric vector of observations
#' @param perfect_line logical whether to plot a blue 45 degree line
#' on which perfect predictions would fall
#' @param outlier_idx integer indices of outliers to be labelled
#' between the predicted and observed value pairs are labeled an outlier
#' @param labs character labels for points, applied to a subset determined by the
#' 'outlier_criterion'
#' @param xlab character label for the x-axis, defaults to "Observed"
#' @param ylab character label for the y-axis, defaults to "Predicted"
#' @param title character title defaults to ""
#'
#' @return a ggplot object
#' @examples
#' library(randomForest)
#' library(edarf)
#' fit <- randomForest(hp ~ ., mtcars)
#' pred <- predict(fit, newdata = mtcars, OOB = TRUE)
#' plot_pred(pred, mtcars$hp,
#'           outlier_idx = which(abs(pred - mtcars$hp) > .5 * sd(mtcars$hp)),
#'           labs = row.names(mtcars))
#' @export
plot_pred <- function(predicted, observed, perfect_line = TRUE, outlier_idx = NULL, labs = NULL,
                      xlab = "Observed", ylab = "Predicted", title = "") {
  if (!(is.numeric(predicted) & is.numeric(observed)))
    stop("predicted, and observed must be numeric")
  out <- data.frame("predicted" = predicted, "observed" = observed)
  out$labs <- labs
  p <- ggplot(out, aes_string("observed", "predicted"))
  p <- p + geom_point()
  if (perfect_line)
    p <- p + geom_abline(aes_string(intercept = 0, slope = 1), colour = "blue")
  if (!is.null(outlier_idx)) {
    out$ymax <- max(c(out$predicted))
    if (!is.integer(outlier_idx))
      stop("Invalid input for outlier_idx")
    if (is.null(labs))
      stop("Labels must be passed to label outliers")
    p <- p + geom_text(data = out[outlier_idx, ],
                       aes_string("observed", "predicted", label = "labs", ymax = "ymax"),
                       size = 3, hjust = 0, vjust = 0, parse = FALSE)
  }
  p <- p + labs(x = xlab, y = ylab, title = title)
  p + theme_bw()    
}
