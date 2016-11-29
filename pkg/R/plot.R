#' Plot partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom data.table melt
#' @importFrom stats as.formula
#' 
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param facet a character vector indicating the variable that should be used
#' to facet on if interaction is plotted. If not specified the variable with less 
#' unique values is chosen.
#' @return a ggplot2 object
#' 
#' @examples
#' library(randomForest)
#' library(edarf)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, "Petal.Width", data = iris)
#' plot_pd(pd)
#' @export
plot_pd <- function(pd, facet = NULL) {
  atts <- attributes(pd)
  if (length(atts$vars) > 2 & atts$interaction) {
    stop("too many variables to plot.")
  }

  if (!is.null(facet)) {
    if (!any(facet %in% atts$vars))
      stop("facet must be one of the variables in the pd argument.")
    ordering <- unique(sort(pd[[facet]]))
    if (is.factor(pd[[facet]]))
      labels <- paste0(facet, " = ", as.character(ordering))
    else
      labels <- paste0(facet, " = ", as.character(signif(ordering, 3)))
    pd[[facet]] <- factor(pd[[facet]], levels = ordering, labels = labels)
    vars <- atts$vars[atts$vars != facet]
  } else {
    vars <- atts$vars
  }

  if (!is.null(facet) | !atts$interaction) {
    dat <- melt(pd, id.vars = c(atts$target, facet), na.rm = TRUE)

    if (is.character(dat$value)) { ## casting factors to numerics, ack!
      dat$value <- as.numeric(dat$value)
    }
  
    if (length(atts$target) > 1)
      dat <- melt(dat, id.vars = c("variable", "value", facet),
        value.name = "prediction", variable.name = "class", na.rm = TRUE)

    if (length(atts$target) == 1) {
      p <- ggplot(dat, aes_string("value", atts$target))
    } else {
      p <- ggplot(dat, aes_string("value", "prediction", colour = "class"))
    }

    p <- p + geom_line() + geom_point()
  } else {
    dat <- pd
    dat <- melt(dat, id.vars = vars)
    p <- ggplot(dat, aes_string(vars[1], vars[2], fill = "value")) + geom_raster()
    facet <- "variable"
  }

  if (length(vars) == 1)
    p <- p + labs(x = vars)

  if (length(atts$vars) > 1) {
    if (!atts$interaction) {
      p <- p + facet_wrap(~ variable, scales = "free_x")
    } else {
      p <- p + facet_wrap(as.formula(paste0("~", facet)))
    }
  }
  p
}
#' Plot variable importance from random forests
#'
#' @import ggplot2
#' @importFrom data.table melt
#' 
#' @param imp object of class \code{c("importance", "data.frame")} as returned by
#' \code{\link{variable_importance}}
#' @param sort character indicating if sorting of the output is to be done.
#' can be "ascending", or "descending."
#' @return a ggplot2 object
#' @examples
#' library(randomForest)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#' imp <- variable_importance(fit, nperm = 2, data = iris)
#' plot_imp(imp)
#' @export
plot_imp <- function(imp, sort = "decreasing") {
  atts <- attributes(imp)
  if (is.list(imp) && all(sapply(imp, length) == 1)) {
    dat <- data.frame(variable = names(imp), values = unname(unlist(imp)))
    dat$variable <- factor(dat$variable,
      dat$variable[order(dat$values, decreasing = sort == "decreasing")])
    p <- ggplot(dat, aes_string("variable", "values")) + geom_point()
    p <- p + labs(y = "permutation importance", x = NULL)
  } else {
    stop("plot_imp only plots importance for multiple variables where there is one importance estimate per variable")
  }
  p
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
  alpha = 1, alpha_label = NULL, color = "black", color_label = NULL,
  shape = "1", shape_label = NULL, size = 2, size_label = NULL,
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
#' pred <- predict(fit, newdata = mtcars)
#' plot_pred(pred, mtcars$hp,
#'   outlier_idx = which(abs(pred - mtcars$hp) > .5 * sd(mtcars$hp)),
#'   labs = row.names(mtcars))
#' @export
plot_pred <- function(predicted, observed, perfect_line = TRUE,
  outlier_idx = NULL, labs = NULL,
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
    if (!is.integer(outlier_idx))
      stop("Invalid input for outlier_idx")
    if (is.null(labs))
      stop("Labels must be passed to label outliers")
    p <- p + geom_text(data = out[outlier_idx, ],
      aes_string("observed", "predicted", label = "labs"),
      size = 3, hjust = 0, vjust = 0, parse = FALSE)
  }
  p + labs(x = xlab, y = ylab, title = title)
}
