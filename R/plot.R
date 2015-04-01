#' Plot partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param geom character describing type of plot desired: "bar", "line", or "area"
#' @param xlab x-axis label, default depends on input
#' @param ylab y-axis label, default depends on input
#' @param title title for the plot
#' @param facet_var a character vector indicating the variable that should be used
#' to facet on if interaction is plotted. If not specified the variable with less 
#' unique values is chosen.
#' @param scales can be "free", "free_x", "free_y" or "fixed", applicable when facetting
#' @return a ggplot2 object
#' 
#' @examples \dontrun{
#' library(randomForest)
#' library(edarf)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, iris, "Petal.Width", type = "prob")
#' plot_pd(pd, geom = "area")
#' }
#' @export
plot_pd <- function(pd, geom = "line", xlab = NULL, ylab = NULL, title = "", facet_var = NULL,
                    scales = "free_x") {
    atts <- attributes(pd)
    ## One predictor Plots
    if (!atts$interaction & length(atts$var) == 1) {
        if (!atts$prob & !atts$multivariate) {
            ## Numeric Y or Majority class
            p <- ggplot(pd, aes_string(colnames(pd)[1], colnames(pd)[2]))
            if (geom == "line") {
                p <- p + geom_point() + geom_line()
                if (atts$ci) p <- p + geom_errorbar(aes_string(ymin = "low", ymax = "high"), alpha = .25)
            } else if (geom == "bar")
                p <- p + geom_bar(stat = "identity")
              else stop("Unsupported geom")
            if (is.null(ylab))
                ylab <- paste("Predicted", colnames(pd)[2])
            if (is.null(xlab))
                xlab <- colnames(pd)[1]
            p <- p + labs(y = ylab, x = xlab, title = title)
        } else if (atts$prob & !atts$multivariate) {
            ## Predicted probabilities
            df <- melt(pd, id.vars = 1)
            colnames(df) <- c("x", "Class", "Probability")
            if (geom == "area") {
                p <- ggplot(df, aes_string("x", "Probability", fill = "Class"))
                p <- p + geom_area(position = "fill")
                p <- p + scale_fill_grey()
            } else if (geom == "line") {
                p <- ggplot(df, aes_string("x", "Probability", colour = "Class"))
                p <- p + geom_line() + geom_point()
            } else stop("Unsupported geom")
            if (is.null(ylab))
                ylab <- "Probability"
            if (is.null(xlab))
                xlab <- colnames(pd)[1]
            p <- p + labs(x = xlab, y = ylab, title = title)
        } else {
            ## Multivariate with single explanatory variable
            df <- melt(pd, id.vars = 1)
            colnames(df) <- c("x", "Outcome", "value")
            df$Outcome <- paste0("Outcome: ", df$Outcome)
            p <- ggplot(df, aes_string("x", "value", group = "Outcome"))
            p <- p + geom_line() + geom_point()
            p <- p + facet_wrap(~ Outcome, scales = scales)
            if (is.null(ylab))
                ylab <- "Predicted Outcome"
            if (is.null(xlab))
                xlab <- colnames(pd)[1]
            p <- p + labs(x = xlab, y = ylab, title = title)
        }
    } else if (!atts$interaction & length(atts$var) > 1) {
        if (!atts$prob & !atts$ci) {
            p <- ggplot(pd, aes_string("value", atts$target, group = "variable"))
            if (geom == "line") {
                p <- p + geom_line() + geom_point()
            } else if (geom == "bar") {
                p <- p + geom_bar(stat = "identity")
            } else {
                stop("unsupported geom for this partial_dependence input")
            }
        } else if (atts$prob & !atts$ci) {
            df <- melt(pd, id.vars = c("value", "variable"), value.name = "Probability",
                       variable.name = "Class")
            if (geom == "area") {
                p <- ggplot(df, aes_string("value", "Probability", fill = "Class"))
                p <- p + geom_area(position = "fill")
                p <- p + scale_fill_grey()
            } else if (geom == "line") {
                p <- ggplot(df, aes_string("value", "Probability", colour = "Class"))
                p <- p + geom_line() + geom_point()
            } else stop("Unsupported geom")
        } else if (atts$ci & !atts$prob) {
            p <- ggplot(pd, aes_string("value", atts$target))
            p <- p + geom_line() + geom_point()
            p <- p + geom_errorbar(aes_string(ymin = "low", y = atts$target, ymax = "high"), alpha = .25)
        } else {
            stop("some sort of error")
        }
        p <- p + facet_wrap(as.formula(paste0("~ ", facet_var)), scales = scales)
        if (is.null(ylab))
            ylab <- "Predicted Outcome"
        if (is.null(xlab))
            xlab <- "Predictor Scale"
        p <- p + labs(x = xlab, y = ylab, title = title)
    } else if (atts$interaction) {
        ## Interaction Plots
        if (length(atts$var) > 2) stop("Only two-way interactions supported")
        if (!is.null(facet_var)) {
            n_unique <- apply(pd[, atts$var], 2, function(x) length(unique(x)))
            facet_var <- names(which.min(n_unique))
        }
        if ((class(pd[, facet_var]) %in% c("numeric", "integer"))) {
            ordering <- as.character(unique(sort(pd[, facet_var])))
            labels <- paste0(facet_var, " = ", ordering)
            pd[, facet_var] <- factor(pd[, facet_var], levels = ordering,
                                      labels = labels)
        } else if (class(pd[, facet_var]) == "character") {
            pd[, facet_var] <- as.factor(pd[, facet_var])
        }
        plot_var <- atts$var[atts$var != facet_var]
        if (atts$prob) {
            df <- melt(pd, id.vars = atts$var)
            colnames(df) <- c(atts$var, "Class", "Probability")
            if (geom == "area") {
                p <- ggplot(df, aes_string(x = plot_var,
                                           y = "Probability",
                                           fill = "Class"))
                p <- p + geom_area(position = "fill")
                p <- p + scale_fill_grey()
            } else if (geom == "line") {
                p <- ggplot(df, aes_string(x = plot_var,
                                           y = "Probability",
                                           colour = "Class"))
                p <- p + geom_line() + geom_point()
            } else stop("Unsupported geom")
            p <- p + facet_wrap(as.formula(paste0("~", facet_var)), scales = scales)
            if (is.null(xlab))
                xlab <- plot_var
            if (is.null(ylab))
                ylab <- "Probability"
            p <- p + labs(x = xlab, y = ylab, title = title)
        } else if (!atts$prob & class(pd[, 3]) == "numeric" & !atts$multivariate) {
            y <- colnames(pd)[3]
            p <- ggplot(pd, aes_string(x = plot_var, y = y, group = facet_var))
            p <- p + facet_wrap(as.formula(paste0("~", facet_var)), scales = scales)
            p <- p + geom_line() + geom_point()
            if (atts$ci) p <- p + geom_errorbar(aes_string(ymin = "low", ymax = "high"), alpha = .25)
            if (is.null(xlab))
                xlab <- plot_var
            if (is.null(ylab))
                ylab <- paste("Predicted", y)
            p <- p + labs(x = xlab, y = ylab, title = title)
        } else if (class(pd[, 3]) == "factor" | class(pd[, 3]) == "character") {
            y <- colnames(pd)[3]
            p <- ggplot(pd, aes_string(x = plot_var, y = y, group = facet_var))
            p <- p + facet_wrap(as.formula(paste0("~", facet_var)))
            p <- p + labs(x = plot_var, y = paste("Predicted", y), title = title)
            if (geom == "line")
                p <- p + geom_point() + geom_line()
            else if (geom == "bar")
                p <- p + geom_bar(stat = "identity")
        } else stop("Unsupported input. Open an issue on GitHub!")
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
#' @param labels character vector giving labels to variables,
#' must have length equal to the number of rows in 'imp'
#' @param sort character indicating if sorting of the output is to be done.
#' can be "none", "ascending", or "descending"
#' @param geom character describing type of plot desired: "point" or "bar"
#' @param facet logical indicating whether to facet, only applicable when returning class-specific variable importance
#' @param scales can be "free", "free_x", "free_y" or "fixed", applicable when facetting
#' @param xlab x-axis label, default "Variables"
#' @param ylab y-axis label, default "Importance"
#' @param title title for the plot
#'
#' @return a ggplot2 object
#' 
#' @examples \dontrun{
#' library(randomForest)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris, importance = TRUE)
#' imp <- variable_importance(fit, "accuracy", TRUE)
#' plot_imp(imp, geom = "bar")
#' }
#' @export
plot_imp <- function(imp, sort = "none", labels = NULL,
                     geom = "point", facet = FALSE,
                     xlab = "Variables", ylab = "Importance", title = "") {
    atts <- attributes(imp)
    if (!is.null(labels) & length(labels) == nrow(imp))
        imp$labels <- labels
    if (atts$type == "local")
        stop("cannot plot local importance")
    if (atts$class_levels)
        imp <- melt(imp, "labels")
    if (sort == "ascending")
        imp$labels <- factor(imp$labels, levels = imp$labels[order(imp$value, decreasing = FALSE)])
    else if (sort == "descending")
        imp$labels <- factor(imp$labels, levels = imp$labels[order(imp$value, decreasing = TRUE)])
    else if (sort == "none")
        imp$labels <- as.factor(imp$labels)
    else
        stop("invalid input to sort argument")
    
    if (facet & atts$class_levels)
        p <- ggplot(imp, aes_string("labels", "value", group = "variable")) + facet_wrap(~ variable)
    else if (!facet & atts$class_levels & geom != "bar") {
        p <- ggplot(imp, aes_string("labels", "value", colour = "variable"))
        p <- p + scale_colour_discrete(name = "Class")
    }
    else if (!facet & geom == "bar" & atts$class_levels) {
        p <- ggplot(imp, aes_string("labels", "value", fill = "variable"))
        p <- p + scale_fill_discrete(name = "Class")
    } else
        p <- ggplot(imp, aes_string("labels", "value"))

    if (geom == "point")
        p <- p + geom_point()
    else if (geom == "bar" & !atts$class_levels)
        p <- p + geom_bar(stat = "identity")
    else if (geom == "bar" & atts$class_levels)
        p <- p + geom_bar(stat = "identity", position = "dodge")
    else
        stop("invalid geom")

    p <- p + labs(y = ylab, x = xlab, title = title)
    p + coord_flip() + theme_bw()
}
#' Plot (sparse) principle components of the proximity matrix
#'
#' @references https://github.com/vqv/ggbiplot
#' @references Gabriel, "The biplot graphic display of matrices with application to principal component analysis," \emph{Biometrika}, 1971
#'
#' @import ggplot2
#'
#' @param pca a prcomp object, pca of an n x n matrix giving the proportion of times across all trees that observation i,j are in the same terminal node
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
#' \dontrun{
#' fit <- randomForest(hp ~ ., mtcars, proximity = TRUE)
#' prox <- extract_proximity(fit)
#' pca <- prcomp(prox, scale = TRUE)
#' plot_prox(prox, labels = row.names(mtcars))
#' 
#' fit <- randomForest(Species ~ ., iris, proximity = TRUE)
#' prox <- extract_proximity(fit)
#' pca <- prcomp(prox, scale = TRUE)
#' plot_prox(pca, color = iris$Species, color_label = "Species", size = 2)
#' }
#' 
#' @export
plot_prox <- function(pca, labels = NULL,
                      alpha = 1, alpha_label = NULL,
                      color = "black", color_label = NULL,
                      shape = "1", shape_label = NULL,
                      size = 2, size_label = NULL,
                      xlab = NULL, ylab = NULL, title = "", ...) {
    if (is.numeric(color))
        stop("gradient coloring not supported. add this outside of this function.")
    nobs_factor <- sqrt(nrow(pca$x) - 1)
    d <- pca$sdev
    u <- sweep(pca$x, 2, 1 / (d * nobs_factor), FUN = "*")
    prop_var <- 100 * pca$sdev[1:2]^2 / sum(pca$sdev^2)
    plt <- as.data.frame(sweep(u[, 1:2], 2, d[1:2], FUN = "*"))
    plt <- plt * nobs_factor
    plt$ymax <- max(plt[, 2])

    plt$alpha <- alpha
    plt$color <- color
    plt$shape <- shape
    plt$size <- size
    
    p <- ggplot(plt, aes_string("PC1", "PC2", color = "color", shape = "shape",
                                alpha = "alpha", size = "size", ymax = "ymax"))
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
        p <- p + geom_point(position = "dodge")
    else
        p <- p + geom_text(position = "dodge", label = labels)

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
#' @param variance numeric non-negative vector of estimated variances
#' @param confidence numeric coverage probability desired,
#' defaults to .95
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
#' \dontrun{
#' fit <- randomForest(hp ~ ., mtcars, keep.inbag = TRUE)
#' out <- var_est(fit, mtcars)
#' plot_pred(out$prediction, mtcars$hp, out$variance,
#'           outlier_idx = which(abs(out$prediction - mtcars$hp) > .5 * sd(mtcars$hp)),
#'           labs = row.names(mtcars))
#' }
#' @export
plot_pred <- function(predicted, observed, variance = NULL, confidence = .95,
                      perfect_line = TRUE, outlier_idx = NULL, labs = NULL,
                      xlab = "Observed", ylab = "Predicted", title = "") {
    if (!(is.numeric(predicted) & is.numeric(observed)))
        stop("predicted, and observed must be numeric")
    out <- data.frame("predicted" = predicted, "observed" = observed)
    out$labs <- labs
    if (!is.null(variance)) {
        if (!(all(variance > 0)))
            stop("The variance must be non-negative and numeric")
        cl <- qnorm((1 - confidence) / 2, lower.tail = FALSE)
        se <- sqrt(variance)
        out$high <- out$predicted + cl * se
        out$low <- out$predicted - cl * se
    }
    p <- ggplot(out, aes_string("observed", "predicted"))
    p <- p + geom_point()
    if (all(c("high", "low") %in% colnames(out)))
        p <- p + geom_errorbar(data = out, aes_string(ymax = "high", ymin = "low"), alpha = .25)
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
                           size = 3, hjust = 0, vjust = 0, position = "dodge", parse = FALSE)
    }
    p <- p + labs(x = xlab, y = ylab, title = title)
    p + theme_bw()    
}
