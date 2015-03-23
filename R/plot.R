#' Plot partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param geom character describing type of plot desired: "bar", "line", or "area"
#' @param title title for the plot
#' @param facet_var A character vector indicating the variable that should be used
#' to facet on if inteaction is plotted. If not specified the variable with less 
#' unique values is chosen.
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
plot_pd <- function(pd, geom = "line", title = "", facet_var) {
    atts <- attributes(pd)
    ## One predictor Plots
    if (!atts$interaction) {
        if (!atts$prob & !atts$multivariate) {
            ## Numeric Y or Majority class
            p <- ggplot(pd, aes_string(colnames(pd)[1], colnames(pd)[2]))
            if (geom == "line") {
                p <- p + geom_point() + geom_line()
                if (atts$ci) p <- p + geom_ribbon(aes(ymin = low, ymax = high), alpha = .25)
            } else if (geom == "bar")
                p <- p + geom_bar(stat = "identity")
            else stop("Unsupported geom")
            p <- p + labs(y = paste("Predicted", colnames(pd)[2]),
                          x = colnames(pd)[1],
                          title = title)
        } else if (atts$prob & !atts$multivariate) {
            ## Predicted probabilities
            df <- melt(pd, id.vars = 1)
            colnames(df) <- c("x", "Class", "Probability")
            if (geom == "area") {
                p <- ggplot(df, aes_string("x", "Probability", fill = "class"))
                p <- p + geom_area(position = "fill")
                p <- p + scale_fill_grey()
            } else if (geom == "line") {
                p <- ggplot(df, aes_string("x", "Probability", colour = "Class"))
                p <- p + geom_line() + geom_point()
            } else stop("Unsupported geom")
            p <- p + labs(x = colnames(pd)[1], title = title)
        } else {
            ## Multivariate with single explanatory variable
            df <- melt(pd, id.vars = 1)
            colnames(df) <- c("x", "Outcome", "value")
            df$Outcome <- paste0("Outcome: ", df$Outcome)
            p <- ggplot(df, aes_string("x", "value", group = "Outcome"))
            p <- p + geom_line() + geom_point()
            p <- p + facet_wrap(~ Outcome, scales = "free")
            p <- p + labs(x = colnames(pd)[1], title = title, y = "Predicted Outcome")
        }
    } else if (atts$interaction) {
        ## Interaction Plots
        if (length(atts$var) > 2) stop("Only two-way interactions supported")
        if (!exists("facet_var", mode = "character")) {
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
            p <- p + facet_wrap(as.formula(paste0("~", facet_var)))
            p <- p + labs(x = plot_var, title = title)
        } else if (!atts$prob & class(pd[, 3]) == "numeric" & !atts$multivariate) {
            y <- colnames(pd)[3]
            p <- ggplot(pd, aes_string(x = plot_var, y = y, group = facet_var))
            p <- p + facet_wrap(as.formula(paste0("~", facet_var)))
            p <- p + geom_line() + geom_point()
            if (atts$ci) p <- p + geom_ribbon(aes_string(ymin = "low", ymax = "high"), alpha = .25)
            p <- p + labs(x = plot_var,
                          y = paste("Predicted", y),
                          title = title)
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
#' @param geom character describing type of plot desired: "point" or "bar"
#' @param horizontal logical x-axis labels are horizontal if TRUE
#' @param facet logical indicating whether to facet, only applicable when returning class-specific variable importance
#' @param title title for the plot
#'
#' @return a ggplot2 object
#' 
#' @examples \dontrun{
#' library(randomForest)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris, importance = TRUE)
#' imp <- variable_importance(fit, "accuracy", TRUE)
#' plot_imp(imp, "bar")
#' }
#' @export
plot_imp <- function(imp, geom = "point", horizontal = TRUE, facet = FALSE, title = "") {
    atts <- attributes(imp)
    if (atts$type == "local")
        stop("cannot plot local importance")
    if (atts$class_levels) 
        imp <- melt(imp, "labels")
    if (facet & atts$class_levels)
        p <- ggplot(imp, aes_string("labels", "value", group = "variable")) + facet_wrap(~ variable)
    else if (!facet & atts$class_levels & geom != "bar")
        p <- ggplot(imp, aes_string("labels", "value", colour = "variable"))
    else if (!facet & geom == "bar" & atts$class_levels) {
        p <- ggplot(imp, aes_string("labels", "value", fill = factor("variable")))
        p <- p + scale_fill_discrete(name = "class")
    } else
        p <- ggplot(imp, aes_string("labels", "value"))

    p <- p + theme_bw()

    if (geom == "point")
        p <- p + geom_point()
    else if (geom == "bar" & !atts$class_levels)
        p <- p + geom_bar(stat = "identity")
    else if (geom == "bar" & atts$class_levels)
        p <- p + geom_bar(stat = "identity", position = "dodge")
    else
        stop("invalid geom")

    if (horizontal)
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    
    p <- p + labs(y = "importance", x = "variables", title = title)
    p
}
#' Plot (sparse) principle components of the proximity matrix
#'
#' @import ggplot2
#'
#' @param prox n x n matrix giving the proportion of times across all trees that observation i,j are in the same terminal node
#' @param labels length n character vector giving observation labels
#' @param size integer which gives the size of points or text labels
#' @param color optional vector of length n which gives a factor with which to color the points
#' @param color_label character legend title if color parameter is used
#' @param title character plot title
#' @param ... arguments to pass to \code{\link{prcomp}}
#'
#' @return a ggplot object
#'
#' @examples
#' \dontrun{
#' fit <- randomForest(hp ~ ., mtcars, proximity = TRUE)
#' prox <- extract_proximity(fit)
#' plot_prox(prox, labels = row.names(mtcars))
#' 
#' fit <- randomForest(Species ~ ., iris, proximity = TRUE)
#' prox <- extract_proximity(fit)
#' plot_prox(prox, color = iris$Species, color_label = "Species", size = 2)
#' }
#' 
#' @export
plot_prox <- function(prox, labels = NULL, size = 3, color = NULL, color_label = NULL, title = "", ...) {
    if (is.numeric(color))
        stop("gradient coloring not supported. add this outside of this function.")
    pca <- prcomp(prox, ...)
    nobs.factor <- sqrt(nrow(pca$x) - 1)
    d <- pca$sdev
    u <- sweep(pca$x, 2, 1 / (d * nobs.factor), '*')
    v <- pca$rotation
    prop_var <- 100 * pca$sdev[1:2]^2 / sum(pca$sdev^2)
    plt <- as.data.frame(sweep(u[, 1:2], 2, d[1:2], '*'))
    plt$ymax <- max(plt[, 2])
    
    if (is.null(color)) {
        p <- ggplot(plt, aes_string("PC1", "PC2", ymax = "ymax"))
    } else {
        plt$color <- color
        rm(color)
        p <- ggplot(plt, aes_string("PC1", "PC2", color = "color", ymax = "ymax"))
        p <- p + scale_color_discrete(name = color_label)
    }
    if (is.null(labels))
        p <- p + geom_point(position = "dodge", size = size)
    else p <- p + geom_text(position = "dodge", label = labels, size = size)
    p <- p + labs(x = paste0("PC1 (", round(prop_var[1], 0), "% explained var.)"),
                  y = paste0("PC2 (", round(prop_var[2], 0), "% explained var.)"),
                  title = title)
    p + theme_bw()
}
