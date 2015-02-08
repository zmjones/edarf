#' Plot partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#'
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param geom "line", "area", or "bar". "line" is applicable to most situations, "area" works well for plotting class probabilities, and "bar" works well for showing the majority class (the axes are flipped in this case)
#' @param facet_var A character vector indicating the variable that should be used
#' to facet on if inteaction is plotted. If not specified the variable with less 
#' unique values is chosen.
#' @param title title for the plot
#'
#' @return a ggplot2 object
#' 
#' @examples
#' library(randomForest)
#' library(edarf)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, iris, "Petal.Width", type = "prob")
#' plot(pd, geom = "area")
#' @export
plot.pd <- function(pd, geom = "line", title = "", facet_var = NULL) {
    atts <- attributes(pd)
    ## One predictor Plots
    if (!atts$interaction) {
        if (!atts$prob) {
            ## Numeric Y or Majority class
            df <- data.frame(x = pd[, 2], y = pd[, 1])
            p <- ggplot(df, aes(x, y))
            p <- p + geom_bar(stat = "identity")
            p <- p + labs(y = colnames(pd)[1],
                          x = paste("Predicted", colnames(pd)[2]),
                          title = title)
            p <- p + theme_bw()
            p
        } else {
            ## Predicted probabilities
            df <- melt(pd, id.vars = 1)
            if (exists("xlab")) xlab <- colnames(pd)[1]
            colnames(df) <- c("x", "Class", "Probability")
            if (geom == "line") {
                p <- ggplot(df, aes(x, Probability, color = Class))
                p <- p + geom_line() + geom_point()
                p <- p + theme_bw() + labs(x = xlab, title = title)
                p
            } else {
                p <- ggplot(df, aes(x, Probability, fill = Class))
                p <- p + geom_area(position = "fill")
                p <- p + scale_fill_grey()
                p <- p + theme_bw() + labs(x = xlab, title = title)
                p
            }
        }
    } else {
        ## Interaction Plots
        assert_that(ncol(pd) == 3)
        if (atts$prob)
            stop("Interaction plot for predicted class probabilities not implemented")
        if (class(pd[, 3]) == "numeric") {
            if(exists("facet_var")) {
                n_unique <- apply(pd[, 1:2], 2, function(x) length(unique(x)))
                facet_var <- names(which.min(n_unique))
            }
            pd[, facet_var] <- as.factor(pd[, facet_var])
            plot_var <- colnames(pd)[which(colnames(pd)[-3] != facet_var)]
            y <- colnames(pd)[3]
            p <- ggplot(pd, aes_string(x = plot_var, y = y, colour = facet_var))
            p <- p + geom_line() + geom_point()
            p <- p + theme_bw()
            p <- p + labs(color = facet_var,
                          x = plot_var,
                          y = paste("Predicted", y),
                          title = title)
            p
        } else {
            stop("Interaction plot for discrete outcomes not implemented")
        }
    }
}
