#' Plot partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#'
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param geom character describing type of plot desired: "bar", "line", or "area"
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
plot.pd <- function(pd, geom = "line", title = NULL, facet_var = NULL) {
    atts <- attributes(pd)
    ## One predictor Plots
    if (!atts$interaction) {
        if (!atts$prob) {
            ## Numeric Y or Majority class
            df <- data.frame(x = pd[, 1], y = pd[, 2])
            p <- ggplot(df, aes(x, y))
            if (geom == "line")
                p <- p + geom_point() + geom_line()
            else if (geom == "bar")
                p <- p + geom_bar(stat = "identity")
            else stop("Unsupported geom")
            p <- p + labs(y = paste("Predicted", colnames(pd)[2]),
                          x = colnames(pd)[1],
                          title = title)
            p <- p + theme_bw()
            p
        } else {
            ## Predicted probabilities
            df <- melt(pd, id.vars = 1)
            colnames(df) <- c("x", "Class", "Probability")
            if (geom == "area") {
                p <- ggplot(df, aes(x = x, y = Probability, fill = Class))
                p <- p + geom_area(position = "fill")
                p <- p + scale_fill_grey()
            } else if (geom == "line") {
                p <- ggplot(df, aes(x, Probability, colour = Class))
                p <- p + geom_line() + geom_point()
            } else stop("Unsupported geom")
            p <- p + theme_bw() + labs(x = colnames(pd)[1],
                                       title = title)
            p
        }
    } else {
        ## Interaction Plots
        assert_that(ncol(pd) == 3)
        if (atts$prob)
            stop("Interaction plot for predicted class probabilities not implemented")
        if (class(pd[, 3]) == "numeric") {
            if (is.null(facet_var)) {
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
