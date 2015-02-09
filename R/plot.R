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
plot.pd <- function(pd, geom = "line", title = "", facet_var) {
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
            p + theme_bw()
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
            p <- p + labs(x = colnames(pd)[1], title = title)
            p + theme_bw()
        }
    } else if (atts$interaction) {
        ## Interaction Plots
        if (length(atts$var) > 2) stop("Only two-way interactions supported")
        if (!exists("facet_var", mode = "character")) {
            n_unique <- apply(pd[, atts$var], 2, function(x) length(unique(x)))
            facet_var <- names(which.min(n_unique))
        }
        if (!(is.numeric(pd[, facet_var]))) stop("Non-numeric facetting variable")
        pd[, facet_var] <- as.factor(pd[, facet_var])
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
            p <- ggplot(pd, aes_string(x = plot_var, y = y, colour = facet_var))
            p <- p + geom_line() + geom_point()
            p <- p + labs(color = facet_var,
                          x = plot_var,
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
        }
        p + theme_bw()
        
    }
}

