#' Plot permutation importance from random forests
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import assertthat
#'
#' @param pd object of class \code{c("pd", "data.frame")} as returned by
#' \code{\link{partial_dependence}}
#' @param title title for the plot
#'
#' @return a ggplot2 object
#' 
#' @examples
#' library(randomForest)
#' data(iris)
#' fit <- randomForest(Species ~ ., iris)
#' pd <- partial_dependence(fit, iris, "Petal.Width")
#' plot(pd)
#' @export

plot.pd <- function(pd, title = NULL) {
  atts <- attributes(pd)
  # One predictor Plots
  if(!atts$interaction) {
    if(!atts$prob) {
      # Numeric Y or Majority class
      df <- data.frame(x = pd[, 1], y = pd[, 2])
      p <- ggplot(df, aes(x, y))
      p <- p + geom_point() + geom_line()
      p <- p + labs(y = colnames(pd)[2], x = colnames(pd)[1], title = title) 
      p <- p + theme_bw()
      p  
    } else {
      # Predicted probabilities
      df <- melt(pd, id.vars = 1)
      if(is.null(xlab)) xlab <- colnames(pd)[1]
      colnames(df) <- c("x", "Class", "Probability")
      p <- ggplot(df, aes(x = x, y = Probability, fill = Class))
      p <- p + geom_area(position = "fill") 
      #p <- p + scale_fill_brewer(palette = "Set5")
      p <- p + scale_fill_grey()
      p <- p + theme_bw() + labs(x = xlab, title = title)
      p
    }
    
  } else {
    # Interaction Plots
    assert_that(ncol(pd) == 3)
    if(atts$prob) 
      stop("Interaction plot for predicted class probabilities not implemented")
    if(class(pd[, 3]) == "numeric") {
      n_unique <- apply(pd[, 1:2], 2, function(x) length(unique(x)))
      if(n_unique[1] == n_unique[2]) {
        plt <- 1
        fct <- 2
      } else {
        plt <- which.max(n_unique)
        fct <- which.min(n_unique)  
      }
      
      pd[, fct] <- as.factor(pd[, fct])
      x <- colnames(pd)[plt]
      y <- colnames(pd)[3]
      fct <- colnames(pd)[fct]
      p <- ggplot(pd, aes_string(x = x, y = y, colour = fct))
      p <- p + geom_line() + geom_point()
      p <- p + theme_bw()
      p <- p + labs(color = fct, x = x,
                    y = paste("Predicted", y), title = title)
      p
    } else {
      stop("Interaction plot for discrete outcomes not implemented")
    }
  }

}