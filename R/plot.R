#' Plot permutation importance from random forests
#'
#' @import ggplot2
#' @importFrom assertthat assert_that noNA
#'
#' @param var character or factor vector of variable labels
#' @param imp numeric vector of variable permutation importance estimates
#' @param ylab label for the y-axis
#' @param xlab label for the y-axis
#' @param title title for the plot
#'
#' @return a ggplot2 object
#' @export
plot_imp <- function(var, imp, ylab = NULL, xlab = NULL, title = NULL) {
    assert_that(is.numeric(imp))
    assert_that(is.factor(var) | is.character(var))
    assert_that(length(var) == length(imp))
    assert_that(!(any(is.na(var) | any(is.na(imp)))))
    assert_that(noNA(var))
    assert_that(noNA(imp))
    
    df <- data.frame(imp, var)
    df$var <- factor(df$var, levels = df$var[order(df$imp)])
    
    p <- ggplot(df, aes(var, imp))
    p <- p + geom_point()
    if ("scales" %in% rownames(installed.packages()))
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks())
    p <- p + geom_hline(aes(yintercept = 0), linetype = "dashed")
    p <- p + labs(y = xlab, x = ylab, title = title) ## yes these are deliberately flit
    p <- p + theme_bw()

    p <- p + coord_flip() ## this is why
    p
}
#' Plot two-way partial dependence from random forests
#'
#' @import ggplot2
#' @importFrom assertthat assert_that
#' 
#' @param var numeric, integer, or factor values the variable takes on.
#' @param pred numeric, factor, or integer vector of model predictions
#' results in a scatterplot for integer or numeric, and a bar plot for factors
#' @param var_lab character or factor that specifies the variables' labels
#' if multiple variables are in 'var'
#' @param grid integer vector of length 2, which specifies the number of rows
#' and columns in the plot grid if 'var_lab' is specified
#' @param smooth logical, whether or not a GAM smoother should be overlaid, only possible if
#' 'var' and 'pred' are numeric or integer
#' @param ylab label for the y-axis
#' @param xlab label for the y-axis
#' @param title title for the plot
#' 
#' @return a ggplot2 object
#' @export
plot_twoway_partial <- function(var, pred, var_lab, grid, smooth = FALSE,
                                ylab = NULL, xlab = NULL, title = NULL) {
    assert_that(is.numeric(pred) | is.factor(pred) | is.integer(pred))
    assert_that(length(var) == length(pred))
    assert_that(!(any(is.na(var) | any(is.na(pred)))))
    assert_that(noNA(var))
    assert_that(noNA(pred))
    assert_that(noNA(grid))
    assert_that(is.flag(smooth))
    if (!missing(var_lab)) {
        assert_that(noNA(var_lab))
        assert_that(is.factor(var_lab) | is.character(var_lab))
        assert_that(!missing(grid))
        assert_that(is.integer(grid) & length(grid == 2))
        df <- data.frame(pred, var, var_lab)
    } else {
        assert_that(missing(var_lab) & missing(grid))
        df <- data.frame(pred, var)
    }

    p <- ggplot(df, aes(var, pred))
    if (is.numeric(pred) | is.integer(pred)) {
        p <- p + geom_point()
        if (smooth == TRUE & (is.integer(var) | is.numeric(var)))
            p <- p + stat_smooth(method = "gam", se = FALSE, formula = y ~ s(x))
        if ("scales" %in% rownames(installed.packages()))
            p <- p + scale_y_continuous(breaks = scales::pretty_breaks())
    } else
        p <- p + geom_bar(stat = "identity")
    if (!missing(var_lab))
        p <- p + facet_wrap(~ var_lab, nrow = grid[1], ncol = grid[2], scales = "free")
    p <- p + labs(y = ylab, x = xlab, title = title)
    p <- p + theme_bw()
    p 
}
