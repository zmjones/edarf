#' Creates a prediction vector for variables to decrease computation time
#'
#' @param df the dataframe used to fit the random forest, extracted from the fitted object
#' @param x a character vector of length 1 indicating the variable in `df` to be extracted
#' @param cutoff an integer indicating the maximal length of the vector to be used for prediction
#' 
#' @return a vector of unique values taken by `x` of length < `cutoff`
#' 
#' @export
ivar_points <- function(df, x, cutoff = 10) {
    rng <- unique(df[, x])
    rng <- rng[!is.na(rng)]
    if (length(rng) > cutoff & !is.factor(df[, x]))
        rng <- seq(min(rng), max(rng), length.out = cutoff)
    class(rng) <- class(df[, x])
    return(rng)
}
