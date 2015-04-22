#' edarf: Exploratory Data Analysis Using Random Forests
#'
#' This package provides utilities for exploratory data analysis (EDA) with random forests. It allows the calculation and visualization of partial dependence for single and multiple predictors, interaction detection and proximity of observations. 
#' 
#' @docType package
#' @name edarf
#' @useDynLib edarf
#' @importFrom Rcpp sourceCpp
NULL
#' Retail marijuana sales in Colorado
#'
#' A dataset containing 2,807 retail marijuana transactions in Colorado from 2010-2013 from priceofweed.com
#'
#' @format A data.frame with 2,807 observations and 8 variables:
#' \describe{
#' \itemize{
#' \item{city: name of nearest city}
#' \item{price: in U.S. dollars}
#' \item{amount: grams}
#' \item{quality: ordinal quality}
#' \item{date: date of purchase}
#' \item{ppg: price per gram}
#' \item{lat: latitude of city}
#' \item{lon: longitude of city}
#' }}
#' @source \url{http://www.priceofweed.com}
#' @name weed
#' @usage data(weed)
#' @keywords datasets
#' @docType data
"weed"
