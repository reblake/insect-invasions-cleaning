#' \code{insectcleanr} package
#'
#' Functions to clean insect data
#'
#' @docType package
#' @name insectcleanr
#' @importFrom dplyr %>%
NULL

## This is copied from here https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
## and then modified
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

