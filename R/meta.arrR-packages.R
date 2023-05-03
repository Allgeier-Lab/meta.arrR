#' @description
#' Simulate metaecosystem of artifical reefs. For more information about how to
#' use the model, please see \code{browseVignettes("meta.arrR")}.
#'
#' @name meta.arrR
#' @docType package
#' @useDynLib meta.arrR, .registration = TRUE
#' @exportPattern ˆ[[:alpha:]]+
#' @importFrom Rcpp evalCpp
#' @importFrom rlang .data
"_PACKAGE"

# global variables
utils::globalVariables("..density..")
