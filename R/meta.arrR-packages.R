#' @title meta.arrR
#'
#' @description
#' Metaecosystem of the arrR model
#'
#' For more information about how to use the model, please see \code{browseVignettes("meta.arrR")}.
#'
#' @name meta.arrR
#' @docType package
#' @useDynLib meta.arrR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

globalVariables(names = c(
  "bottom_left",
  "bottom_right",
  "input",
  "measure",
  "meta",
  "timestep",
  "top_left",
  "top_right",
  "value"
))
