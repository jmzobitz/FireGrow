#' Computed site diffusivity
#'
#' A dataset containing MODIS NDVI data
#'
#' \itemize{
#'   \item ID. Replicate plot name
#'   \item layer. Depth of sample (10 or 5 cm)
#'   \item Date. Date sample taken
#'   \item rate. first derivative (dC/dt)
#'   \item sdv. second derivative (d^2C/dz^2)
#'   \item diffusivity. computed diffusivity
#'   \item real. Gap filled or not?
#' }
#'
#' @docType data
#' @keywords datasets
#' @name diffusivity
#' @usage data(diffusivity)
#' @format A data frame with 967 rows and 7 variables
#' @source Processed in 'data-process/05-process-diffusivity.R'
NULL
