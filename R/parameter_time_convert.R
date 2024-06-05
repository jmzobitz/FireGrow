#' Update a parameter value to a daily timescale
#'
#' \code{parameter_time_convert} Updates a parameter value to a daily timestep (from hourly or yearly)
#'
#' @param name name of parameters
#' @param units timescale to convert - one of hr^-1 or yr^-1
#' @param value current value of parameter
#'
#' @return Data frame that contains the updated name and value
#' @examples
#'  # TBD
#' @export

parameter_time_convert <- function(parameters) {

  new_units = parameters$units
  parameters |>
  mutate(across(.cols=c("min_value","value","max_value"),.fns=~if_else(new_units == "yr^-1",.x/365,.x))) |> # Change from yearly values
    mutate(across(.cols=c("min_value","value","max_value"),.fns=~if_else(new_units == "hr^-1",.x*24,.x)))
}


