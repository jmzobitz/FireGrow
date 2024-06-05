#' Report the GSVD solution for a given year associated with Land Surface temperature
#'
#' \code{smoother_compute} Computes computes the smoothed timeseries of GPP from a GSVD decomposition
#'
#' @param in_data input data for LST from MODIS for a given year. Has columns Date and column named measurement
#' @param uncertainty_val associated uncertainty with the given measurement - data frame with column sigma and the type of uncertainty:
#'  - rmse: root mean square error
#'  - rmse_percent: = rmse / mean (for signal to noise
#'  - solution: solution norm
#'  - residual: residual norm

#' @return Data frame that contains the smoothed LST for a given day of the year
#' @examples
#'
#' # To be filled in later

#' @export

smoother_compute<-function(in_data,uncertainty_val) {

  curr_year <- lubridate::year(in_data$Date[1])
  # Take in the GPP data for one site
  date_start = ymd(paste0(curr_year,'-01-01'))
  date_end = ymd(paste0(curr_year,'-12-31'))

  # Set up the original vector to go out
  data_out <- tibble(Date=seq(date_start,date_end,by='1 day'))



  nrows <- dim(in_data)[1]
  ncols <- dim(data_out)[1]
  # Make the K and B matrices
  K <- diag(0,nrows,ncols)

  # Loop through each row, setting them to be 1
  for (i in 1:nrows) {
    K[i,data_out$Date %in% in_data$Date[i]] <- 1
  }

  # Set the values of K in the corresponding row to be 1.
  B <- formBMatrix(ncols,ncols)


  ### The measurement value we are using
  rho <- in_data |>
    select(measurement) |>
    as.matrix()

  # Now we can do the GSVD:
  gsvd_test <- gsvd_compute(K,B)


  # Initialize the first run
  # This will return a list with the norm and residual for each of our values. # initialize

  results <- gsvd_optimize(gsvd_test,rho,uncertainty_val)
  out_vector <- gsvd_solution_compute(gsvd_test,results,rho)

  data_out$measurement <- as.vector(out_vector)

  return(data_out)



  }
