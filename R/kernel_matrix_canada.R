#' Compute the matrix for BRDF inversion
#'
#' \code{kernel_matrix_canada} Computes the B, K, and rho matrices for a given site in preparation to do the GSVD
#'
#' @param year Current year of data
#' @param in_data Input data to analyze

#' @return A list that contains the K, B, and rho matrix, and time points
#' @examples
#' # TBD

#' @import dplyr
#' @export


kernel_matrix_canada <-function(year,in_data) {

  t <- if_else((year %%4 == 0 & year %%100 != 0) | year %%400 == 0, 366, 365) # Number of days, accounting for leap years
  n <- 3  # number of kernels

  B <- formBMatrix(n*t,t)

  # Determine the reflectance data
  rho <- in_data |>
    pivot_longer(cols = contains("band")) |>
    select(name,value)


  K <- as.array(array(0, dim=c(dim(in_data)[1],n*t)))

  kernelData <- in_data |> select(contains("K_")) |> as.matrix()

  colIndices <- lubridate::yday(in_data$Date)


  # Ok, now we want to get all the K values set up:
  for (i in 1:length(colIndices)) {
    columnSelect <- seq(from=colIndices[i],to=n*t,by=t)
    K[i,columnSelect] <- kernelData[i,]

  }


  return(list(K=K,B=B,rho=rho,time=colIndices))

}





