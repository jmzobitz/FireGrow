#' Compute the solution for the kernel weights for a GSVD decomposition
#'
#' \code{gsvd_optimize} Computes residual and solution norm for a GSVD matrix decomposition
#'
#' @param gsvdResult GSVD decomposition
#' @param rho right hand side of the equation - is a data frame with the band, value, and measurement
#' @param sigma uncertainty that we want to target for the RMSE
#'
#' @return A vector of lambda values that optimize for each band.  Converged = True - able to determine the norm.  FALSE = max number of iterations reached.
#'
#' @examples
#'
#' # To be filled in later

#' @export


gsvd_optimize<-function(gsvdResult,rho,sigma) {

  # This will return a list with the norm and residual for each of our values. # initialize
  results <- bisection(gsvdResult,rho,sigma)


  return(results)
}

