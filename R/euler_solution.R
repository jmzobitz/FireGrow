#' Solve one step of the euler differential equation y' = f(y) (can be a system of equations)
#'
#' \code{euler_solution} solve one step of the differential equation y' = f(y)
#'
#' @param y_old previous time value
#' @param rates the right hand side
#' @param dt timestep
#'
#' @return Vector of the new_y value and the new value of the accumulated integral p(t)

#' @export


euler_solution <- function(y_old,rates,dt) {
  # y_old: previous time value
  # dt: timestep
  # rates: the right hand side


  # int_p = old value of the integral
  y_new  <- pmax(y_old + rates*dt,0)

  return(y_new)

}

