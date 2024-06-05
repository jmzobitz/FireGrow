#' Compute one step of a linear differential equation
#'
#' \code{compute_solution} solve one step of the differential equation y' + p(t)y = f(t)
#'
#' @param f forcing function
#' @param p time-dependent coefficient of linear DE
#' @param y_old previous time value
#' @param int_p_old previous value of the accumulated integral of p(t)
#' @param dt timestep
#'
#' @return Vector of the new_y value and the new value of the accumulated integral p(t)

#' @export


compute_solution <- function(f,p,y_old,int_p_old,dt) {
  # Need two steps of f,p to move forward
  # int_p_old: previous value of the accumulated integral of p(t)
  # y_old: previous time value
  # dt: timestep
  # new_y = old_y + exp(-int(p))*[int exp(int(p))*f]

  # int_p = old value of the integral
  int_p_new  <- mean(p)*dt
  int_p <- c(int_p_old,int_p_new)
  f_int <- mean(f*exp(int_p)*dt)

  new_y <- exp(-int_p_new)*(y_old + f_int)

  # p and f: [old new]
  # int_p = old value of the integral

  int_p <- c(int_p_old,int_p_new)
  f_int <- mean(f*exp(int_p)*dt)

  # y' + p(t)y = f(t) -->
  # y_new - y_old = (f(t)-p(t)y_old)*dt
  # y_new = y_old + (f(t)-p(t)y_old)*dt
  # new_y <- (f[1]-p[1]*y_old)*dt+y_old


  int_p_new  <- mean(p)*dt




  return(c(new_y,int_p_new))

}

