#' Parameter estimate using the Nash equivalent of LM algorithm.
#'
#' \code{nlxb_jz} Determines a parameter estimate.  I had to modify if from nlxb so it doesn't print out the progress messages.
#'
#' @param formula The formula (expression we use for data assimilation)
#' @param start Start values for parameters
#' @param trace Should we report progress
#' @param data What data will we use for assimilation?
#' @param lower lower bound for parameters
#' @param upper Upper bound for parameters
#' @param masked Parameters not used
#' @param weights weights on the data
#' @param control A list of controls for the algorithm.
#'
#' @seealso nlxb from the nlsr pacakge
#' @source Function nlxb in the nlsr pacakge

#' @export


nlxb_jz <- function (formula, start, trace = FALSE, data = NULL, lower = -Inf,
                     upper = Inf, masked = NULL, weights = NULL, control = list())
{
  pnames <- names(start)
  start <- as.numeric(start)
  names(start) <- pnames
  npar <- length(start)
  if (length(lower) == 1)
    lower <- rep(lower, npar)
  if (length(upper) == 1)
    upper <- rep(upper, npar)
  if (length(lower) != npar)
    stop("Wrong length: lower")
  if (length(upper) != npar)
    stop("Wrong length: upper")
  if (any(start < lower) || any(start > upper))
    stop("Infeasible start")
  if (trace) {
    cat("formula: ")
    print(formula)
    cat("lower:")
    print(lower)
    cat("upper:")
    print(upper)
  }
  ctrl <- list(watch = FALSE, phi = 1, lamda = 1e-04, offset = 100,
               laminc = 10, lamdec = 4, femax = 10000, jemax = 5000,
               rofftest = TRUE, smallsstest = TRUE)
  ncontrol <- names(control)
  nctrl <- names(ctrl)
  for (onename in ncontrol) {
    if (!(onename %in% nctrl)) {
      if (trace)
        cat("control ", onename, " is not in default set\n")
      stop(onename, " is not a control for nlxb")
    }
    ctrl[onename] <- control[onename]
  }
  if (trace)
    print(ctrl)
  phiroot <- sqrt(ctrl$phi)
  vn <- all.vars(formula)
  pnum <- start
  pnames <- names(pnum)
  #cat("vn:")
  #print(vn)
  bdmsk <- rep(1, npar)
  maskidx <- union(which(lower == upper), which(pnames %in%
                                                  masked))
  if (length(maskidx) > 0 && trace) {
    cat("The following parameters are masked:")
    print(pnames[maskidx])
  }
  bdmsk[maskidx] <- 0
  if (trace) {
    cat("Finished masks check\n")
    parpos <- match(pnames, vn)
    datvar <- vn[-parpos]
    cat("datvar:")
    print(datvar)
    for (i in 1:length(datvar)) {
      dvn <- datvar[[i]]
      cat("Data variable ", dvn, ":")
      if (is.null(data)) {
        print(eval(parse(text = dvn)))
      }
      else {
        print(with(data, eval(parse(text = dvn))))
      }
    }
  }
  trjfn <- nlsr::model2rjfun(formula, pnum, data = data)
  if (trace) {
    cat("trjfn:\n")
    print(trjfn)
  }
  resfb <- nlsr::nlfb(start = pnum, resfn = trjfn, jacfn = trjfn,
                trace = trace, data = data, lower = lower, upper = upper,
                maskidx = maskidx, weights = weights, control = ctrl)
  resfb$formula <- formula
  pnum <- as.vector(resfb$coefficients)
  names(pnum) <- pnames
  result <- resfb
  class(result) <- "nlsr"
  result
}

