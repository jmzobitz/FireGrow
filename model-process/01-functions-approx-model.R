### Author: JMZ
### Last modified: 24/06/01
### Purpose: Define functions that interpolate the signal for a data stream, globally bound so accessible outside of the function

# Interpolates a continuous signal from a timeseries measurement, using linear approximation.
signal_approx<- function(input_data_stream) {
  times <<- seq(0, length(input_data_stream)-1, by = 1) # This will now be defined globally

  signal <- data.frame(times = times, data = input_data_stream)
  output <- approxfun(signal, rule = 2)
  return(output)
}


# Globally assign values for an input data stream
assign_signals <- function(input_data) {
  gpp_signal <<- signal_approx(input_data$gpp)
  swc_signal <<- signal_approx(input_data$swc)
  soilT_signal <<- signal_approx(input_data$soilT)
}
