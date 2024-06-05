### Author: JMZ
### Last modified: 24/06/01
### Purpose: Compute uncertainty in the NDVI


# Define the uncertainties on the bands
sigma_band1 <-  0.005
sigma_band2 <- 0.014

# Compute function for error propagation
uncertainty_ndvi <- function(rho1,rho2,s1,s2) {

  drho1 <- ((rho1-rho2)/(rho1+rho2)^2)^2 + 1/(rho1+rho2)

  drho2 <- ((rho1-rho2)/(rho1+rho2)^2)^2 - 1/(rho1+rho2)

  sigma_ndvi <- sqrt((s1*drho1)^2 + (s2*drho2)^2)

  return(sigma_ndvi)

}

# Now we are cooking with gas!

ndvi_calc <- canada_modis_data |>
  mutate(ndvi_sigma = uncertainty_ndvi(band1,band2,sigma_band1,sigma_band2))

### Make a plot

ndvi_calc |>
  ggplot(aes(x=Date,y=ndvi_sigma,color=sample)) + geom_point() + facet_grid(.~site) + ylim(c(0,1))

ndvi_calc$ndvi_sigma |> summary()

### Looks like the mean is 0.03488 --> 0.035
