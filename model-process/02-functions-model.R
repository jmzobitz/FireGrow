### Author: JMZ
### Last modified: 24/06/01
### Purpose: Define the different model functions used to compute respiration as well as initial conditions

### Make a list for the initial conditions we are facing - these are functions because we will be inputting different parameters


# Define the null model
solve_null_model <- function(times, parameters, initial_conditions, dt = 1) {
  ####
  with(as.list(c(parameters)), {
    # Adjust GPP and rates when soilT is frozen
    soilT <- soilT_signal(times)
    gpp <- gpp_signal(times) * (soilT > 0)
    swc <- swc_signal(times)

    swc <- pmax(3.11 * swc - 2.42 * swc^2, 0) # Adjustment Moyano 2013

    root_litter_rate <- root_turnover
    root_R_rate <- kR * Q10R^(soilT / 10) * swc * (soilT > 0)


    # Microbes - null model  - contains f scaling parameter
    microbe_R_rate <- f * kS * Q10M^(soilT / 10) * swc


    ### Define an iterative method
    CR_new <- array(initial_conditions[1], dim = length(times))
    CS_new <- array(initial_conditions[2], dim = length(times))


    CR_new <- (alphaR - betaR) * gpp / (root_litter_rate + root_R_rate)

    CS_new <- betaR * gpp + shrub_litter + moss_litter + wood_litter + root_litter_rate * CR_new

    ####

    ### Compute respiration
    out_resp <- tibble(
      root_R = root_R_rate * CR_new,
      microbe_R = CS_new, # previously: microbe_R_rate*CS_new,
      soil_R = root_R + microbe_R,
      RA = root_R / soil_R
    )

    ###
    out_state <- tibble(
      CR = CR_new,
      CS = CS_new
    )

    return(list(respiration = out_resp, states = out_state))
  })
}

# Define the microbe model
solve_microbe_model <- function(times, parameters, initial_conditions, dt = 1) {
  ### In this model there is a scaling parameter for microbe respiration
  # Microbes - single pool model contains f scaling parameter
  # microbeGrowth_out <- f*mu*CS/(kA+CS)*CM
  # microbe_R <- f*kM*Q10M^(soilT/10)*CM*swc # Fortunately this is the same


  ####
  with(as.list(c(parameters)), {
    # Adjust GPP and rates when soilT is frozen
    soilT <- soilT_signal(times)

    gpp <- gpp_signal(times) * (soilT > 0)
    swc <- swc_signal(times)

    swc <- pmax(3.11 * swc - 2.42 * swc^2, 0) # Adjustment Moyano 2013

    root_litter_rate <- root_turnover
    root_R_rate <- kR * Q10R^(soilT / 10) * swc * (soilT > 0)


    aboveground_input_rates <- betaR * gpp + shrub_litter + moss_litter + wood_litter


    # notice the extra scaling parameter f
    microbe_R_rate <- kM * Q10M^(soilT / 10) * swc
    microbe_growth_rate <- mu



    ### Define an iterative method
    CR_new <- array(initial_conditions[1], dim = length(times))
    CS_new <- array(initial_conditions[2], dim = length(times))
    CM_new <- array(initial_conditions[3], dim = length(times))


    y_old <- c(CS_new[[1]], CM_new[[1]])

    CR_new <- (alphaR - betaR) * gpp / (root_litter_rate + root_R_rate)

    RH_new <- (1 - epsilon) * (betaR * gpp + shrub_litter + moss_litter + wood_litter + root_litter_rate * CR_new)
    RG_new <- epsilon * (betaR * gpp + shrub_litter + moss_litter + wood_litter + root_litter_rate * CR_new)

    out_resp <- tibble(
      root_R = root_R_rate * CR_new,
      microbe_R = RH_new,
      microbeGrowth = RG_new,
      soil_R = root_R + microbe_R + microbeGrowth,
      RA = root_R / soil_R
    )

    out_state <- tibble(
      CR = CR_new,
      CS = CS_new,
      CM = CM_new
    )

    return(list(respiration = out_resp, states = out_state))
  })
}



# Define the quality model
solve_quality_model <- function(times, parameters, initial_conditions, dt = 1) {
  ### In this model microbe respiration has a linear scaling parameter (_incubation_field_)
  ####
  ####
  with(as.list(c(parameters)), {
    # Adjust GPP and rates when soilT is frozen
    soilT <- soilT_signal(times)
    gpp <- gpp_signal(times) * (soilT > 0)
    swc <- swc_signal(times)

    swc <- pmax(3.11 * swc - 2.42 * swc^2, 0) # Adjustment Moyano 2013

    root_litter_rate <- root_turnover
    root_R_rate <- kR * Q10R^(soilT / 10) * swc * (soilT > 0)

    # Microbes - quality model contains f scaling paramater
    microbe_R_rate <- kM * Q10M^(soilT / 10) * swc
    microbe_growth_rate <- mu




    ### Define an iterative method
    CR_new <- array(initial_conditions[1], dim = length(times))
    C1_new <- array(initial_conditions[2], dim = length(times))
    C2_new <- array(initial_conditions[3], dim = length(times))
    C3_new <- array(initial_conditions[4], dim = length(times))
    CA_new <- array(initial_conditions[5], dim = length(times))
    CM_new <- array(initial_conditions[6], dim = length(times))


    ### NEW CODE HERE
    CR_new <- (alphaR - betaR) * gpp / (root_litter_rate + root_R_rate)

    c1_new <- (betaR * gpp + shrub_litter) / (k1 + r1)
    c2_new <- (moss_litter + root_litter_rate * CR_new + r1 * c1_new) / (k2 + r2)
    c3_new <- (wood_litter + r2 * c2_new) / (k3)

    RG_new <- epsilon * (k1 * c1_new + k2 * c2_new + k3 * c3_new)
    RH_new <- (1 - epsilon) * (k1 * c1_new + k2 * c2_new + k3 * c3_new)

    out_resp <- tibble(
      root_R = root_R_rate * CR_new,
      microbe_R = RH_new,
      microbeGrowth = RG_new,
      soil_R = root_R + microbe_R + microbeGrowth,
      RA = root_R / soil_R
    )

    ###


    ###
    out_state <- tibble(
      CR = CR_new,
      C1 = C1_new,
      C2 = C2_new,
      C3 = C3_new,
      CA = CA_new,
      CM = CM_new
    )

    return(list(respiration = out_resp, states = out_state))
  })
}



# Create a vector of functions and names in an output list

functions <- c(
  solve_null_model,
  solve_microbe_model,
  solve_quality_model
)

model_names <- c(
  "null",
  "microbe",
  "quality"
)

model_functions_list <- tibble(
  equations = functions,
  model = model_names
)

#### Now do initial conditions
# Assign initial conditions

# Null model: CR, CS
null_initial_conditions <- function(input_parameters) {
  c(
    CR = pull(filter(input_parameters, name == "CR_start"), value),
    CS = pull(filter(input_parameters, name == "CS_start"), value)
  )
}



# Microbe model: CR, CS, CM
microbe_initial_conditions <- function(input_parameters) {
  c(
    CR = pull(filter(input_parameters, name == "CR_start"), value),
    CS = pull(filter(input_parameters, name == "CS_start"), value),
    CM = pull(filter(input_parameters, name == "CM_start"), value)
  )
}

# Quality C1, C2, C3, CA, CM
quality_initial_conditions <- function(input_parameters) {
  c(
    CR = pull(filter(input_parameters, name == "CR_start"), value),
    C1 = pull(filter(input_parameters, name == "C1_start"), value),
    C2 = pull(filter(input_parameters, name == "C2_start"), value),
    C3 = pull(filter(input_parameters, name == "C3_start"), value),
    CA = pull(filter(input_parameters, name == "CA_start"), value),
    CM = pull(filter(input_parameters, name == "CM_start"), value)
  )
}



initial_conditions_list <- tibble(
  initial_conditions = c(
    null_initial_conditions,
    microbe_initial_conditions,
    quality_initial_conditions
  ),
  model = c(
    "null",
    "microbe",
    "quality"
  )
)
