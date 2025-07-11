# Compare measured to modeled values of RS across the chronosequence site and models.

# load up parameters
load('parameter-estimation-outputs/parameter-estimate-summary.Rda')


###

### STEP 0: Read in simulation results. Keep track of files that didn't work (we need to investigate what is happening)

input_files <- list.files(path = 'parameter-estimation-outputs/estimates',pattern="^N",full.names = TRUE)

# Define a list of vectors for different outputs
filtered_fluxes <- vector(mode="list",length=length(input_files))
filtered_fluxes_cumulative <- vector(mode="list",length=length(input_files))
out_params <- vector(mode="list",length=length(input_files))
boxplot_data <- vector(mode="list",length=length(input_files))

# Join in with the necessary data
meas_data <- FireGrow::field_data |> unnest(cols=c(field_data))

out_synthesized_data <- vector(mode="list",length = length(input_files))

for(i in seq_along(input_files)) {

  print(i)
  load(input_files[[i]])


  # Get out the median value
  input_month <- 8

  # Now see if I can generalize this to all values:
  out_synthesized_data[[i]] <- out_tibble |>
    mutate(summary_R = map(.x=fluxes, .f=~(.x |>
                                             mutate(med_soilR = map_dbl(.x=results,.f=~(.x |>   filter(month %in% input_month) |>
                                                                                          summarize(medianR = median(soil_R,na.rm=TRUE)) |> pull(medianR) ))) |>
                                             select(-results) ) ) ) |>

    select(summary_R) |> unnest(cols=c(summary_R))

}


# Now start plotting
my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990","T_soil_5"="5~cm","T_soil_10" = "10~cm"),default = label_parsed)

#Custom colors
custom_colors <- c("N2012" = '#a6cee3', "N1990" = '#1f78b4',
                   "N1969" = '#b2df8a', "NC" = '#33a02c')

# Make the large data frame and join it to the measured data
model_comparison_info <- tibble(
  model = map_chr(.x=input_files,.f=~str_extract(.x,pattern="(?<=--)[:graph:]+(?=--)")),
  depth = map_chr(.x=input_files,.f=~str_extract(.x,pattern="T_soil_[:digit:]{1,2}")),
  model_data = out_synthesized_data
) |> unnest(cols=c(model_data)) |>
  inner_join(meas_data,by=c("Year","Area"))

# Compute regression stats
model_stats <- model_comparison_info |>
  group_by(depth,model) |>
  nest() |>
  mutate(lm = map(.x=data,.f=~lm(med_soilR~Rsoil,data=.x)),
         stats = map(.x=lm,.f=broom::glance)) |>
  select(-data,-lm) |>
  unnest(cols=c(stats)) |>
  mutate(rmse = sqrt(deviance / nobs)) |>
  select(r.squared,p.value,rmse) |>
  ungroup() |>
  mutate(r2_label = paste0("R^2 == ", round(r.squared, 2)),
         depth = factor(depth,levels=c('T_soil_5','T_soil_10')),
         model = factor(model,levels=c('null','microbe','quality'))
         )



p1 <- model_comparison_info |>
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
         depth = factor(depth,levels=c('T_soil_5','T_soil_10')),
         model = factor(model,levels=c('null','microbe','quality'))) |>
  #filter(depth=="T_soil_5") |>
  ggplot(aes(x=Rsoil,y=med_soilR,color=Year,fill=Year)) + geom_violin() +
  xlim(c(0,4)) + ylim(c(0,4)) + geom_abline() +
  facet_grid(depth~model,labeller = my_labeller2) +
    theme(legend.position="bottom") +
    theme_fulbright() +
    labs(x=bquote(R[italic(S)*","*italic(meas)]~"("~g~C~m^{-2}~d^{-1}~")"),y=bquote(R[italic(S)*","*italic(mod)]~"("~g~C~m^{-2}~d^{-1}~")"),color="Site:",fill="Site:") +
    scale_color_manual(
      values = custom_colors,
      limits = c("N2012", "N1990", "N1969","NC"),
      labels = c("2012", "1990", "1968","Control")
    ) +
  scale_fill_manual(
    values = custom_colors,
    limits = c("N2012", "N1990", "N1969","NC"),
    labels = c("2012", "1990", "1968","Control")
  ) +
  #guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(
    axis.text = element_text(size=10),
    axis.title=element_text(size=18),
  ) +
  geom_text(
    data = model_stats,
    aes(x = 0.75, y = 3.5, label = r2_label),
    parse = TRUE,
    inherit.aes = FALSE
  )


ggsave(p1,filename = 'manuscript-figures/mcmc-regression-results.png',width = 10,height = 6)

