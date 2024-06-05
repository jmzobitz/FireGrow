### Author: JMZ
### Last modified: 24/06/01
### Purpose: Plot the flux results at each of the different sites, summarized by the year
# Outputs it to a table that we can then import into our document with minimal processing - yay!



load('parameter-estimation-outputs/parameter-estimate-summary.Rda')

# Function that makes a string summary from a result
make_summary <- function(q1,q2,q3) {
  str_c(round(q2,1), " (", q1, ", ", q3, ")" )

}

# Function that takes a deeply nested list of estimated parameters from different simulations and computes the annual total litter and then finds the quantile
compute_litter_quantiles <- function(input_estimated_params) {
  input_estimated_params |>
    mutate(annual_litter = map_dbl(.x=params,
                                   .f=~(.x |>
                                          filter(str_detect(name,pattern = "_litter$")) |>
                                          pull(value) |>
                                          sum()*365)
                                   )
           ) |>
    reframe(value = quantile(annual_litter, c(0.25, 0.5, 0.75),na.rm=TRUE),
            quantile = c("q0.25", "q0.5", "q0.75") )
}

# Get out the strings for the cumulative fluxes
litter_results <- model_sensitivity_data |>
  select(Year,model,depth,params) |>
  mutate(litter_total = map(params,compute_litter_quantiles)) |>
  unnest(cols=c(litter_total)) |>
  rename(name = quantile) |>
  mutate(value = round(value,0)) |>
  pivot_wider() |>
  mutate(annual_litter = make_summary(q0.25,q0.5,q0.75)) |>
  select(Year,depth,model,annual_litter)



flux_results <- model_sensitivity_data |>
  select(-params,-filtered_fluxes,-Year) |>
  unnest(cols=c(filtered_fluxes_cumulative)) |>
  mutate(value = map_dbl(.x=value,.f=~round(.x,0))) |>
  group_by(depth) |>
  nest() |>
  mutate(data = map(.x=data,.f=~unite(.x,col="name",name,quantile) |> pivot_wider()) ) |>
  unnest(cols=c(data)) |>
  mutate(rootR = make_summary(root_R_q0.25,root_R_q0.5,root_R_q0.75),
         microbeGrowth = make_summary(microbeGrowth_q0.25,microbeGrowth_q0.5,microbeGrowth_q0.75),
         microbe_R = make_summary(microbe_R_q0.25,microbe_R_q0.5,microbe_R_q0.75),
         soil_R = make_summary(soil_R_q0.25,soil_R_q0.5,soil_R_q0.75)) |>
  select(Year,depth,model,rootR,microbeGrowth,microbe_R,soil_R) |>
  mutate(across(.cols='rootR':'soil_R',.fns=~if_else(is.na(.x),'',.x))) |>
  inner_join(litter_results,by=c("Year","depth","model")) |>
  relocate(Year,depth,model,annual_litter)

fr2 <- flux_results |>
  group_by(depth) |>
  nest() |>
  mutate(data = map(.x=data,.f=~pivot_longer(.x,cols=c("annual_litter":"soil_R")) |>
                      pivot_wider(names_from = model))) |>
  unnest(cols=c(data)) |>
  mutate(Year = if_else(Year == "N2012","01_2012",Year),
         Year = if_else(Year == "N1990","02_1990",Year),
         Year = if_else(Year == "N1969","03_1968",Year),
         Year = if_else(Year == "NC","04_NC",Year),
         depth = if_else(depth == "T_soil_5","01_5cm","02_10cm") ) |>
  arrange(depth,Year) |>
  relocate(depth,Year,name,`null`,`microbe`,`quality`) |>
  mutate(name = if_else(name == "annual_litter","$L_{VP}+L_{ML}+L_{T}$",name),
         name = if_else(name == "rootR","$R_{A}$",name),
         name = if_else(name == "microbeGrowth","$R_{G}$",name),
         name = if_else(name == "microbe_R","$R_{H}$",name),
         name = if_else(name == "soil_R","$R_{S}$",name))

# Now add in the lines for the total,
out_text5cm <- fr2 |>
  filter(depth == '01_5cm') |>
  mutate(Year = str_extract(Year,pattern = '(?<=_).+'),
         Year = if_else(Year == "NC","Control",Year),
         depth = str_extract(depth,pattern = '(?<=_).+') |> str_replace(pattern = "cm", " cm"),
         end_str = if_else(name =='$R_{S}$' & Year == 'Control','\\\\ \\hline','\\\\')) |>
  unite(col='out_str',depth:`quality`,sep=' & ') |>
  unite(col='out_str2',everything(),sep=' ')


out_text10cm <- fr2 |>
  filter(depth == '02_10cm') |>
  mutate(Year = str_extract(Year,pattern = '(?<=_).+'),
         Year = if_else(Year == "NC","Control",Year),
         depth = str_extract(depth,pattern = '(?<=_).+') |> str_replace(pattern = "cm", " cm"),
         end_str = if_else(name =='$R_{S}$' & Year == 'Control','\\\\ \\hline','\\\\')) |>
  unite(col='out_str',depth:`quality`,sep=' & ') |>
  unite(col='out_str2',everything(),sep=' ')

out_text <- rbind(tibble(out_str2=c("\\begin{table}",
                                    "\\scriptsize",
                                    "\\begin{tabular}{l|l|l|lll}",
                                    "Depth (cm) & Year & Output (g C m$^{-2}$ yr$^{-1}$) & Null & Microbe & Quality \\\\ \\hline")),
                  out_text5cm,
                  out_text10cm,
                  tibble(out_str2=c("\\end{tabular}",
                                    "\\caption{Cumulative annual litter fall inputs or respiration fluxes for each of the models studied, along with the parameterization depth. Values in parentheses represent the 25th and 75th percentiles. \\label{tab:flux-cumulative}}",
                                    "\\end{table}")))

write_delim(x=out_text,file = 'manuscript-figures/cumulative_fluxes.txt',quote="none",col_names = FALSE)

