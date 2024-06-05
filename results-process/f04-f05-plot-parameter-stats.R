### Author: JMZ
### Last modified: 24/06/01
### Purpose: Test to see if distributions are different from one another based on parameter values. Show across all models there is a difference in parameters across years.


# Parameter histograms
# https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_one-way_analysis_of_variance
# https://en.wikipedia.org/wiki/Scheirer%E2%80%93Ray%E2%80%93Hare_test. (compare Year AND model)

load('parameter-estimation-outputs/parameter-estimate-summary.Rda')

library(gt)

parameter_results <- model_sensitivity_data |>
  select(-filtered_fluxes,-filtered_fluxes_cumulative) |>
  unnest(cols=c(params)) |>
  unnest(cols=c(params)) |>
  group_by(depth,model,name) |>
  nest() |>
  rename(boxplot_data = data) |>
  mutate(boxplot_data = map(.x=boxplot_data,.f= ~mutate(.x,Year = factor(Year,levels=c('N2012','N1990','N1969','NC')))))

parameter_median <- model_sensitivity_data |>
  select(-filtered_fluxes) |>
  unnest(cols=c(params)) |>
  unnest(cols=c(params)) |>
  group_by(depth,Year,model,name) |>
  summarize(med_val = median(value)) |>
  group_by(depth,model,name) |>
  nest() |>
  rename(median_data = data) |>
  mutate(median_data = map(.x=median_data,.f= ~mutate(.x,Year = factor(Year,levels=c('N2012','N1990','N1969','NC'))))) |>
  ungroup()

model_names <- parameter_median$model |> unique()
param_names <- parameter_results$name |> unique()


# Read in parameters and if they are in the model
input_parameters <- readxl::read_xlsx('param-values/soil-model-param.xlsx') |>
  rename(N1969 = N1968) |>
  pivot_longer(cols=c("N2012","N1990","N1969","NC"),names_to="site",values_to="value") |>
  filter(model_forcing_estimate) |>
  select(name,min_value,max_value) |>
  distinct(name,min_value,max_value,.keep_all = TRUE)

in_model <- read_csv('param-values/soil-model-param-type.csv') |>
  pivot_longer(cols=c('null-incubation-field','microbe-incubation-field','quality-incubation-field'),names_to = "model",values_to="in_model") |>
  mutate(model =str_extract(model,pattern =".+(?=-incubation-field)")) |>
  select(name,model,in_model) |>
  filter(model %in% model_names,
         name %in% param_names)


model_structures <- in_model |>
  left_join(input_parameters,by="name")



parameter_plot <- function(in_box_data,in_med_data,min_val,max_val,make_plot) {


  base_plot <- ggplot() +
    geom_blank() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      aspect.ratio = 9/16,
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_blank()
    ) +
    scale_y_continuous(expand = c(0, 0))


  if(make_plot) {


    range <- max_val-min_val
    y_span <- c(max_val-1.01*range, min_val+1.01*range)

    lines = tibble(pctile = seq(0,1,by=0.25),
                   values = range*pctile)


    p1 <- base_plot +
      geom_hline(data = lines,aes(yintercept=values),alpha=0.25,linetype='dashed') +
      geom_line(data = in_med_data,aes(x=Year,y=med_val,group=1),color='red',linewidth=1.5,inherit.aes = TRUE) +
      geom_boxplot(data = in_box_data,aes(x=Year,y=value),inherit.aes = TRUE,width=0.6)


  } else { p1 <- base_plot}


  return(p1)

}


boxplot_results <- parameter_results |> left_join(parameter_median,by=c("depth","model","name")) |>
  group_by(depth) |>
  nest() |>
  mutate(data = map(.x=data,.f=~full_join(.x,model_structures,by=c("name","model"))) ) |>
  unnest(cols=c(data)) |>
  mutate(out_plot = pmap(.l=list(boxplot_data,median_data,min_value,max_value,in_model),.f=~parameter_plot(in_box_data = ..1,
                                    in_med_data = ..2,
                                    min_val = ..3,
                                    max_val = ..4,
                                    make_plot = ..5) ) ) |>
  select(model,depth,name,out_plot)







# Next phase - put 5 cm and 10 cm next to each other
make_parameter_table <- function(input_vector,save_name,table_name=NULL) {

  ordering <- tibble(vals = 3:1,
                     model = c("quality","microbe","null"))

  ordering2 <- tibble(vals = 1:2,
                      depth = c("T_soil_5","T_soil_10"))

  test_data <- input_vector |>
    # pivot_longer(cols = c("dT_soil_10","dT_soil_5")) |>
    group_by(depth,name) |>
    nest() |>
    mutate(data = map(.x=data,.f=~pivot_wider(.x,names_from = "model",values_from = "out_plot") |> relocate(`null`,`microbe`,`quality`))) |>
    pivot_wider(names_from = "depth",values_from = "data") |>
    relocate(name,T_soil_5,T_soil_10) |>
    unnest(cols=c(T_soil_5),names_sep = '_') |>
    unnest(cols=c(T_soil_10),names_sep = '_')


  # Define an empty table to fill up
  curr_table <-  test_data |>
    ungroup() |>
    mutate(across(.cols=!c("name"),.fns=~NA)) |>
    mutate(name = factor(name,
                         levels = c("wood_litter",
                                    "moss_litter",
                                    "shrub_litter",
                                    "root_turnover",
                                    "kR",
                                    "kM",
                                    "kA",
                                    "mu",
                                    "epsilon",
                                    "r1",
                                    "r2"
                         ),
                         labels = c(html("<em>L<sub>W</sub></em>"),
                                    html("<em>L<sub>M</sub></em>"),
                                    html("<em>L<sub>S</sub></em>"),
                                    html("<em>\u03B4<sub>R</sub></em>"),
                                    html("<em>k<sub>R</sub></em>"),
                                    html("<em>k<sub>M</sub></em>"),
                                    html("<em>k<sub>A</sub></em>"),
                                    "\u03BC",
                                    "\u03B5",
                                    html("<em>r<sub>1</sub></em>"),
                                    html("<em>r<sub>2</sub></em>")) ) ) |>
    gt(rowname_col = "name") |>
    tab_spanner(
      label = "5 cm depth",
      columns = contains("T_soil_5")
    ) |>
    tab_spanner(
      label = "10 cm depth",
      columns = contains("T_soil_10")
    ) |>
    cols_label(
      "T_soil_5_null" = "Null",
      "T_soil_10_null" = "Null",
      "T_soil_5_microbe" = "Microbe",
      "T_soil_10_microbe" = "Microbe",
      "T_soil_5_quality" = "Quality",
      "T_soil_10_quality" = "Quality",
    ) |>
    fmt(
      columns = name,
      rows = everything(),
      fns = function(x) {
        map(.x=x,.f=~html(paste0(.x)) )
      }
    )



  variable_names <- test_data |> ungroup() |> select(-name) |> names()
  variable_row <- test_data |> pull(name)

  # Function the updates each cell of the table with a plot
  update_table <- function(input_data,curr_name) {

    new_table <- curr_table |>
      text_transform(
        locations = cells_body(columns = all_of(curr_name)),
        fn = function(x) {

          map(pull(select(test_data,all_of(curr_name))), ggplot_image, height = px(100))

        }
      )

    return(new_table)

  }

  for(i in seq_along(variable_names)) {
    curr_table <- update_table(test_data,variable_names[i])
  }




  # Now start customizing!
  out_plot <- curr_table |>
    fmt_markdown(columns=all_of(variable_names)) |>
    sub_missing(columns = everything(),missing_text = "") |>
    tab_options(data_row.padding = px(0),
                column_labels.padding = px(0),
                heading.padding = px(0))



  # Add on the title if we have it
  if(!is.null(table_name)) { out_plot <- out_plot |>
    tab_header(title = table_name)}

  #return(out_plot)

  # Save the plot
  gtsave(out_plot,filename=save_name,vwidth = 1600)

}



# Works for field and incubation field
make_kruskal_table <- function(input_vector,save_name,table_name=NULL) {

  ordering <- tibble(vals = 3:1,
                     model = c("quality","microbe","null"))

  test_data <- input_vector |>
    pivot_wider(names_from = "model",values_from="plots") #|>
    #inner_join(ordering,by="model") |>
    #arrange(vals) |>
    #select(-vals)

  # Define an empty table to fill up
  curr_table <-  test_data |>
    ungroup() |>
    relocate(name,`null`,`microbe`,`quality`) |>
    mutate(across(.cols=!c("name"),.fns=~NA)) |>
    mutate(name = factor(name,
                         levels = c("wood_litter",
                                    "moss_litter",
                                    "shrub_litter",
                                    "root_turnover",
                                    "kR",
                                    "kM",
                                    "kA",
                                    "mu",
                                    "epsilon",
                                    "r1",
                                    "r2"
                         ),
                         labels = c(html("<em>L<sub>W</sub></em>"),
                                    html("<em>L<sub>M</sub></em>"),
                                    html("<em>L<sub>S</sub></em>"),
                                    html("<em>\u03B4<sub>R</sub></em>"),
                                    html("<em>k<sub>R</sub></em>"),
                                    html("<em>k<sub>M</sub></em>"),
                                    html("<em>k<sub>A</sub></em>"),
                                    "\u03BC",
                                    "\u03B5",
                                    html("<em>r<sub>1</sub></em>"),
                                    html("<em>r<sub>2</sub></em>")) ) ) |>
    #mutate(model = factor(model,levels=c("quality-incubation-field","microbe-incubation-field","null-incubation-field"),
                          #labels =c("Quality","Microbe","Null") )) #|>
    gt(rowname_col = "name")  |>
    fmt(
      columns = name,
      rows = everything(),
      fns = function(x) {
        map(.x=x,.f=~html(paste0(.x)) )
      }
    )



  # Get names to loop across
  variable_names <- test_data |> ungroup() |> select(-name) |> names()

  # Function the updates each cell of the table with a plot
  update_table <- function(input_data,curr_name) {

    new_table <- curr_table |>
      text_transform(
        locations = cells_body(columns = all_of(curr_name)),
        fn = function(x) {

          map(pull(select(test_data,all_of(curr_name))), ggplot_image, height = px(300))

        }
      )

    return(new_table)

  }

  for(i in seq_along(variable_names)) {
    curr_table <- update_table(test_data,variable_names[i])
  }



  # Now start customizing!
  out_plot <- curr_table |>
    fmt_markdown(columns=all_of(variable_names)) |>
    sub_missing(columns = everything(),missing_text = "") |>
    tab_options(data_row.padding = px(05)) |>
    cols_label("microbe" = "Microbe",
               "null" = "Null",
               "quality" = "Quality")



  # Add on the title if we have it
  if(!is.null(table_name)) { out_plot <- out_plot |>
    tab_header(title = table_name)}

  #return(out_plot)

  # Save the plot
  gtsave(out_plot,filename=save_name,vwidth = 3000)

}






make_parameter_table(boxplot_results,save_name = 'manuscript-figures/parameter-table.png')

######## KRUSKAL PLOTS -- upper diagonal is 5 cm depth
kruskal_plot <- function(input_data,model_plot) {

  if(model_plot) {

    diagonals <- rbind( tibble(name=c("p05","p01","pNone"),
                               value = FALSE,
                               group1 = c("N2012"),
                               group2 = c("N2012") ),

                        tibble(name=c("p05","p01","pNone"),
                               value = FALSE,
                               group1 = c("NC"),
                               group2 = c("NC") ),

                        tibble(name=c("p05","p01","pNone"),
                               value = FALSE,
                               group1 = c("N1990"),
                               group2 = c("N1990") ),

                        tibble(name=c("p05","p01","pNone"),
                               value = FALSE,
                               group1 = c("N1969"),
                               group2 = c("N1969") )

    )

    d1 <- diagonals |>
      mutate(depth = "T_soil_10",
             diag = "lower")

    d2 <- diagonals |>
      mutate(depth = "T_soil_5",
             diag = "upper")



    small_data <- input_data |>
      ungroup() |>
      select(depth,group1,group2,p01,p05,pNone) |>
      pivot_longer(cols="p01":"pNone") |>
      mutate(diag = if_else(depth == "T_soil_10","lower","upper")) |>
      rbind(d1,d2)


    sdu <- small_data |>
      filter(diag == "upper",
             value,
             name != "pNone") |>
      mutate(name = if_else(name == "p01","**","*"))

    sdl <- small_data |>
      filter(diag == "lower",value,
             name != "pNone") |>
      mutate(name = if_else(name == "p01","**","*"))

    plot_layout <- tibble(x = c("N1_2012","N2_1990","N3_1969","N4_C"),
                          y = x)
    upper_poly <- tibble(x = c(0.3, 4.6, 0.3), y = c(0.3,4.6,4.6))

    ggplot() +
      geom_polygon(data = plot_layout,aes(x=x,y=y)) +
      geom_polygon(data = upper_poly,aes(x=x,y=y),fill='grey90',color=NA) +
      geom_text(data = sdu,aes(x=group2,y=group1,label=name),size = 22,vjust = 0.75) +
      geom_text(data = sdl,aes(x=group1,y=group2,label=name),size=22,vjust = 0.75) +
      geom_abline(slope=1,intercept = 0,linewidth=0.1) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.text = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        plot.title = element_blank(),
        axis.text = element_text(size=18),
        panel.border = element_rect(colour = "grey50", fill=NA)
      ) +
      scale_x_discrete(labels= c("2012", "1990", "1969","Control")) +
      scale_y_discrete(labels= c("2012", "1990", "1969","Control")) #+
      #geom_label(aes(x=1,y=4.35),label='5 cm depth',size=8) +
      #geom_label(aes(x=3,y=0.65),label='10 cm depth',size=8)




  } else {

    ggplot() +
      geom_blank() +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        plot.title = element_blank()
      )


  }


}


# Does the pairwise Wilcoxin and kruskal
kruskal_test_stats <- model_sensitivity_data |>
  select(Year,model,depth,params) |>
  unnest(cols=c(params)) |>
  unnest(cols=c(params)) |>
  mutate(Year = if_else(Year == "N2012","N1_2012",Year),
         Year = if_else(Year == "N1990","N2_1990",Year),
         Year = if_else(Year == "N1969","N3_1969",Year),
         Year = if_else(Year == "NC","N4_C",Year)) |>
  group_by(depth,model,name) |>
  nest() |>
  mutate(kruskal = map(.x=data,.f=~(kruskal.test(value ~ Year, data = .x ) |> broom::tidy()))) |>
  hoist(kruskal,"p.value") |>
  rename(kruskal.p.value = p.value) |>
  mutate(wilcoxin = map(.x=data,.f=~(pairwise.wilcox.test(.x$value, .x$Year, p.adjust.method = "bonferroni") |>
                                       broom::tidy() |> mutate(p01 = p.value <.01,
                                                               p05 = p.value<.05 & !p01,
                                                               pNone = !p01 & !p05,
                                       ))) ) |>
  select(-data) |>
  unnest(cols=c(wilcoxin))

kruskal_test_stats |>
  group_by(model, depth, name, group1) |>
  summarize(across(.cols="p01":"pNone",.fns=sum)) |>
  rename(p_name = name) |>
  pivot_longer(cols=c("p01","p05")) |>
  group_by(model,depth,p_name) |>
  summarize(tot = sum(value)) |>
  arrange((tot)) |> View()

kruskal_test_stats |>
  group_by(model, depth, name, group1) |>
  summarize(across(.cols="p01":"pNone",.fns=sum)) |>
  rename(p_name = name) |>
  pivot_longer(cols=c("p01","p05")) |>
  group_by(model,depth,name) |>
  summarize(tot = sum(value)) |>
  arrange((tot)) |> View()

# Does the pairwise Wilcoxin and kruskal
kruskal_test_nest <- kruskal_test_stats |>
  group_by(model,name) |>
  nest() |>
  right_join(in_model,by=c("name","model"))  |>
  mutate(plots = map2(.x=data,.y=in_model,.f=~kruskal_plot(.x,.y) ))  |>
  select(-data,-in_model)




make_kruskal_table(kruskal_test_nest,
                     save_name = 'manuscript-figures/parameter-kruskal.png')


# Code I don't want to delete yet

# # Kruskal Tests!
# kruskal.test(value ~ Year, data = kruskal_test_nest$data[[1]])
#
# pairwise.wilcox.test(kruskal_test_nest$data[[1]]$value, kruskal_test_nest$data[[1]]$Year, p.adjust.method = "bonferroni") |>
#   broom::tidy()
#
#
# broom::tidy(yee)
#
# rcompanion::scheirerRayHare(value ~ Year + model,
#                             data = kruskal_test_nest$data[[2]] )
#
#
#
# DT <- FSA::dunnTest(value ~ Year ,
#               data=kruskal_test_nest$data[[1]],
#               method="bonferroni")
#
# kruskal_test_nest$data[[1]]
#
# Location = c(rep("Olympia" , 6), rep("Ventura", 6),
#              rep("Northampton", 6), rep("Burlington", 6))
#
# Tribe  = c(rep(c("Jedi", "Sith"), 12))
#
# Midichlorians = c(10,  4, 12,  5, 15,  4, 15,  9, 15, 11, 18, 12,
#                   8, 13,  8, 15, 10, 17, 22, 22, 20, 22, 20, 25)
#
# Data = data.frame(Tribe, Location, Midichlorians)
#
# str(Data)
#
#
# ### Scheirer–Ray–Hare test
#
# library(rcompanion)
#
#
#
# library(FSA)
#
#  FSA::dunnTest(Midichlorians ~ Location ,
#               data=Data,
#               method="bonferroni")      # Adjusts p-values for multiple comparisons;
# # See ?dunnTest for options
#
#  dunn.test(Midichlorians,Location,kw=FALSE,method="bonferroni")
#
# DT
#
