### Author: JMZ
### Last modified: 24/06/01
### Purpose: Plot the flux results at each of the different sites, summarized by the year


load('parameter-estimation-outputs/parameter-estimate-no-swc-summary.Rda')

flux_results <- model_sensitivity_data |>
  mutate(filtered_fluxes = map(.x=filtered_fluxes,.f=~(.x |> filter(name %in% c("microbeGrowth","RA","root_R","soil_R","microbe_R"))))) |>
  select(-params,-filtered_fluxes_cumulative) |>
  unnest(cols=c(filtered_fluxes)) |>
  #mutate(#Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
  #  name = factor(name,levels=c('root_R','microbeGrowth','microbe_R','soil_R','RA'))) |>
  pivot_wider(names_from="quantile")


# Load up field data to add to the plot
### STEP 4: Read in the flux data, join to soil carbon data
field_data <- readxl::read_xlsx('data-raw/Modeling data Canada North.xlsx') |>
  select(1,8,9,12,15:20) |>
  separate(1,into=c("Year","Plot")) |>
  separate(Plot,into=c("Area","Plot"),sep=1) |>
  #mutate(Year = if_else(Year=="N1969","N1968",Year)) |>  # Change 1969 to 1968
  mutate(
    Plot = as.numeric(Plot),
    respiration = `Soil Respiration, g/CO2/m2/h`* (12 / 44 ) * (24),  # Convert to g C / m2 day
  )  |>
  select(Year,Area,Plot,respiration) |>
  group_by(Year) |>
  summarize(q25_Rsoil = quantile(respiration,probs = 0.25),
            q75_Rsoil = quantile(respiration,probs = 0.75)) |>
  mutate(aug1 = 213,
         aug31 = 243,
         name = 'soil_R') |>
  full_join(tibble(Year = 'N1969',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
  full_join(tibble(Year = 'N2012',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
  full_join(tibble(Year = 'NC',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
  full_join(tibble(Year = 'N1990',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
         name = factor(name,levels=c('root_R','microbeGrowth','microbe_R','soil_R','RA')))

my_labeller <- as_labeller(c("root_R"="R[A]", "microbeGrowth"="R[G]","microbe_R"="R[H]","RA" = "p[A]","soil_R" = "R[soil]","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)


my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)

# Code to create each individual plot
  make_respiration_plots <- function(input_depth) {

    flux_results |>
      filter(depth == input_depth) |>
      mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
             name = factor(name,levels=c('root_R','microbeGrowth','microbe_R','soil_R','RA')),
             model = factor(model,levels=c('null','microbe','quality'))) |>
      ggplot(aes(x=doy+as.Date("2018-12-31"))) +
      geom_line(aes(y=q0.5,color=model)) +
      geom_ribbon(aes(ymin=q0.25,ymax=q0.75,fill=model),alpha=0.3) +
      geom_rect(data = field_data,aes(xmin=aug1+as.Date("2018-12-31"),xmax=aug31+as.Date("2018-12-31"),ymin=q25_Rsoil,ymax=q75_Rsoil),inherit.aes = FALSE,fill='grey50',alpha=0.5)+
      facet_grid(name~Year,labeller = my_labeller,scales="free_y") +
      ggh4x::facetted_pos_scales(
        y = list(
          name == "RA" ~ scale_y_continuous(limits = c(0, 0.5)),
          name == "microbeGrowth" ~ scale_y_continuous(limits = c(0, 2)),
          name == "root_R" ~ scale_y_continuous(limits = c(0, 1.5)),
          name == "microbe_R" ~ scale_y_continuous(limits = c(0, 3)),
          name == "soil_R" ~ scale_y_continuous(limits = c(0, 4))
        )
      ) +
      theme(legend.position="bottom") +
      theme_fulbright() +
      labs(x=NULL,y=bquote(~Flux~'('~g~C~m^-2~d^-1*~')'),color="Model:",fill="Model:") +
      scale_color_discrete(
        limits = c("null", "microbe", "quality"),
        labels = c("Null", "Microbe", "Quality")
      ) +
      scale_fill_discrete(
        limits = c("null", "microbe", "quality"),
        labels = c("Null", "Microbe", "Quality")
      ) +
      scale_x_date( date_labels = "%b",
                      breaks = c(as.Date('2019-01-01'),as.Date('2019-04-01'),as.Date('2019-07-01'),as.Date('2019-10-01'),as.Date('2019-12-31')))  +
      theme(axis.text.x=element_text(angle = 45, vjust = 0.5) )


  }

  # Make plots
  p1 <- make_respiration_plots("T_soil_5") + ggtitle('5 cm depth')
  p2 <- make_respiration_plots("T_soil_10") + ggtitle('10 cm depth')

  # Now save them
  ggsave(filename = 'manuscript-figures/flux-results-no-swc-model-5cm.png',plot = p1,width = 11,height=8)
  ggsave(filename = 'manuscript-figures/flux-results-no-swc-model-10cm.png',plot = p2,width = 11,height=8)

  my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)

  make_stream_plots <- function(input_depth) {

    flux_results |>
      filter(depth == input_depth) |>
      select(-q0.25,-q0.75) |>
      pivot_wider(names_from="name",values_from = "q0.5") |>
      mutate(across(.cols="microbeGrowth":"root_R",.fns = ~if_else(root_R == 0, NA,.x))) |> # If there is no flux we want that to be excluded from the plot.
      select(-RA,-soil_R) |>
      pivot_longer(cols="microbeGrowth":"root_R") |>
      mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
             name = factor(name,levels=c('microbeGrowth','microbe_R','root_R')),
             model = factor(model,levels=c('null','microbe','quality'))) |>
      ggplot(aes(x=doy+as.Date("2018-12-31"),y = value, fill = name),alpha=0.7) +
      ggstream::geom_stream(type='proportional',alpha=0.7) +
      geom_rect(data = field_data,aes(xmin=aug1+as.Date("2018-12-31"),xmax=aug31+as.Date("2018-12-31"),ymin=0,ymax=1),inherit.aes = FALSE,fill='grey50',alpha=0.5)+
      facet_grid(model~Year,labeller = my_labeller2,scales="free_y") +
      theme(legend.position="bottom") +
      theme_fulbright() +
      labs(x=NULL,y="Proportion (unitless)",fill="Flux proportion:") +
      scale_y_continuous(breaks = seq(0,1,by=0.2)) +
      scale_x_date( date_labels = "%b",
                    limits = as.Date(c('2018-12-21','2020-01-01')),
                    breaks = c(as.Date('2019-01-01'),as.Date('2019-04-01'),as.Date('2019-07-01'),as.Date('2019-10-01'),as.Date('2019-12-31')))  +
      theme(axis.text.x=element_text(angle = 45, vjust = 0.5) ) +
      scale_fill_viridis_d( labels = c(root_R=expression(R[A]), microbeGrowth=expression(R[G]),microbe_R=expression(R[H])))


  }
  stream_5cm <- make_stream_plots("T_soil_5") +  ggtitle('5 cm depth')
  stream_10cm <- make_stream_plots("T_soil_10") +  ggtitle('10 cm depth')

  # Now save them
  ggsave(filename = 'manuscript-figures/proportion-results-no-swc-model-5cm.png',plot = stream_5cm,width = 11,height=8)
  ggsave(filename = 'manuscript-figures/proportion-results-no-swc-model-10cm.png',plot = stream_10cm,width = 11,height=8)

  no_swc <- gridExtra::grid.arrange(p1,p2,stream_5cm,stream_10cm,nrow=2,ncol=2)
  ggsave(filename = 'manuscript-figures/results-no-swc-model.png',plot = no_swc,width = 22,height=16)
  #### Do this for linear SWC

  load('parameter-estimation-outputs/parameter-estimate-linear-swc-summary.Rda')

  flux_results <- model_sensitivity_data |>
    mutate(filtered_fluxes = map(.x=filtered_fluxes,.f=~(.x |> filter(name %in% c("microbeGrowth","RA","root_R","soil_R","microbe_R"))))) |>
    select(-params,-filtered_fluxes_cumulative) |>
    unnest(cols=c(filtered_fluxes)) |>
    #mutate(#Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
    #  name = factor(name,levels=c('root_R','microbeGrowth','microbe_R','soil_R','RA'))) |>
    pivot_wider(names_from="quantile")


  # Load up field data to add to the plot
  ### STEP 4: Read in the flux data, join to soil carbon data
  field_data <- readxl::read_xlsx('data-raw/Modeling data Canada North.xlsx') |>
    select(1,8,9,12,15:20) |>
    separate(1,into=c("Year","Plot")) |>
    separate(Plot,into=c("Area","Plot"),sep=1) |>
    #mutate(Year = if_else(Year=="N1969","N1968",Year)) |>  # Change 1969 to 1968
    mutate(
      Plot = as.numeric(Plot),
      respiration = `Soil Respiration, g/CO2/m2/h`* (12 / 44 ) * (24),  # Convert to g C / m2 day
    )  |>
    select(Year,Area,Plot,respiration) |>
    group_by(Year) |>
    summarize(q25_Rsoil = quantile(respiration,probs = 0.25),
              q75_Rsoil = quantile(respiration,probs = 0.75)) |>
    mutate(aug1 = 213,
           aug31 = 243,
           name = 'soil_R') |>
    full_join(tibble(Year = 'N1969',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
    full_join(tibble(Year = 'N2012',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
    full_join(tibble(Year = 'NC',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
    full_join(tibble(Year = 'N1990',name = c('root_R','microbeGrowth','microbe_R','RA')),by=c('name','Year')) |>
    mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
           name = factor(name,levels=c('root_R','microbeGrowth','microbe_R','soil_R','RA')))

  my_labeller <- as_labeller(c("root_R"="R[A]", "microbeGrowth"="R[G]","microbe_R"="R[H]","RA" = "p[A]","soil_R" = "R[soil]","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)


  my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)

  # Code to create each individual plot
  make_respiration_plots <- function(input_depth) {

    flux_results |>
      filter(depth == input_depth) |>
      mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
             name = factor(name,levels=c('root_R','microbeGrowth','microbe_R','soil_R','RA')),
             model = factor(model,levels=c('null','microbe','quality'))) |>
      ggplot(aes(x=doy+as.Date("2018-12-31"))) +
      geom_line(aes(y=q0.5,color=model)) +
      geom_ribbon(aes(ymin=q0.25,ymax=q0.75,fill=model),alpha=0.3) +
      geom_rect(data = field_data,aes(xmin=aug1+as.Date("2018-12-31"),xmax=aug31+as.Date("2018-12-31"),ymin=q25_Rsoil,ymax=q75_Rsoil),inherit.aes = FALSE,fill='grey50',alpha=0.5)+
      facet_grid(name~Year,labeller = my_labeller,scales="free_y") +
      ggh4x::facetted_pos_scales(
        y = list(
          name == "RA" ~ scale_y_continuous(limits = c(0, 0.5)),
          name == "microbeGrowth" ~ scale_y_continuous(limits = c(0, 2)),
          name == "root_R" ~ scale_y_continuous(limits = c(0, 1.5)),
          name == "microbe_R" ~ scale_y_continuous(limits = c(0, 3)),
          name == "soil_R" ~ scale_y_continuous(limits = c(0, 4))
        )
      ) +
      theme(legend.position="bottom") +
      theme_fulbright() +
      labs(x=NULL,y=bquote(~Flux~'('~g~C~m^-2~d^-1*~')'),color="Model:",fill="Model:") +
      scale_color_discrete(
        limits = c("null", "microbe", "quality"),
        labels = c("Null", "Microbe", "Quality")
      ) +
      scale_fill_discrete(
        limits = c("null", "microbe", "quality"),
        labels = c("Null", "Microbe", "Quality")
      ) +
      scale_x_date( date_labels = "%b",
                    breaks = c(as.Date('2019-01-01'),as.Date('2019-04-01'),as.Date('2019-07-01'),as.Date('2019-10-01'),as.Date('2019-12-31')))  +
      theme(axis.text.x=element_text(angle = 45, vjust = 0.5) )


  }

  # Make plots
  p1 <- make_respiration_plots("T_soil_5") + ggtitle('5 cm depth')
  p2 <- make_respiration_plots("T_soil_10") + ggtitle('10 cm depth')

  # Now save them
  ggsave(filename = 'manuscript-figures/flux-results-no-swc-model-5cm.png',plot = p1,width = 11,height=8)
  ggsave(filename = 'manuscript-figures/flux-results-no-swc-model-10cm.png',plot = p2,width = 11,height=8)

  my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)

  make_stream_plots <- function(input_depth) {

    flux_results |>
      filter(depth == input_depth) |>
      select(-q0.25,-q0.75) |>
      pivot_wider(names_from="name",values_from = "q0.5") |>
      mutate(across(.cols="microbeGrowth":"root_R",.fns = ~if_else(root_R == 0, NA,.x))) |> # If there is no flux we want that to be excluded from the plot.
      select(-RA,-soil_R) |>
      pivot_longer(cols="microbeGrowth":"root_R") |>
      mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
             name = factor(name,levels=c('microbeGrowth','microbe_R','root_R')),
             model = factor(model,levels=c('null','microbe','quality'))) |>
      ggplot(aes(x=doy+as.Date("2018-12-31"),y = value, fill = name),alpha=0.7) +
      ggstream::geom_stream(type='proportional',alpha=0.7) +
      geom_rect(data = field_data,aes(xmin=aug1+as.Date("2018-12-31"),xmax=aug31+as.Date("2018-12-31"),ymin=0,ymax=1),inherit.aes = FALSE,fill='grey50',alpha=0.5)+
      facet_grid(model~Year,labeller = my_labeller2,scales="free_y") +
      theme(legend.position="bottom") +
      theme_fulbright() +
      labs(x=NULL,y="Proportion",fill="Flux:") +
      scale_y_continuous(breaks = seq(0,1,by=0.2)) +
      scale_x_date( date_labels = "%b",
                    limits = as.Date(c('2018-12-21','2020-01-01')),
                    breaks = c(as.Date('2019-01-01'),as.Date('2019-04-01'),as.Date('2019-07-01'),as.Date('2019-10-01'),as.Date('2019-12-31')))  +
      theme(axis.text.x=element_text(angle = 45, vjust = 0.5) ) +
      scale_fill_viridis_d( labels = c(root_R=expression(p[A]), microbeGrowth=expression(p[G]),microbe_R=expression(p[H])))


  }
  stream_5cm <- make_stream_plots("T_soil_5") +  ggtitle('5 cm depth')
  stream_10cm <- make_stream_plots("T_soil_10") +  ggtitle('10 cm depth')

  # Now save them
  ggsave(filename = 'manuscript-figures/proportion-results-linear-swc-model-5cm.png',plot = stream_5cm,width = 11,height=8)
  ggsave(filename = 'manuscript-figures/proportion-results-linear-swc-model-10cm.png',plot = stream_10cm,width = 11,height=8)

  linear_swc <- gridExtra::grid.arrange(p1,p2,stream_5cm,stream_10cm,nrow=2,ncol=2)
  ggsave(filename = 'manuscript-figures/results-linear-swc-model.png',plot = no_swc,width = 22,height=16)
  #### Do this for linear SWC
