# Plot all the simulations for each one ...


simulation_files <- list.files('parameter-estimation-outputs/estimates',pattern = 'iterations',full.names = TRUE)

load(simulation_files[[1]])


fluxes_rss <- vector(mode="list",length=length(simulation_files))


for(i in seq_along(simulation_files)) {

  print(i)
  load(simulation_files[[i]])

  fluxes_rss[[i]] <- out_values
}

rss_data <- tibble(
  Year = map_chr(.x=simulation_files,.f=~str_extract(.x,pattern="(?<=estimates/)[:alnum:]+(?=--)")),
  model = map_chr(.x=simulation_files,.f=~str_extract(.x,pattern="(?<=--)[:graph:]+(?=--)")),
  depth = map_chr(.x=simulation_files,.f=~str_extract(.x,pattern="T_soil_[:digit:]{1,2}")),
  # value = histogram_data,
  rss = fluxes_rss
  #  filtered_sims = filtered_sims
) |> unnest(cols=c(rss))


my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990"),default = label_parsed)

plot_rss <- function(input_depth) {
  rss_data |>
    filter(depth == input_depth) |>
    mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
           model = factor(model,levels=c('null','microbe','quality'))) |>
    ggplot(aes(x=rss,fill=as.factor(iteration))) +
    geom_histogram(position="identity",alpha=0.5) +
    facet_grid(model~Year,labeller = my_labeller2,scales="free") +
    theme_fulbright() +
    theme(legend.position = "bottom") +
    labs(y="Count",x=bquote(~RMSE~'('~g~C~m^-2~d^-1*~')'),fill="Simulation:")
}




stream_5cm <- plot_rss("T_soil_5") +  ggtitle('5 cm depth') +
  theme(strip.text.y = element_blank(),legend.position = "none")

stream_10cm <- plot_rss("T_soil_10") + ggtitle("10 cm depth") +
  labs(y=NULL) + theme(legend.position = "none")

shared_legend<-lemon::g_legend(plot_rss("T_soil_5"))

out_big_stream <- grid.arrange(stream_5cm,stream_10cm,nrow=1,
                               bottom=shared_legend$grobs[[1]],vp=viewport(width=1, height=1, clip = TRUE))
  # Now save them
  ggsave(filename = 'manuscript-figures/simulation-convergence-results.png',plot = out_big_stream,width = 18,height=7)




fluxes_rss[[1]] |> group_by(iteration) |> summarize(min_rss = min(rss))
