# Make a plot of the MCMC convergence tests for the Supplementary information

estimate_files <- list.files(path = 'parameter-estimation-outputs/estimates-test',full.names = TRUE)


make_simulation_plot <- function(curr_file) {

  load(curr_file)
  # Extract components
  matches <- str_match(curr_file, ".*/N([^-/]+)--([^-/]+)--T_soil_(\\d+)\\.Rda")

  # Assign to named vectors
  curr_year <- matches[, 2]
  model     <- matches[, 3] |> str_to_title()
  depth     <- matches[, 4]

  if (curr_year =="1969") {
    curr_year = "1968"
  }

  if (curr_year =="C") {
    curr_year = "Control"
  }



  p1 <-out_tibble |>
    mutate(sims=if_else(chain ==0,sims-1000,sims ),
           chain_label = case_when(
             chain == 0 ~ "Estimate Chain",
             chain == 1 ~ "Tuning Chain 1",
             chain == 2 ~ "Tuning Chain 2",
             chain == 3 ~ "Tuning Chain 3",
             chain == 4 ~ "Tuning Chain 4",
             chain == 5 ~ "Tuning Chain 5"
           )) |>
    ggplot(aes(x=sims,y=log_likely,group=chain,color=chain_label)) + geom_point(size=0) + geom_line() +
    theme_bw() +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 1000,linetype='dashed') +
    labs(y="log(L)",x="Simulation",color='Chain',
         subtitle=paste0("Model: ",model,"; Site: ",curr_year, "; Depth: ",depth)) +
    guides(color='none') + ylim(c(0,6))


  p2 <- ggExtra::ggMarginal(p1,type="histogram",margin="y",fill="#F8766D")

  return(p2)

}


p1 <- make_simulation_plot(estimate_files[[1]])
p2 <- make_simulation_plot(estimate_files[[7]])
p3 <- make_simulation_plot(estimate_files[[13]])
p4 <- make_simulation_plot(estimate_files[[19]])

p_mic <- gridExtra::grid.arrange(p3,p2,p1,p4,nrow=1)


p1 <- make_simulation_plot(estimate_files[[3]])
p2 <- make_simulation_plot(estimate_files[[9]])
p3 <- make_simulation_plot(estimate_files[[15]])
p4 <- make_simulation_plot(estimate_files[[21]])

p_null <- gridExtra::grid.arrange(p3,p2,p1,p4,nrow=1)

p1 <- make_simulation_plot(estimate_files[[5]])
p2 <- make_simulation_plot(estimate_files[[11]])
p3 <- make_simulation_plot(estimate_files[[17]])
p4 <- make_simulation_plot(estimate_files[[23]])

p_qual <- gridExtra::grid.arrange(p3,p2,p1,p4,nrow=1)


p_10cm <- gridExtra::grid.arrange(p_null,p_mic,p_qual,nrow=3)


p1 <- make_simulation_plot(estimate_files[[2]])
p2 <- make_simulation_plot(estimate_files[[8]])
p3 <- make_simulation_plot(estimate_files[[14]])
p4 <- make_simulation_plot(estimate_files[[20]])

p_mic <- gridExtra::grid.arrange(p3,p2,p1,p4,nrow=1)


p1 <- make_simulation_plot(estimate_files[[4]])
p2 <- make_simulation_plot(estimate_files[[10]])
p3 <- make_simulation_plot(estimate_files[[16]])
p4 <- make_simulation_plot(estimate_files[[22]])

p_null <- gridExtra::grid.arrange(p3,p2,p1,p4,nrow=1)

p1 <- make_simulation_plot(estimate_files[[6]])
p2 <- make_simulation_plot(estimate_files[[12]])
p3 <- make_simulation_plot(estimate_files[[18]])
p4 <- make_simulation_plot(estimate_files[[24]])

p_qual <- gridExtra::grid.arrange(p3,p2,p1,p4,nrow=1)


p_5cm <- gridExtra::grid.arrange(p_null,p_mic,p_qual,nrow=3)

ggsave(p_5cm,filename = 'manuscript-figures/mcmc-compare_5cm.png',width=20,height=9)
ggsave(p_10cm,filename = 'manuscript-figures/mcmc-compare_10cm.png',width=20,height=9)


# Make a table
