# Make a table of the MCMC convergence tests for the Supplementary information

estimate_files <- list.files(path = 'parameter-estimation-outputs/estimates-test',full.names = TRUE)


out_vals <- vector(mode='list',length=length(estimate_files))

for (i in seq_along(estimate_files)) {

  curr_file <- estimate_files[[i]]
  load(curr_file)
  # Extract components
  matches <- str_match(curr_file, ".*/N([^-/]+)--([^-/]+)--T_soil_(\\d+)\\.Rda")

  # Assign to named vectors
  curr_year <- matches[, 2]
  model     <- matches[, 3] |> str_to_title()
  depth     <- paste0(matches[, 4], " cm")

  if (curr_year =="1969") {
    curr_year = "1968"
  }

  if (curr_year =="C") {
    curr_year = "Control"
  }

  ll_median <- out_tibble$log_likely |> median(na.rm=TRUE) |> round(2)
  ll_max <-  out_tibble$log_likely |> max(na.rm=TRUE) |> round(2)
  ll_sd <-  out_tibble$log_likely |> sd(na.rm=TRUE) |> round(2)

  out_vals[[i]] <- tibble( year = curr_year,
                           model = model,
                           depth = depth,
                           Median = ll_median,
                           Maximum = ll_max,
                           `Standard Deviation` = ll_max
                           )



}

out_stats <- bind_rows(out_vals) |>
  mutate(model = factor(model, levels = c("Null", "Microbe", "Quality")),
         depth = factor(depth, levels = c("5 cm", "10 cm")),
         year = factor(year, levels = c("2012", "1990","1968","Control"))) |>
  group_by(depth) |>
  arrange(depth,year,model) |>
  nest() |>
  mutate(data = map(.x=data,.f=~pivot_longer(.x,cols=c("Median":"Standard Deviation")) |>
                      pivot_wider(names_from = "year") )) |>
  unnest(cols=c("data")) |>
  rename(`Depth (cm)`=depth,
         Model = model,
         ln_ll_stat=name)

colnames(out_stats)[which(colnames(out_stats) == "ln_ll_stat")] <- "$\\ln(LL)\\text{ statistic}$"


out_text <- knitr::kable(out_stats, format = "latex",
             booktabs = TRUE,
             caption = "Statistics from the posterior log-likelihood function for each MCMC parameter estimation method for the chronosequence sites.",
             label = "ll_stats",
             escape = FALSE)


writeLines(out_text, "manuscript-figures/ll_stats.tex")
