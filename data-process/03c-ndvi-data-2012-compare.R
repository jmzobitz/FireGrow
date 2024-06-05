### Author: JMZ
### Last modified: 24/06/01
### Purpose: determine how much choosing a MODIS pixel inside the fire area will affect differences in GPP


nested_data <- canada_modis_data |>
  inner_join(gpp_data,by=c("ID","site","sample","Date")) |>
  inner_join(lst_data,by=c("ID","site","sample","Date")) |>
  mutate(freezing = lst < 273.15) |>
  select(ID,site,sample,Date,ndvi,gpp,lst,freezing) |>
  pivot_longer(cols=c("ndvi","gpp","lst")) |>
  group_by(name) |>
  nest()

# Define a function to compute the percent difference
compute_pct_diff <- function(data,name) {

  data_no_fire <- data |>
    rename(in_data = all_of(name)) |>
    filter(site %in% c("N2012")) |>
    select(Date,site,in_data)


  data_diff <- data |>
    rename(in_data = all_of(name)) |>
    filter(site %in% c("N2012-F")) |>
    select(Date,site,in_data,freezing) |>
    rename(in_data_f=in_data)|>
    inner_join(data_no_fire,by="Date") |>
    mutate(pct_diff = (in_data_f-in_data)/in_data_f) |>
    select(Date,pct_diff,freezing)

  return(data_diff)

}

diff_vector <- nested_data |>
  mutate(pct_diff = map(.x=data,~compute_pct_diff(.x,"value")),
         summary = map(.x=pct_diff,.f=~summary(filter(.x,!is.infinite(pct_diff)))),
         summary_f = map(.x=pct_diff,.f=~summary(filter(.x,!is.infinite(pct_diff),!freezing))))


# Show all of the data
diff_vector |>
  unnest(cols=c(pct_diff)) |>
  ggplot(aes(x=name,y=pct_diff)) + geom_boxplot() + ylim(c(-1,1))

# Show all of the data above freezing
diff_vector |>
  unnest(cols=c(pct_diff)) |>
  filter(!freezing) |>
  ggplot(aes(x=name,y=pct_diff)) + geom_boxplot() + ylim(c(-1,1))



diff_vector$summary[[1]] # ndvi
diff_vector$summary_f[[1]] # ndvi
# 4-5% lower in the fire sites


diff_vector$summary[[2]] # gpp
diff_vector$summary_f[[2]] # gpp
# 20% lower in the fire sites


diff_vector$summary[[3]] # lst
diff_vector$summary_f[[3]] # lst

# .04% higher
