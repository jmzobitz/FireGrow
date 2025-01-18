### Author: JMZ
### Last modified: 24/06/01
### Purpose: Make a plot of annual values of RS versus litterfall to see how they stack on predicted regressions

# Helpful reference
# https://stackoverflow.com/questions/46068074/double-box-plots-in-ggplot2

# pH = -0.138 ln(RS) +1.482
# pA + pH = 1 so pA - 0.138 ln(RS) + 1.482 = 1
# pA = 0.138 ln(RS) - 0.482

## pA = -0.66 + 0.16 ln(RS) BBL 2004
## ln(RH) = 0.22 + 0.87 ln(RS) BBL & Thomson 2010 (USE THIS, reference next one)
## ln(RH) = 1.22 + 0.73 ln(RS) BBL 2004 <--- see above
## RH = 253.49 + 0.61 * Litterfall BBL 2004
## RH/RS = -0.138 ln(RS) + 1.482 Subke 2006
## pH = -0.138 ln(RS) +1.482
## pA + pH = 1 so pA - 0.138 ln(RS) + 1.482 = 1
## pA = 0.138 ln(RS) - 0.482

library(gridExtra)
library(grid)

load('parameter-estimation-outputs/parameter-estimate-summary.Rda')

### Prep the datasets for the three regressions
boxplot_results <- model_sensitivity_data |>
  select(Year,model,depth,boxplot_data) |>
  unnest(cols=c(boxplot_data)) |>
  filter(name %in% c("soil_R","annual_litter")) |>
  group_by(name) |>
  nest() |>
  pivot_wider(values_from = "data") |>
  rename(x = annual_litter, y = soil_R)

colnames(boxplot_results$y[[1]]) <- gsub(pattern = "x.",
                                              replacement = "y.",
                                              x=colnames(boxplot_results$y[[1]]))

litter_rs <- inner_join(boxplot_results$x[[1]],boxplot_results$y[[1]],by=c("Year","model","depth"))



boxplot_results_2 <- model_sensitivity_data |>
  select(Year,model,depth,boxplot_data) |>
  unnest(cols=c(boxplot_data)) |>
  filter(name %in% c("soil_R","microbe_R")) |>
  mutate(across(.cols=c("x.min":"x.max"),.fns=log)) |>
  group_by(name) |>
  nest() |>
  pivot_wider(values_from = "data") |>
  rename(x = soil_R, y = microbe_R)

colnames(boxplot_results_2$y[[1]]) <- gsub(pattern = "x.",
                                                   replacement = "y.",
                                                   x=colnames(boxplot_results_2$y[[1]]))

rs_rh <- inner_join(boxplot_results_2$x[[1]],boxplot_results_2$y[[1]],by=c("Year","model","depth"))

boxplot_results_3 <- model_sensitivity_data |>
  select(Year,model,depth,boxplot_data) |>
  unnest(cols=c(boxplot_data)) |>
  filter(name %in% c("soil_R","RA_gs")) |>
  group_by(name) |>
  nest() |>
  pivot_wider(values_from = "data") |>
  mutate(soil_R = map(.x=soil_R,.f=~(.x |>  mutate(across(.cols=c("x.min":"x.max"),.fns=log))))) |>
  rename(x = soil_R,y=RA_gs)

colnames(boxplot_results_3$y[[1]]) <- gsub(pattern = "x.",
                                            replacement = "y.",
                                            x=colnames(boxplot_results_3$y[[1]]))

rs_ra <- inner_join(boxplot_results_3$x[[1]],boxplot_results_3$y[[1]],by=c("Year","model","depth"))


### Common to all
regression_data <- list(litter_rs,rs_rh,rs_ra)
regression_name <- c("Davidson et. al (2002)","Bond-Lamberty & Thomson (2010)","Subke, Inglima, & Cotrufo (2006)")
regression_color <- c('blue','red','darkgreen')
regression_slope <- c(3.61,0.87,0.138)
regression_intercept <- c(161,0.22,-0.482)
regression_linetype <- c('dashed','dotted','longdash')
x_label <- c(bquote(~Annual~Litterfall~'('~g~C~m^-2~yr^-1*~')'),bquote(~ln(R[S])~'('~'ln('~g~C~m^-2~yr^-1*~')'~')'),bquote(~ln(R[S])~'('~'ln('~g~C~m^-2~yr^-1*~')'~')'))
y_label <- c(bquote(~R[S]~'('~g~C~m^-2~yr^-1*~')'),bquote(~ln(R[H])~'('~'ln('~g~C~m^-2~yr^-1*~')'~')'),bquote(~p[A]~'(no units)'))
subplot_label <- c("a)","b)","c)")

  y_limits_min <- c(0,4.5,0)
  y_limits_max <- c(3000,8.5,1)

  x_limits_min <- c(0,4.5,4.5)
  x_limits_max <- c(600,8.5,8.5)

  plot_info <- tibble(input_data =regression_data,x_label,y_label,x_limits_min,x_limits_max,
                      y_limits_min,y_limits_max,regression_slope,regression_intercept,
                      regression_linetype,regression_name,subplot_label)

  plot_info |>
    mutate(rmse = pmap(.l = list(input_data,regression_slope,regression_intercept),.f=function(x,y,z) {
      x |> group_by(model,depth) |>
        summarize(rmse_out = sqrt(sum( (y.middle - (z+y*x.middle) )^2 ) )) |>
        arrange((rmse_out))

      }) ) |> pull(rmse)

make_regression_plot <- function(input_data,
                                 x_label,
                                 y_label,
                                 x_limits_min,
                                 x_limits_max,
                                 y_limits_min,
                                 y_limits_max,
                                 regression_slope,
                                 regression_intercept,
                                 regression_linetype,
                                 regression_name,
                                 subplot_label,
                                 show_sites = TRUE,
                                 show_regression = TRUE)  {

  my_labeller2 <- as_labeller(c("null"="Null", "microbe"="Microbe","quality"="Quality","N2012"="2012","N1969"="1968","NC"="Control","N1990"="1990","T_soil_5"="5~cm","T_soil_10" = "10~cm"),default = label_parsed)

  # creating a dataframe for annotating text
  ann_dat_text<-tibble(

    # Providing F as an annotation of Plot 1
    # and M as an annotation of Plot 2


    depth=factor("T_soil_5"),
    model = factor("null"),
    label=subplot_label,
    Year = "N2012",
    x_val = max(1.1*x_limits_min,0.1*x_limits_max),
    y_val = 0.9*y_limits_max
  )

  print(ann_dat_text)
  input_data |>
    group_by(depth,model) |>
    nest() |>
    mutate(fit = map(.x=data,.f=~lm(y.middle~x.middle,data=.x)),
           stats = map(.x=fit,.f=~broom::tidy(.x)),
           vals = map(.x=fit,.f=~broom::glance(.x))) |>
    select(-data,-fit,-stats) |>
    unnest(cols=c(vals)) |>
    mutate(p01 = p.value<.01,
           p05 = p.value<.05) |>
    select(model,depth,p01,p05) |>
    print()


  # Custom colors for categories
  custom_colors <- c("N2012" = '#a6cee3', "N1990" = '#1f78b4',  "N1969" = '#b2df8a', "NC" = '#33a02c')

  out_plot <- input_data |>
    mutate(Year = factor(Year,levels=c('N2012','N1990','N1969','NC')),
           depth = factor(depth,levels=c('T_soil_5','T_soil_10')),
           model = factor(model,levels=c('null','microbe','quality'))) |>
    ggplot(aes(color=Year,fill=Year)) +
    scale_color_manual(values = custom_colors) +

    # 2D box defined by the Q1 & Q3 values in each dimension, with outline
    geom_rect(aes(xmin = x.lower, xmax = x.upper, ymin = y.lower, ymax = y.upper), alpha = 0.3,color=NA) +
    geom_rect(aes(xmin = x.lower, xmax = x.upper, ymin = y.lower, ymax = y.upper),
              color = NA, fill = NA) +
    geom_point(aes(x=x.middle,y=y.middle),size=2) +

    # whiskers for x-axis dimension with ends
    geom_segment(aes(x = x.min, y = y.middle, xend = x.max, yend = y.middle)) + #whiskers
    geom_segment(aes(x = x.min, y = y.lower, xend = x.min, yend = y.upper)) + #lower end
    geom_segment(aes(x = x.max, y = y.lower, xend = x.max, yend = y.upper)) + #upper end

    # whiskers for y-axis dimension with ends
    geom_segment(aes(x = x.middle, y = y.min, xend = x.middle, yend = y.max)) + #whiskers
    geom_segment(aes(x = x.lower, y = y.min, xend = x.upper, yend = y.min)) + #lower end
    geom_segment(aes(x = x.lower, y = y.max, xend = x.upper, yend = y.max)) +
    #upper end
    geom_abline(aes(slope=regression_slope,intercept = regression_intercept,linetype=regression_linetype)) +
    facet_grid(depth~model,labeller = my_labeller2)  +
    theme(legend.position="bottom") +
    theme_fulbright() +
    labs(x=x_label,y=y_label,color="Chronosequence Site:",fill="Chronosequence Site:") +
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
    scale_linetype_manual(
      labels = regression_name,
      values = setNames(regression_linetype, regression_linetype)
    ) +
    labs(linetype = "Regression:") +
    coord_cartesian(ylim = c(y_limits_min,y_limits_max),
                    xlim = c(x_limits_min,x_limits_max)) +
    theme(
      axis.text = element_text(size=10),
      axis.title=element_text(size=18),
    ) +
    geom_text(

      # the new dataframe for annotating text
      data = ann_dat_text,
      mapping=aes(x=x_val,y=y_val),
      label=ann_dat_text$label,
      color='black',
      size=6
    )


  if(!show_sites) {
    out_plot <- out_plot + guides(fill = "none", color = "none")
  }

  if(!show_regression) {
    out_plot <- out_plot + guides(linetype = "none")
  }

  return(out_plot)
}



out_plots_regression_legend <- pmap(plot_info,make_regression_plot,show_sites=FALSE)


out_plots_sites <- pmap(plot_info,make_regression_plot,show_regression=FALSE)


g <- ggplotGrob(out_plots_sites[[1]] + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

### Function that plots them all together
grid_arrange_shared_legend <- function(in_plots,in_legend) {
  #g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  #legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  grid.arrange(
    do.call(arrangeGrob, lapply(in_plots, function(x)
      x )),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight,lheight)
  )
}

### Create and save!
out_plot <- grid_arrange_shared_legend(out_plots_regression_legend,legend)

ggsave(filename = 'manuscript-figures/regressions-respiration.png',plot = out_plot,width = 7,height=14)


####
regression_data[[1]] |>
  group_by(model,depth) |>
  nest() |>
  mutate(fit = map(.x=data,.f=~lm(y.middle~x.middle,data=.x)),
         vals = map(fit,broom::glance)) |>
  select(model,depth,vals) |>
  unnest(cols=c(vals)) |>
  select(model,depth,p.value)

regression_data[[2]] |>
  group_by(model,depth) |>
  nest() |>
  mutate(fit = map(.x=data,.f=~lm(y.middle~x.middle,data=.x)),
         vals = map(fit,broom::glance)) |>
  select(model,depth,vals) |>
  unnest(cols=c(vals)) |>
  select(model,depth,p.value)

regression_data[[3]] |>
  group_by(model,depth) |>
  nest() |>
  mutate(fit = map(.x=data,.f=~lm(y.middle~x.middle,data=.x)),
         vals = map(fit,broom::glance)) |>
  select(model,depth,vals) |>
  unnest(cols=c(vals)) |>
  select(model,depth,p.value)



