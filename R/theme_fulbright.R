#' Define a custom theme for making ggplots
#'
#' \code{theme_fulbright} Sets up defaults for ggplots so I don't need to keep doing these over and over.
#'

theme_fulbright <- function() {
  theme_bw() +
    theme(legend.position = "bottom",
          axis.text = element_text(size=14),
          axis.title=element_text(size=28),
          title=element_text(size=26),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          strip.text.x = element_text(size=12),
          strip.text.y = element_text(size=12),
          strip.background = element_rect(colour="white", fill="white")) +
    theme( panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

}
