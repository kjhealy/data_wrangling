library(systemfonts)
clear_registry()

register_variant(
  name = "Tenso Slide",
  family = "Tenso Slab",
  weight = c("normal", "medium"),
  width = "normal"
)


ragg_png <- function(..., res = 150) {
  ragg::agg_png(..., res = res, units = "in")
}
knitr::opts_chunk$set(warning = FALSE,
                      message = FALSE,
                      fig.retina = 3,
                      fig.align = "center",
                      dev = "ragg_png")



slide_colors <- c(
  `slate` = "#242E3D",
  `pink` = "#FC2290",
  `red` =  "#FB0307",
  `orange` =  "#EF9B2D",
  `yellow` =  "#FFFF54",
  `lightgrey` = "#F6F6F6",
#  `grey` =  "#CBCBCB",
   `grey` =  "#E8E8E8",
  `darkgrey` =  "#C0C0C0",
  `blue` =  "#212E3E",
  `lblue` =  "#4EAFF0",
  `green` = "#1BB71C"
)

# Reordered Okabe-Ito
slide_colors_opt <- unname(palette.colors()[c(2:4,6:8,5,1,9)])

## NB UK spelling of colour here
options(ggplot2.discrete.colour = slide_colors_opt,
        ggplot2.discrete.fill = slide_colors_opt)


theme_baselayer <- function (base_size = 14, base_family = "")
{
  thm <- ggplot2::theme_grey(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA),
                       legend.background = ggplot2::element_rect(colour = NA),
              line = ggplot2::element_line(colour = "black"),
              rect = ggplot2::element_rect(fill = "white",
                                  colour = "black"),
              text = ggplot2::element_text(colour = "black"))
}



theme_tenso <- function (base_size = 12, base_family = "Tenso Slide") {
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
     ggplot2::theme(line = ggplot2::element_line(colour = slide_colors["slate"]),
                    rect = ggplot2::element_rect(fill = slide_colors["lightgrey"],
                                                 linetype = 0, colour = NA),
                    text = ggplot2::element_text(colour = slide_colors["slate"]),
                    axis.title = ggplot2::element_text(ggplot2::rel(1.15)),
                    axis.text = ggplot2::element_text(size = ggplot2::rel(1.15)),
                    strip.text = ggplot2::element_text(size = ggplot2::rel(1.35),
                                                       face = "bold"),
                    axis.ticks = ggplot2::element_line(),
                    axis.line = ggplot2::element_line(),
                    legend.background = ggplot2::element_rect(),
                    legend.position = "top",
                    legend.direction = "horizontal",
                    legend.box = "vertical",
                    panel.grid = ggplot2::element_line(colour = NULL),
                    panel.grid.major = ggplot2::element_line(colour = slide_colors["grey"]),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(hjust = 0,
                                                       size = ggplot2::rel(1.5),
                                                       face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0,
                                                          size = ggplot2::rel(1.25),
                                                          face = "plain"),
                    plot.caption = ggplot2::element_text(hjust = 0,
                                                         size = ggplot2::rel(0.8),
                                                         face = "plain"),
                    plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                    strip.background = ggplot2::element_rect()
     )
  )
}



#ggplot2::theme_set(theme_solitas())
ggplot2::theme_set(theme_tenso())
