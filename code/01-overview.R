## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, 
                      fig.retina = 3, fig.align = "center")


## ----packages-data, include=FALSE--------------------------------------------------------------------------------------------------------------------------
library(flipbookr)
library(cowplot)
ggplot2::theme_set(theme_cowplot())



## ----xaringanExtra, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------
xaringanExtra::use_xaringan_extra(c("tile_view"))
xaringanExtra::use_animate_css()
xaringanExtra::use_animate_all("fade")
xaringanExtra::use_clipboard()

