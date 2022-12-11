## Build a single html file from an Rmd file

## This script runs from the corresponding rule in the ../Makefile
## e.g. make slides/01-overview/01-overview.Rmd will make 01-overview.html
## in the same directory
fname <- commandArgs(TRUE)
suppressMessages(library(knitr))
suppressMessages(library(tidyverse))
kjhslides::kjh_render_one_slide(fname[1])
