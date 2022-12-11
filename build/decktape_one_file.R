## Build a single PDF file from rendered html file

## This script runs from the corresponding rule in the ../Makefile
## e.g. make slides/01-overview/01-overview.html will make pdf_slides/01-overview.pdf
fname <- commandArgs(TRUE)
suppressMessages(library(knitr))
suppressMessages(library(tidyverse))
kjhslides::kjh_decktape_one_slide(fname[1])
