## Create code files

library(tidyverse)

fnames <- tibble(
  inpath =
  fs::dir_ls(
  path = here::here("slides/"),
  recurse = 1,
  glob = "*.Rmd"
)) %>%
  mutate(outname = paste0(tools::file_path_sans_ext(basename(inpath)), ".R"),
         outpath = here::here("code", outname)) %>%
  filter(outname != "00-template.R")

walk2(fnames$inpath, fnames$outpath, purl)
