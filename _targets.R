library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(tidyverse))

# Deployment variables:
# See deploy_site below for how these are applied
yaml_vars <- yaml::read_yaml(here::here("_variables.yml"))



# Parallelize things --- when we build the PDFs
# it'll take forever otherwise
library(crew)
tar_option_set(
  controller = crew_controller_local(workers = future::availableCores())
)

library(kjhslides)
library(ggthemes)
library(usethis)

# We need to return the path to the rendered HTML file. In this case,
# rmarkdown::render() *does* return a path, but it returns an absolute path,
# which makes the targets pipline less portable. So we return our own path to
# the HTML file instead.
render_quarto <- function(slide_path) {
  quarto::quarto_render(slide_path, quiet = FALSE)

  #return(paste0(tools::file_path_sans_ext(slide_path), ".html"))
}


## Use decktape (via kjhslides) to convert quarto HTML slides to PDF.
## Return a relative path to the PDF to keep targets happy.

html_to_pdf <- function(slide_path) {
  outdir_path <- fs::path_real("pdf_slides")
  kjhslides::kjh_decktape_one_slide(infile = slide_path,
                                    outdir = outdir_path)

  fl <- list.files(here_rel("slides"),
                   pattern = "\\.qmd", full.names = TRUE)
  paste0("_site/", stringr::str_replace(fl, "qmd", "html"))

  return(paste0("pdf_slides/", basename(tools::file_path_sans_ext(slide_path)), ".pdf"))
}

# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Ensure deletion_candidates has at least one dummy dir, to keep target branching happy
if(!fs::dir_exists(here::here("00_dummy_files"))) { fs::dir_create(here::here("00_dummy_files")) }
if(!fs::dir_exists(here::here("00_dummy_files/figure-revealjs"))) { fs::dir_create(here::here("00_dummy_files/figure-revealjs")) }
fs::file_create(here::here("00_dummy_files/figure-revealjs/00_dummy.png"))

# Ensure pdf_slides exists
if(!fs::dir_exists(here::here("pdf_slides"))) { fs::dir_create(here::here("pdf_slides")) }

get_flipbookr_orphans <- function() {
  all_candidates <- fs::dir_ls(glob = "*_files/figure-revealjs/*.png", recurse = TRUE)
  all_candidates <- all_candidates[stringr::str_detect(all_candidates, "_site", negate = TRUE)]
  if(length(all_candidates) == 0) { return(character(0))} else return(all_candidates)
}

relocate_orphans <- function(file) {
  if(length(file) == 0) { return(character(0))}
  if(is.null(file)) {return(character(0))}
  destdir <- paste0("_site/slides/", fs::path_dir(file))
  if(!fs::dir_exists(here::here(destdir))) {fs::dir_create(here::here(destdir))}
  fs::file_move(file, paste0("_site/slides/", file))
}

get_leftover_dirs <- function() {
  # the figure-revealjs subdirs will all have been moved
  deletion_candidates <- fs::dir_ls(glob = "*_files", recurse = TRUE)
  deletion_candidates <- deletion_candidates[stringr::str_detect(deletion_candidates, "_site|_targets", negate = TRUE)]
  if(length(deletion_candidates) == 0) { return(character(0))} else return(deletion_candidates)
}

remove_leftover_dirs <- function (dirs) {
  if(length(dirs) == 0) { return(character(0))}
  if(is.null(dirs)) { return(character(0))} else fs::dir_delete(dirs)
}

## Variables and options
page_suffix <- ".html"

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

tar_option_set(
  packages = c("tibble"),
  format = "rds",
  workspace_on_error = TRUE
)

## SITE PIPELINE ----
list(

  ## Build site ----
  tar_quarto(site, path = ".", quiet = FALSE),

  ## Convert HTML slides to PDF ----
  ### Render the built html slides in _site/slides to PDFs
  ### We wait till quarto has built the site to do this.

  tar_files(rendered_slides, {
    # Force dependencies
    site
    fl <- list.files(here_rel("slides"),
                     pattern = "\\.qmd", full.names = TRUE)
    paste0("_site/", stringr::str_replace(fl, "qmd", "html"))
  }),

  tar_target(quarto_pdfs, {
    html_to_pdf(rendered_slides)
  },
  pattern = map(rendered_slides),
  format = "file"),

  ## Fix any flipbookr leftover files
  tar_files(flipbookr_orphans, {
    # Force dependencies
    rendered_slides
    # Flipbooks created in the top level
    get_flipbookr_orphans()
  }
  ),

  tar_target(move_orphans, {
    relocate_orphans(flipbookr_orphans)
  },
  pattern = map(flipbookr_orphans),
  format = "file"),

  ## Remove any flipbookr leftover dirs
  tar_files(flipbookr_dirs, {
    # Force dependencies
    quarto_pdfs
    # Top-level flipbookr dirs now empty
    get_leftover_dirs()
  }
  ),

  tar_invalidate(empty_dirs),

  tar_target(empty_dirs, {
    remove_leftover_dirs(flipbookr_dirs)
  },
  pattern = map(flipbookr_dirs)),


  ## Upload site ----
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy_site, {
    # Force dependencies
    site
    # Run the deploy script if both conditions are met
    # deploy_username and deploy_site are set in _variables.yml
    if (Sys.info()["user"] != yaml_vars$deploy$user | yaml_vars$deploy$site != TRUE) message("Deployment vars not set. Will not deploy site.")
    if (Sys.info()["user"] == yaml_vars$deploy$user & yaml_vars$deploy$site == TRUE) message("Running deployment script ...")
    if (Sys.info()["user"] == yaml_vars$deploy$user & yaml_vars$deploy$site == TRUE) processx::run(paste0("./", deploy_script), echo = TRUE)
  })
)


