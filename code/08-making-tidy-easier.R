## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, 
                      fig.retina = 3, fig.align = "center")


## ----packages-data, include=FALSE-------------------------------------------------------------------------------------------------
library(flipbookr)
library(cowplot)
ggplot2::theme_set(theme_cowplot())



## ----xaringanExtra, echo=FALSE----------------------------------------------------------------------------------------------------
xaringanExtra::use_xaringan_extra(c("tile_view"))
xaringanExtra::use_animate_css()
xaringanExtra::use_animate_all("fade")
xaringanExtra::use_clipboard()


## ----06-getting-data-in-1, message = TRUE-----------------------------------------------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(haven)     # for Stata, SAS, and SPSS files

library(broom)     # tidy model summaries


## ----08-making-tidy-easier-1------------------------------------------------------------------------------------------------------
library(reprex)


## ----08-making-tidy-easier-2------------------------------------------------------------------------------------------------------
library(tidyverse)

starwars %>% 
  count(homeworld, species) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  arrange(desc(pct))


## ----08-making-tidy-easier-3, echo=FALSE------------------------------------------------------------------------------------------
# Oh no, its the GSS
gss_sm %>% 
  count(bigregion, religion) %>% 
  pivot_wider(names_from =  bigregion, 
              values_from  = n) %>% 
  knitr::kable()


## ----08-making-tidy-easier-4------------------------------------------------------------------------------------------------------
library(gtsummary)

trial


## ----08-making-tidy-easier-5, eval = FALSE----------------------------------------------------------------------------------------
## trial %>%
##   tbl_summary(
##     by = trt, # split table by group
##     missing = "no" # don't list missing data separately
##   ) %>%
##   add_n() %>% # add column with total number of non-missing observations
##   add_p() %>% # test for a difference between groups
##   modify_header(label = "**Variable**") %>% # update the column header
##   bold_labels()


## ----08-making-tidy-easier-6, echo = FALSE----------------------------------------------------------------------------------------
trial %>% 
  tbl_summary(
    by = trt, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()


## ----08-making-tidy-easier-7, eval = FALSE----------------------------------------------------------------------------------------
## gss_sm %>%
##   select(race, degree, marital) %>%
##   drop_na() %>%
##   tbl_summary(
##     by = race, # split table by group
##     missing = "no" # don't list missing data separately
##   ) %>%
##   add_n() %>% # add column with total number of non-missing observations
## #  modify_header(label = "**Variable**") %>% # update the column header
##   bold_labels()


## ----08-making-tidy-easier-8, echo = FALSE----------------------------------------------------------------------------------------
gss_sm %>% 
  select(race, degree, marital) %>% 
  drop_na() %>% 
  tbl_summary(
    by = race, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()


## ----08-making-tidy-easier-9, eval = FALSE----------------------------------------------------------------------------------------
## trial %>%
##   select(trt, age, marker) %>%
##   tbl_summary(
##     by = trt,
##     type = all_continuous() ~ "continuous2",
##     statistic = all_continuous() ~ c("{N_nonmiss}",
##                                      "{mean} ({sd})",
##                                      "{median} ({p25}, {p75})",
##                                      "{min}, {max}"),
##     missing = "no"
##   ) %>%
##   italicize_levels()


## ----08-making-tidy-easier-10, echo = FALSE---------------------------------------------------------------------------------------
trial %>%
  select(trt, age, marker) %>%
  tbl_summary(
    by = trt,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{N_nonmiss}",
                                     "{mean} ({sd})", 
                                     "{median} ({p25}, {p75})", 
                                     "{min}, {max}"),
    missing = "no"
  ) %>%
  italicize_levels()


## ----08-making-tidy-easier-11-----------------------------------------------------------------------------------------------------
library(gapminder)

## Fit as a function, for clarity
fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}


out_le <- gapminder %>%
  filter(continent %nin% "Oceania") %>% 
  group_by(continent) %>%
  nest() %>%
  mutate(model = map(data, fit_ols),
         mod_sum = map(model, glance),
         mod_terms = map(model, tidy, conf.int = TRUE),
         ) %>%
  unnest(cols = c(mod_terms))



## ----08-making-tidy-easier-12-----------------------------------------------------------------------------------------------------
out_le


## ----08-making-tidy-easier-13-----------------------------------------------------------------------------------------------------
## Nice formatting of the numbers
## There are many convenience packages
## like this; it's not too hard to write your own, either
# remotes::install_github("tjmahr/printy")

text_ready <- out_le %>%
  mutate(
    across(c(estimate, conf.low, conf.high),
           printy::fmt_fix_digits, 2),
    se = printy::fmt_fix_digits(std.error, 3),
    ci = glue::glue("[{conf.low}, {conf.high}]")
  ) %>%
  select(continent, term, estimate, se, ci)
  


## ----08-making-tidy-easier-14-----------------------------------------------------------------------------------------------------
text_ready


## ----08-making-tidy-easier-15-----------------------------------------------------------------------------------------------------

stats <- text_ready %>% 
  mutate(term = janitor::make_clean_names(term)) %>%
  printy::super_split(continent, term) # Thanks again, TJ Mahr



## ----08-making-tidy-easier-16-----------------------------------------------------------------------------------------------------
stats


## ----08-making-tidy-easier-test-1-------------------------------------------------------------------------------------------------


countries <- read_csv(here("data", "countries.csv"))

countries



## ----08-making-tidy-easier-test-2-------------------------------------------------------------------------------------------------

get_stmf <- function(url = "https://www.mortality.org/Public/STMF/Outputs",
                     fname = "stmf",
                     date = lubridate::today(),
                     ext = "csv",
                     dest = "data-raw/data",
                     save_file = c("n", "y"),
                     ...) {
  save_file <- match.arg(save_file)
  target <-  fs::path(url, fname, ext = ext)
  message("target: ", target)

  destination <- fs::path(here::here("data-raw/data"),
                          paste0(fname, "_", date), ext = ext)

  tf <- tempfile(fileext = ext)
  curl::curl_download(target, tf)

  switch(save_file,
         y = fs::file_copy(tf, destination),
         n = NULL)

  janitor::clean_names(read_csv(tf, ...))
}




## ----08-making-tidy-easier-test-3-------------------------------------------------------------------------------------------------
stmf_raw <- get_stmf(skip = 2) %>%
  rename(deaths_total = d_total, rate_total = r_total) %>%
  select(country_code:sex, deaths_total, rate_total, split:forecast, everything()) %>%
  pivot_longer(
    cols = d0_14:r85p,
    names_to = c("measure", "age_group"),
    names_pattern = "(r|d)(.*)"
  ) %>%
  pivot_wider(names_from = measure,
              values_from = value) %>%
  mutate(age_group = stringr::str_replace(age_group, "_", "-"),
         age_group = stringr::str_replace(age_group, "p", "+")) %>%
  rename(death_count = d, death_rate = r) %>%
  mutate(approx_date = paste0(year, "-", "W", 
                              stringr::str_pad(week, width = 2, pad = "0"), "-", "7"),
         approx_date = ISOweek::ISOweek2date(approx_date)) %>%
  select(country_code:sex, split:forecast, approx_date, 
         age_group:death_rate, deaths_total, rate_total) %>%
  mutate(country_code = replace(country_code, country_code == "AUS2", "AUS"),
         country_code = replace(country_code, country_code == "NZL_NP", "NZL"))



## ----08-making-tidy-easier-test-4-------------------------------------------------------------------------------------------------
stmf_raw


## ----08-making-tidy-easier-test-5-------------------------------------------------------------------------------------------------
md_ccodes <- tibble(country_code = unique(stmf_raw$country_code)) %>%
  left_join(countries, by = c("country_code" = "iso3")) %>%
  mutate(cname = replace(cname, country_code == "DEUTNP", "Germany"),
         iso2 = replace(iso2, country_code == "DEUTNP", "DE"),
         continent = replace(continent, country_code == "DEU", "Europe"),
         cname = replace(cname, country_code == "FRATNP", "France"),
         iso2 = replace(iso2, country_code == "FRATNP", "FR"),
         continent = replace(continent, country_code == "FRA", "Europe"),
         cname = replace(cname, country_code == "GBRTENW", "England and Wales"),
         cname = replace(cname, country_code == "GBR_SCO", "Scotland"),
         cname = replace(cname, country_code == "GBR_NIR", "Northern Ireland"),
         continent = replace(continent, country_code %in% c("GBRTENW", "GBR_SCO", "GBR_NIR"), "Europe")
         ) %>%
  left_join(countries)


stmf <- left_join(stmf_raw, md_ccodes) %>%
  select(country_code, cname:iso3, everything()) %>%
  mutate(iso3 = replace(iso3, iso2 == "DE", "DEU"),
         iso3 = replace(iso3, iso2 == "FR", "FRA"))



## ----08-making-tidy-easier-test-6-------------------------------------------------------------------------------------------------

stmf



## ---------------------------------------------------------------------------------------------------------------------------------

## stmf
stmf_colnames <- c("country_code", "cname", "iso2", "continent", "iso3", "year",
                     "week", "sex", "split", "split_sex", "forecast", "approx_date",
                     "age_group", "death_count", "death_rate", "deaths_total", "rate_total")

all.equal(colnames(stmf), stmf_colnames)




## ---- eval = FALSE, echo = TRUE---------------------------------------------------------------------------------------------------
## 
## ## countries
## test_that("countries conforms to spec", {
##   countries_colnames <- c("cname", "iso3", "iso2", "continent")
##   expect_equal(colnames(countries), countries_colnames)
## })
## 
## 
## ## stmf
## test_that("stmf conforms to spec", {
##   stmf_colnames <- c("country_code", "cname", "iso2", "continent", "iso3", "year",
##                      "week", "sex", "split", "split_sex", "forecast", "approx_date",
##                      "age_group", "death_count", "death_rate", "deaths_total", "rate_total")
##   expect_equal(colnames(stmf), stmf_colnames)
## })
## 


## ----08-making-tidy-easier-test-8, eval = FALSE, echo = TRUE----------------------------------------------------------------------
## 
## testthat::test_dir(here("tests", "testthat"))
## 
## ## ✓ |  OK F W S | Context
## ##
## ## - |   0       | stmf
## ## - |   0       | Validating package data objects
## ## ✓ |   2       | Validating package data objects
## ##
## ## ══ Results ═════════════════════════════════════════════════════════════════════
## ## [ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]
## 
## 

