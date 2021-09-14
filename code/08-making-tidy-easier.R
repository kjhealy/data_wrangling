## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, 
                      fig.retina = 3, fig.align = "center")


## ----packages-data, include=FALSE--------------------------------------------------------------------------------------------------
library(flipbookr)
library(cowplot)
ggplot2::theme_set(theme_cowplot())



## ----xaringanExtra, echo=FALSE-----------------------------------------------------------------------------------------------------
xaringanExtra::use_xaringan_extra(c("tile_view"))
xaringanExtra::use_animate_css()
xaringanExtra::use_animate_all("fade")
xaringanExtra::use_clipboard()


## ----06-getting-data-in-1, message = TRUE------------------------------------------------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(haven)     # for Stata, SAS, and SPSS files

library(broom)     # tidy model summaries


## ----08-making-tidy-easier-1-------------------------------------------------------------------------------------------------------
library(reprex)


## ----08-making-tidy-easier-2-------------------------------------------------------------------------------------------------------
library(tidyverse)

starwars %>% 
  count(homeworld, species) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  arrange(desc(pct))


## ----08-making-tidy-easier-3, echo=FALSE-------------------------------------------------------------------------------------------
# Oh no, its the GSS
gss_sm %>% 
  count(bigregion, religion) %>% 
  pivot_wider(names_from =  bigregion, 
              values_from  = n) %>% 
  knitr::kable()


## ----08-making-tidy-easier-4-------------------------------------------------------------------------------------------------------
library(gtsummary)

trial


## ----08-making-tidy-easier-5, eval = FALSE-----------------------------------------------------------------------------------------
## trial %>%
##   tbl_summary(
##     by = trt, # split table by group
##     missing = "no" # don't list missing data separately
##   ) %>%
##   add_n() %>% # add column with total number of non-missing observations
##   add_p() %>% # test for a difference between groups
##   modify_header(label = "**Variable**") %>% # update the column header
##   bold_labels()


## ----08-making-tidy-easier-6, echo = FALSE-----------------------------------------------------------------------------------------
trial %>% 
  tbl_summary(
    by = trt, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()


## ----08-making-tidy-easier-7, eval = FALSE-----------------------------------------------------------------------------------------
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


## ----08-making-tidy-easier-8, echo = FALSE-----------------------------------------------------------------------------------------
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


## ----08-making-tidy-easier-9, eval = FALSE-----------------------------------------------------------------------------------------
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


## ----08-making-tidy-easier-10, echo = FALSE----------------------------------------------------------------------------------------
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


## ----08-making-tidy-easier-11------------------------------------------------------------------------------------------------------
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



## ----08-making-tidy-easier-12------------------------------------------------------------------------------------------------------
out_le


## ----08-making-tidy-easier-13------------------------------------------------------------------------------------------------------
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
  


## ----08-making-tidy-easier-14------------------------------------------------------------------------------------------------------
text_ready


## ----08-making-tidy-easier-15------------------------------------------------------------------------------------------------------

stats <- text_ready %>% 
  mutate(term = janitor::make_clean_names(term)) %>%
  printy::super_split(continent, term) # Thanks again, TJ Mahr



## ----08-making-tidy-easier-16------------------------------------------------------------------------------------------------------
stats

