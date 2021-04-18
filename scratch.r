### --------------------------------------------------
###
###
### --------------------------------------------------


### --------------------------------------------------
### Libraries
### --------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(socviz)
library(ggrepel)

## --------------------------------------------------------------------
## Custom font and theme, omit if you don't have the myriad library
## (https://github.com/kjhealy/myriad) and associated Adobe fonts.
## --------------------------------------------------------------------
library(showtext)
showtext_auto()
library(myriad)
import_myriad_semi()

theme_set(theme_myriad_semi())

### --------------------------------------------------------------------


### --------------------------------------------------
### Local Functions
### --------------------------------------------------



### --------------------------------------------------
### Data
### --------------------------------------------------



### --------------------------------------------------
###
### --------------------------------------------------

as_tibble(mtcars) %>%
  nest_by(cyl) %>%
  mutate(model = list(lm(mpg ~ wt, data = data))) %>%
  summarize(broom::glance(model))



### Floating point numbers



