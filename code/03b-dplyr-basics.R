## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, 
                      fig.retina = 3, fig.align = "center")


## ----packages-data, include=FALSE--------------------------------------------------------------------------------
library(flipbookr)
library(cowplot)
ggplot2::theme_set(theme_cowplot())



## ----xaringanExtra, echo=FALSE-----------------------------------------------------------------------------------
xaringanExtra::use_xaringan_extra(c("tile_view"))
xaringanExtra::use_animate_css()
xaringanExtra::use_animate_all("fade")
xaringanExtra::use_clipboard()


## ----03-dplyr-basics-1, message = TRUE---------------------------------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## ----03-dplyr-basics-48------------------------------------------------------------------------------------------
## Data on COVID-19
library(covdata)

covnat_weekly 


## ----03-dplyr-basics-49------------------------------------------------------------------------------------------
covnat_weekly %>% 
  filter(cname == "United States") %>% 
  select(date, cname, iso3, cases) %>% 
  mutate(cumulative = cumsum(cases)) 



## ----03-dplyr-basics-50------------------------------------------------------------------------------------------
covnat_weekly %>% 
  select(date, cname, iso3, deaths) %>% 
  filter(cname == "United States") %>% 
  filter(cume_dist(desc(deaths)) < 0.1) # i.e. Top 10%



## ----03-dplyr-basics-51------------------------------------------------------------------------------------------
covus %>% 
  filter(measure == "death") %>% 
  group_by(state) %>% 
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")


## ----03-dplyr-basics-52------------------------------------------------------------------------------------------
my_vec <- c(1:20)
my_vec
lag(my_vec) # first element has no lag

my_vec - lag(my_vec)



## ----03-dplyr-basics-53------------------------------------------------------------------------------------------
covus %>%
  select(-data_quality_grade) %>% 
  filter(measure == "death") %>%
  group_by(state) %>%
  arrange(date) %>% 
  mutate(deaths_daily = count - lag(count, order_by = date)) %>% 
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")
  


## ----03-dplyr-basics-54------------------------------------------------------------------------------------------
my_fun <- function(x) {
  x + 1
}

my_fun # we've created the function; it's just an object

my_fun(x = 1) # But we can supply it with an input!

my_fun(10)


## ----03-dplyr-basics-55------------------------------------------------------------------------------------------
get_daily_count <- function(count, date){
  count - lag(count, order_by = date)
}


## ----03-dplyr-basics-56------------------------------------------------------------------------------------------
covus %>%
  filter(measure == "death") %>%
  select(-data_quality_grade) %>% 
  group_by(state) %>%
  arrange(date) %>% 
  mutate(deaths_daily = get_daily_count(count, date)) %>% 
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")
  


## ----03-dplyr-basics-57------------------------------------------------------------------------------------------
# install.packages("slider")
library(slider)


## ----03-dplyr-basics-58------------------------------------------------------------------------------------------
covus %>%
  filter(measure == "death") %>%
  select(-data_quality_grade) %>% 
  group_by(state) %>%
  arrange(date) %>% 
  mutate(
    deaths_daily = get_daily_count(count, date), 
    deaths7 = slide_mean(deaths_daily, #<<
                         before = 7, #<<
                         na_rm = TRUE)) %>% #<<
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")


## ----03-dplyr-basics-59------------------------------------------------------------------------------------------
gss_sm


## ----03-relocate-pipeline, include = FALSE-----------------------------------------------------------------------
gss_sm %>% 
  select(region, bigregion, year, 
         id:region, 
         starts_with("p"), 
         contains("income")) %>% 
  rename(children = childs, 
         siblings = sibs) %>% 
  relocate(id) %>% 
  select(-ballot) %>% 
  relocate(where(is.numeric), 
           .before = where(is.factor)) %>% 
  relocate(contains("region"), 
           .after = year) 


## ---- echo = FALSE-----------------------------------------------------------------------------------------------

# Make some sample data with tribb
df <- tribble(~id, ~ prop1, ~prop2,
              "A", 0.1,      0.2,
              "B", 0.1,      0.21, 
              "C", 0.11,     0.2,
              "D", 0.1,      0.1)


## ----------------------------------------------------------------------------------------------------------------
df


## ----------------------------------------------------------------------------------------------------------------
df %>% 
  filter(prop1 + prop2 > 0.3)


## ----------------------------------------------------------------------------------------------------------------
df %>% 
  filter(prop1 + prop2 == 0.3)


## ----------------------------------------------------------------------------------------------------------------
df %>% 
  mutate(prop3 = prop1 + prop2) %>% 
  filter(prop3 == 0.3)


## ----------------------------------------------------------------------------------------------------------------
df %>% 
  filter(prop1*100 + prop2*100 == 0.3*100)


## ----------------------------------------------------------------------------------------------------------------
df %>% 
  filter(near(prop1 + prop2, 0.3))


## ----------------------------------------------------------------------------------------------------------------
df <- read_csv(here("data", "first_terms.csv"))

df


## ---- echo = FALSE-----------------------------------------------------------------------------------------------
## Hex colors for sex
sex_colors <- c("#E69F00", "#993300")

## Group labels
mf_labs <- tibble(M = "Men", F = "Women")

theme_set(cowplot::theme_cowplot())



## ----------------------------------------------------------------------------------------------------------------
df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N))



## ----------------------------------------------------------------------------------------------------------------
p_col <- df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               fill = sex)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = sex_colors, labels = c("Women", "Men")) +
    labs(x = "Year", y = "Percent", fill = "Group") +
    facet_wrap(~ party)


## ---- fig.height = 6, fig.width=10-------------------------------------------------------------------------------
p_col


## ----------------------------------------------------------------------------------------------------------------
p_line <- df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)


## ---- fig.height = 6, fig.width=9--------------------------------------------------------------------------------
p_line


## ----------------------------------------------------------------------------------------------------------------
df_f <- df %>% 
  mutate(party_f = factor(party))

df_f


## ----------------------------------------------------------------------------------------------------------------
df_f %>% 
  group_by(party_f) %>% 
  tally()


## ----------------------------------------------------------------------------------------------------------------
typeof(df_f$party_f)
levels(df_f$party_f)



## ----------------------------------------------------------------------------------------------------------------
df_f <- df %>% 
  mutate(party_f = factor(party, 
                          levels = c("Democrat", 
                                     "Republican", 
                                     "Libertarian")))
df_f %>% 
  group_by(party_f) %>% 
  tally()

levels(df_f$party_f)



## ----------------------------------------------------------------------------------------------------------------
df %>% 
  mutate(across(where(is.character), as_factor)) %>% 
  group_by(start_year, party, sex) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))



## ----------------------------------------------------------------------------------------------------------------
df %>% 
  mutate(across(where(is.character), as_factor)) %>% 
  group_by(start_year, party, sex, .drop = FALSE) %>% #<<
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))
  


## ----------------------------------------------------------------------------------------------------------------
df_c <- df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ungroup() %>%#<<
    complete(start_year, party, sex,#<<
             fill = list(N = 0, freq = 0))#<<


## ----------------------------------------------------------------------------------------------------------------
df_c


## ----------------------------------------------------------------------------------------------------------------
p_out <- df_c %>% 
  ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)


## ---- fig.height = 6, fig.width=9--------------------------------------------------------------------------------
p_out
