## ----07-iterating-on-data-1, include=FALSE------------------------------------------------------------------------------------------------
library(flipbookr)
library(here)
library(tidyverse)
library(kjhslides)


## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------

kjh_register_tenso()

kjh_set_knitr_opts()

kjh_set_slide_theme()

kjh_set_xaringnan_opts()



## ----07-iterating-on-data-2, message = TRUE-----------------------------------------------------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## ----07-iterating-on-data-3---------------------------------------------------------------------------------------------------------------
# A little trick from the fs package: 
fs::dir_tree(here("data", "congress"))


## ----07-iterating-on-data-4---------------------------------------------------------------------------------------------------------------
read_csv(here("data", "congress", "17_95_congress.csv")) %>% 
  janitor::clean_names() %>% 
  head()


## ----07-iterating-on-data-5---------------------------------------------------------------------------------------------------------------
a <- c(1:10)

b <- 1

# You know what R will do here
a + b



## ----07-iterating-on-data-6---------------------------------------------------------------------------------------------------------------
add_b <- function(x) {
  b <- 1
  x + b # for any x
}


## ----07-iterating-on-data-7---------------------------------------------------------------------------------------------------------------
add_b(x = a)


## ----07-iterating-on-data-8---------------------------------------------------------------------------------------------------------------
add_b(x = 10)


## ----07-iterating-on-data-9---------------------------------------------------------------------------------------------------------------
add_b(x = c(1, 99, 1000))


## ----07-iterating-on-data-10--------------------------------------------------------------------------------------------------------------
library(gapminder)
gapminder %>% 
  summarize(country_n = n_distinct(country), 
            continent_n = n_distinct(continent), 
            year_n = n_distinct(year), 
            lifeExp_n = n_distinct(lifeExp), 
            population_n = n_distinct(population))


## ----07-iterating-on-data-11--------------------------------------------------------------------------------------------------------------
library(gapminder)
gapminder %>% 
  summarize(n_distinct(country), 
            n_distinct(continent), 
            n_distinct(year), 
            n_distinct(lifeExp), 
            n_distinct(population))


## ----07-iterating-on-data-12--------------------------------------------------------------------------------------------------------------
gapminder %>% 
  summarize(across(everything(), n_distinct))


## ----07-iterating-on-data-13--------------------------------------------------------------------------------------------------------------
  map(gapminder, n_distinct)


## ----07-iterating-on-data-14--------------------------------------------------------------------------------------------------------------
gapminder %>% 
  map(n_distinct)


## ----07-iterating-on-data-15--------------------------------------------------------------------------------------------------------------
result <- gapminder %>% 
  map(n_distinct)

class(result)

result$continent

result[[2]]


## ----07-iterating-on-data-16--------------------------------------------------------------------------------------------------------------
gapminder %>% 
  map_int(n_distinct)


## ----07-iterating-on-data-17--------------------------------------------------------------------------------------------------------------
filenames <- dir(path = here("data", "congress"),
                 pattern = "*.csv",
                 full.names = TRUE)

filenames[1:15] # Just displaying the first 15, to save slide space



## ----07-iterating-on-data-18--------------------------------------------------------------------------------------------------------------
df <- filenames %>% 
  map_dfr(read_csv, .id = "congress") %>% #<<
  janitor::clean_names()

df


## ----07-iterating-on-data-19--------------------------------------------------------------------------------------------------------------
tmp <- read_csv(filenames, id = "path",
                name_repair = janitor::make_clean_names)

tmp %>% 
  mutate(congress = stringr::str_extract(path, "_\\d{2,3}_congress"), 
         congress = stringr::str_extract(congress, "\\d{2,3}")) %>% 
  relocate(congress)



## ----07-iterating-on-data-20--------------------------------------------------------------------------------------------------------------
## Register for a free Census API key
library(tidycensus)


## ----07-iterating-on-data-21, message=FALSE, results='hide'-------------------------------------------------------------------------------
out <- get_acs(geography = "county", 
                    variables = "B19013_001",
                    state = "NY", 
                    county = "New York", 
                    survey = "acs1",
                    year = 2005)


## ----07-iterating-on-data-22--------------------------------------------------------------------------------------------------------------
out


## ----07-iterating-on-data-23, message=FALSE, results='hide'-------------------------------------------------------------------------------
out <- get_acs(geography = "county", 
                    variables = "B19013_001",
                    state = "NY", 
                    survey = "acs1",
                    year = 2005)


## ----07-iterating-on-data-24--------------------------------------------------------------------------------------------------------------
out


## ----07-iterating-on-data-25, 07-iterating-on-census-3------------------------------------------------------------------------------------
x <- c(1:10)

x

x <- set_names(x, nm = letters[1:10])

x


## ----07-iterating-on-data-26, 07-iterating-on-census-4------------------------------------------------------------------------------------
c(1:10) %>% 
  set_names()



## ----07-iterating-on-data-27, message=FALSE, results='hide'-------------------------------------------------------------------------------
df <- 2005:2019 %>% 
  map_dfr(~ get_acs(geography = "county", 
                    variables = "B19013_001",
                    state = "NY", 
                    county = "New York", 
                    survey = "acs1",
                    year = .x), 
        .id = "id")


## ----07-iterating-on-data-28--------------------------------------------------------------------------------------------------------------
df


## ----07-iterating-on-data-29, message=FALSE, results='hide'-------------------------------------------------------------------------------
df <- 2005:2019 %>% 
  set_names() %>% 
  map_dfr(~ get_acs(geography = "county", 
                    variables = "B19013_001",
                    state = "NY", 
                    county = "New York", 
                    survey = "acs1",
                    year = .x), 
        .id = "year") %>% 
  mutate(year = as.integer(year))


## ----07-iterating-on-data-30--------------------------------------------------------------------------------------------------------------
df


## ----07-iterating-on-data-31, message=FALSE, results='hide'-------------------------------------------------------------------------------
p_out <- 2005:2019 %>% 
  set_names() %>% 
  map_dfr(~ get_acs(geography = "county", variables = "B19013_001",
                    state = "NY", survey = "acs1", year = .x), .id = "year") %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(mapping = aes(x = year, y = estimate, group = year)) + 
  geom_boxplot(fill = "lightblue", alpha = 0.5, outlier.alpha = 0) + 
  geom_jitter(position = position_jitter(width = 0.1), shape = 1) +
  scale_y_continuous(labels = scales::label_dollar()) + labs(x = "Year", y = "Dollars", 
  title = "Median Household Income by County in New York State, 2005-2019", 
  subtitle = "ACS 1-year estimates", caption = "Data: U.S. Census Bureau.") 



## ----07-iterating-on-data-32, fig.width=12, fig.height=5----------------------------------------------------------------------------------
print(p_out)


## ----07-iterating-on-data-33--------------------------------------------------------------------------------------------------------------
df <- filenames %>% 
  map_dfr(read_csv, .id = "congress") %>% #<<
  janitor::clean_names()

df %>% 
  select(born, death, start, end)


## ----07-iterating-on-data-34--------------------------------------------------------------------------------------------------------------
library(lubridate)

date_recodes <- c("born", "death", "start", "end")
df <- df %>% 
    mutate(across(any_of(date_recodes), mdy), 
           congress = as.double(congress) + 78)

df 



## ----07-iterating-on-data-35--------------------------------------------------------------------------------------------------------------
sessions <- tibble(congress = 79:116,
                   start_year = seq(1945, 2019, by = 2),
                   end_year = seq(1947, 2021, by = 2)) %>% 
  mutate(start_year = ymd(paste(start_year, "01", "03", sep = "-")), 
         end_year = ymd(paste(end_year, "01", "03", sep = "-")))


sessions



## ----07-iterating-on-data-36--------------------------------------------------------------------------------------------------------------
df %>% 
  select(congress, last, born)



## ----07-iterating-on-data-37--------------------------------------------------------------------------------------------------------------
sessions



## ----07-iterating-on-data-38, message = TRUE----------------------------------------------------------------------------------------------

df <- left_join(df, sessions) %>% 
  relocate(start_year:end_year, .after = congress)  

df 



## ----07-iterating-on-data-39--------------------------------------------------------------------------------------------------------------
df <- tribble(
  ~subject, ~age,
  "A", 20,
  "B", 25,
  "C", NA,
  "D", 34
)

df



## ----07-iterating-on-data-40--------------------------------------------------------------------------------------------------------------
# OK
df %>% 
  filter(age == 25)


## ----07-iterating-on-data-41--------------------------------------------------------------------------------------------------------------
# Nope
df %>% 
  filter(age == NA)


## ----07-iterating-on-data-42--------------------------------------------------------------------------------------------------------------
# E.g.
23 == NA


## ----07-iterating-on-data-43--------------------------------------------------------------------------------------------------------------
# Yes
df %>% 
  filter(is.na(age))


## ----07-iterating-on-data-44--------------------------------------------------------------------------------------------------------------
library(naniar)
library(visdat)

organdata


## ----07-iterating-on-data-45, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
gg_miss_var(organdata)


## ----07-iterating-on-data-46, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
vis_dat(organdata)


## ----07-iterating-on-data-47, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
miss_var_summary(organdata)


## ----07-iterating-on-data-48--------------------------------------------------------------------------------------------------------------
miss_case_summary(organdata)


## ----07-iterating-on-data-49--------------------------------------------------------------------------------------------------------------
organdata %>%
  select(consent_law, year, pubhealth, roads) %>%
  group_by(consent_law) %>%
  miss_var_summary()



## ----07-iterating-on-data-50, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
vis_miss(organdata)


## ----07-iterating-on-data-51, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
library(congress)
gg_miss_upset(congress)


## ----07-iterating-on-data-52, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
vis_miss(organdata, cluster = TRUE)


## ----07-iterating-on-data-53, fig.height=6, fig.width=8-----------------------------------------------------------------------------------
gg_miss_upset(organdata)


## ----07-iterating-on-data-54--------------------------------------------------------------------------------------------------------------
symptoms <- c("Anosmia", "Cough", "Fatigue", 
              "Diarrhea", "Breath", "Fever")
names(symptoms) <- symptoms
symptoms


## ----07-iterating-on-data-55--------------------------------------------------------------------------------------------------------------
# An Excel file!
dat <- readxl::read_xlsx(here("data", "symptoms.xlsx")) 
dat %>% print(n = nrow(dat))



## ----07-iterating-on-data-56--------------------------------------------------------------------------------------------------------------
subsets <- dat %>% 
  pull(combination)

## Check if each subset mentions each symptom or not
symptom_mat <- map_dfc(subsets, str_detect, symptoms) %>%
    data.frame() %>%
    t() %>% # transpose the result, this is a little gross, sorry
    as_tibble(.name_repair = "unique")

colnames(symptom_mat) <- symptoms
symptom_mat$count <- dat$count


## ----07-iterating-on-data-57--------------------------------------------------------------------------------------------------------------
symptom_mat %>% print(n = nrow(symptom_mat))


## ----07-iterating-on-data-58--------------------------------------------------------------------------------------------------------------
indvs <- symptom_mat %>%
    uncount(count) 

indvs



## ----07-iterating-on-data-59, fig.width=16, fig.height=9, eval = FALSE--------------------------------------------------------------------
## # devtools::install_github("krassowski/complex-upset")
## 
## library(ComplexUpset)
## 
## upset(data = indvs, intersect = symptoms,
##       name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.",
##       min_size = 0,
##       width_ratio = 0.125) +
##     labs(title = "Co-Occurence of COVID-19 Symptoms",
##          caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")
## 
## 


## ----07-iterating-on-data-60, fig.width=12, fig.height=7, echo = FALSE--------------------------------------------------------------------
# devtools::install_github("krassowski/complex-upset")

library(ComplexUpset)

upset(data = indvs, intersect = symptoms, 
      name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.", 
      min_size = 0,
      width_ratio = 0.125) +
    labs(title = "Co-Occurence of COVID-19 Symptoms",
         caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")




## ----07-iterating-on-data-61--------------------------------------------------------------------------------------------------------------
library(broom)
library(gapminder)


## ----r 07-.kjh-green[Iterating]-2---------------------------------------------------------------------------------------------------------
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,
          data = gapminder)


## ----r 07-.kjh-green[Iterating]-3---------------------------------------------------------------------------------------------------------
summary(out)


## ----r 07-.kjh-green[Iterating]-8---------------------------------------------------------------------------------------------------------
library(broom)


## ----r 07-.kjh-green[Iterating]-9---------------------------------------------------------------------------------------------------------
tidy(out)


## ----r 07-.kjh-green[Iterating]-11--------------------------------------------------------------------------------------------------------
out_conf <- tidy(out, conf.int = TRUE)
out_conf 


## ----07-iterating-on-data-62--------------------------------------------------------------------------------------------------------------
out_conf %>%
    filter(term %nin% "(Intercept)") %>%
    mutate(nicelabs = prefix_strip(term, "continent")) %>%
    select(nicelabs, everything())


## ----r 07-.kjh-green[Iterating]-20--------------------------------------------------------------------------------------------------------
eu77 <- gapminder %>% filter(continent == "Europe", year == 1977)
fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)


## ----r 07-.kjh-green[Iterating]-21--------------------------------------------------------------------------------------------------------

summary(fit)


## ----r 07-.kjh-green[Iterating]-22--------------------------------------------------------------------------------------------------------

out_le <- gapminder %>%
    group_by(continent, year) %>%
    nest()

out_le



## ----r 07-.kjh-green[Iterating]-23--------------------------------------------------------------------------------------------------------
out_le %>% filter(continent == "Europe" & year == 1977) %>% 
    unnest(cols = c(data))


## ----r 07-.kjh-green[Iterating]-24, echo = FALSE------------------------------------------------------------------------------------------
old_digits <- getOption("digits")
options(digits = 3)


## ----r 07-.kjh-green[Iterating]-25--------------------------------------------------------------------------------------------------------

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

out_le <- gapminder %>%
    group_by(continent, year) %>%
    nest() %>% 
    mutate(model = map(data, fit_ols)) #<<


## ----07-iterating-on-data-63--------------------------------------------------------------------------------------------------------------
out_le


## ----r 07-.kjh-green[Iterating]-26--------------------------------------------------------------------------------------------------------

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

out_tidy <- gapminder %>%
    group_by(continent, year) %>%
    nest() %>% 
    mutate(model = map(data, fit_ols),
           tidied = map(model, tidy)) %>%
    unnest(cols = c(tidied)) %>%
    filter(term %nin% "(Intercept)" &
           continent %nin% "Oceania")


## ----07-iterating-on-data-64--------------------------------------------------------------------------------------------------------------
out_tidy


## ----07-iterating-on-data-65--------------------------------------------------------------------------------------------------------------
out_tidy %>% 
    ungroup() %>%
    sample_n(5)


## ----r 07-.kjh-green[Iterating]-27, echo = FALSE------------------------------------------------------------------------------------------
options(digits = old_digits)

