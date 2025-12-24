#| label: "07-iterating-on-data-2"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-3"
# A little trick from the fs package:
fs::dir_tree(here("data", "congress"))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-4"
read_csv(here("data", "congress", "17_95_congress.csv")) |>
  janitor::clean_names() |>
  head()


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-5"
a <- c(1:10)

b <- 1

# You know what R will do here
a + b



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-6"
add_b <- function(x) {
  b <- 1
  x + b # for any x
}


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-6b"
add_b <- function(x) {
  b <- 1
  x + b # for any x
}


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-7"
add_b(x = a)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-8"
add_b(x = 10)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-9"
add_b(x = c(1, 99, 1000))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-10"
library(gapminder)
gapminder |>
  summarize(country_n = n_distinct(country),
            continent_n = n_distinct(continent),
            year_n = n_distinct(year),
            lifeExp_n = n_distinct(lifeExp),
            population_n = n_distinct(population))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-11"
library(gapminder)
gapminder |>
  summarize(n_distinct(country),
            n_distinct(continent),
            n_distinct(year),
            n_distinct(lifeExp),
            n_distinct(population))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-12"
gapminder |>
  summarize(across(everything(), n_distinct))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-13"
  map(gapminder, n_distinct)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-14"
gapminder |>
  map(n_distinct)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-15"
result <- gapminder |>
  map(n_distinct)

class(result)

result$continent

result[[2]]


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-16"
gapminder |>
  map_int(n_distinct)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-17"
filenames <- dir(path = here("data", "congress"),
                 pattern = "*.csv",
                 full.names = TRUE)

filenames[1:15] # Just displaying the first 15, to save slide space



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-18"
df <- filenames |>
  map(read_csv) |> #<<
  list_rbind(names_to = "congress") |>
  janitor::clean_names()

df


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-19"
tmp <- read_csv(filenames, id = "path",
                name_repair = janitor::make_clean_names)

tmp |>
  mutate(congress = str_extract(path, "_\\d{2,3}_congress"),
         congress = str_extract(congress, "\\d{2,3}")) |>
  relocate(congress)



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-20"
## Register for a free Census API key
library(tidycensus)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-21"
#| message: FALSE
#| results: "hide"
out <- get_acs(geography = "county",
                    variables = "B19013_001",
                    state = "NY",
                    county = "New York",
                    survey = "acs1",
                    year = 2005)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-22"
out


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-23"
#| message: FALSE
#| results: "hide"
out <- get_acs(geography = "county",
                    variables = "B19013_001",
                    state = "NY",
                    survey = "acs1",
                    year = 2005)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-24"
out


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-25 07-iterating-on-census-3"
x <- c(1:10)

x

x <- set_names(x, nm = letters[1:10])

x


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-26 07-iterating-on-census-4"
c(1:10) |>
  set_names()



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-27"
#| message: FALSE
#| results: "hide"
df <- 2005:2019 |>
  map(\(x) get_acs(geography = "county",
                   variables = "B19013_001",
                   state = "NY",
                   survey = "acs1",
                   year = x)) |>
  list_rbind(names_to = "year")


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-28"
df


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-29"
#| message: FALSE
#| results: "hide"
df <- 2005:2019 |>
  set_names() |>
  map(\(x) get_acs(geography = "county",
                   variables = "B19013_001",
                   state = "NY",
                   survey = "acs1",
                   year = x)) |>
  list_rbind(names_to = "year") |>
  mutate(year = as.integer(year))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-30"
df


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-31"
#| message: FALSE
#| results: "hide"
p_out <- 2005:2019 |>
  set_names() |>
  map(\(x) get_acs(geography = "county",
                   variables = "B19013_001",
                   state = "NY",
                   survey = "acs1",
                   year = x)) |>
  list_rbind(names_to = "year") |>
  mutate(year = as.integer(year)) |>
  ggplot(mapping = aes(x = year, y = estimate, group = year)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(position = position_jitter(width = 0.1), shape = 1) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(x = "Year", y = "Dollars",
       title = "Median Household Income by County in New York State, 2005-2019",
       subtitle = "ACS 1-year estimates", caption = "Data: U.S. Census Bureau.")



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-32"
#| fig.width: 12
#| fig.height: 5
print(p_out)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-33"
df <- filenames |>
  map(read_csv) |> #<<
  list_rbind(names_to = "congress") |>
  janitor::clean_names()

df |>
  select(born, death, start, end)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-34"
library(lubridate)

date_recodes <- c("born", "death", "start", "end")
df <- df |>
    mutate(across(any_of(date_recodes), mdy),
           congress = as.integer(congress) + 78)

df



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-35"
sessions <- tibble(congress = 79:116,
                   start_year = seq(1945, 2019, by = 2),
                   end_year = seq(1947, 2021, by = 2)) |>
  mutate(start_year = ymd(paste(start_year, "01", "03", sep = "-")),
         end_year = ymd(paste(end_year, "01", "03", sep = "-")))


sessions



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-36"
df |>
  select(congress, last, born)



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-37"
sessions



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-38"
#| message: TRUE

df <- left_join(df, sessions) |>
  relocate(start_year:end_year, .after = congress)

df



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-39"
df <- tribble(
  ~subject, ~age,
  "A", 20,
  "B", 25,
  "C", NA,
  "D", 34
)

df



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-40"
# OK
df |>
  filter(age == 25)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-41"
# Nope
df |>
  filter(age == NA)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-42"
# E.g.
23 == NA


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-43"
# Yes
df |>
  filter(is.na(age))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-44"
library(naniar)
library(visdat)

organdata


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-45"
#| fig.height: 6
#| fig.width: 8
gg_miss_var(organdata)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-46"
#| fig.height: 6
#| fig.width: 8
vis_dat(organdata)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-47"
#| fig.height: 6
#| fig.width: 8
miss_var_summary(organdata)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-48"
miss_case_summary(organdata)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-49"
organdata |>
  select(consent_law, year, pubhealth, roads) |>
  group_by(consent_law) |>
  miss_var_summary()



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-50"
#| fig.height: 6
#| fig.width: 8
vis_miss(organdata)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-51"
#| fig.height: 6
#| fig.width: 8
library(dwcongress)
gg_miss_upset(congress)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-52"
#| fig.height: 6
#| fig.width: 8
vis_miss(organdata, cluster = TRUE)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-53"
#| fig.height: 6
#| fig.width: 8
gg_miss_upset(organdata)


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-54"
symptoms <- c("Anosmia", "Cough", "Fatigue",
              "Diarrhea", "Breath", "Fever")
names(symptoms) <- symptoms
symptoms


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-55"
# An Excel file!
dat <- readxl::read_xlsx(here("data", "symptoms.xlsx"))
dat |> print(n = nrow(dat))



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-56"
subsets <- dat |>
  pull(combination)

## Check if each subset mentions each symptom or not
symptom_mat <- map(subsets, \(x) str_detect(x, symptoms)) |>
  set_names(nm = subsets) |>
  map(\(x) set_names(x, nm = symptoms)) |>
  bind_rows(.id = "subset") |>
  left_join(dat, join_by(subset == combination))



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-57"
symptom_mat |> print(n = nrow(symptom_mat))


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-58"
indvs <- symptom_mat |>
    uncount(count)

indvs



## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-59"
#| fig.width: 16
#| fig.height: 9
#| eval: FALSE
# # devtools::install_github("krassowski/complex-upset")
# 
# library(ComplexUpset)
# 
# upset(data = indvs, intersect = symptoms,
#       name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.",
#       min_size = 0,
#       width_ratio = 0.125) +
#     labs(title = "Co-Occurence of COVID-19 Symptoms",
#          caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")
# 
# 


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-60"
#| fig.width: 12
#| fig.height: 7
#| echo: FALSE
# devtools::install_github("krassowski/complex-upset")

library(ComplexUpset)

upset(data = indvs, intersect = symptoms,
      name="Symptom Groupings by Frequency. Total pool is 1,764 individuals.",
      min_size = 0,
      width_ratio = 0.125) +
    labs(title = "Co-Occurence of COVID-19 Symptoms",
         caption = "Data: covid.joinzoe.com/us | Graph: @kjhealy")




## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-61"
library(broom)
library(gapminder)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-2"
out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,
          data = gapminder)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-3"
summary(out)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-8"
library(broom)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-9"
tidy(out)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-11"
out_conf <- tidy(out, conf.int = TRUE)
out_conf


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-62"
out_conf |>
    filter(term %nin% "(Intercept)") |>
    mutate(nicelabs = prefix_strip(term, "continent")) |>
    select(nicelabs, everything())


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-20"
eu77 <- gapminder |> filter(continent == "Europe", year == 1977)
fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-21"

summary(fit)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-22"

out_le <- gapminder |>
    group_by(continent, year) |>
    nest()

out_le



## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-23"
out_le |> filter(continent == "Europe" & year == 1977) |>
    unnest(cols = c(data))


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-24"
#| echo: FALSE
old_digits <- getOption("digits")
options(digits = 3)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-25"

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

out_le <- gapminder |>
    group_by(continent, year) |>
    nest() |>
    mutate(model = map(data, fit_ols)) #<<


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-63"
out_le


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-26"

fit_ols <- function(df) {
    lm(lifeExp ~ log(gdpPercap), data = df)
}

out_tidy <- gapminder |>
    group_by(continent, year) |>
    nest() |>
    mutate(model = map(data, fit_ols),
           tidied = map(model, tidy)) |>
    unnest(cols = c(tidied)) |>
    filter(term %nin% "(Intercept)" &
           continent %nin% "Oceania")


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-64"
out_tidy


## -----------------------------------------------------------------------------
#| label: "07-iterating-on-data-65"
out_tidy |>
    ungroup() |>
    sample_n(5)


## -----------------------------------------------------------------------------
#| label: "r 07-[Iterating]{.fg-green}-27"
#| echo: FALSE
options(digits = old_digits)

