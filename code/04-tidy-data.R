#| message: FALSE
library(here)      # manage file paths
library(socviz)    # data and some useful functions


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-3"
#| message: TRUE
library(tidyverse) # your friend and mine
library(gapminder) # gapminder data

## Quieten dplyr summarise chatter (with an 's')!
options(dplyr.summarise.inform = FALSE)



## -----------------------------------------------------------------------------
#| label: "04-tidy-data-6"
gapminder


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-7"
library(covdata)
covus |> 
  filter(state == "NY") |> 
  select(date:fips, measure:count)


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-8"
library(palmerpenguins)
penguins |> 
  group_by(species, island, year) |> 
  summarize(bill = round(mean(bill_length_mm, na.rm = TRUE),2)) |> 
  knitr::kable()


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-9"
penguins |> 
  group_by(species, island, year) |> 
  summarize(bill = round(mean(bill_length_mm, na.rm = TRUE), 2)) |> 
  pivot_wider(names_from = year, values_from = bill) |> 
  knitr::kable()


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-10"
edu


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-11"
edu |> 
  pivot_longer(elem4:coll4, names_to = "education")


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-12"
edu |> 
  pivot_longer(elem4:coll4, names_to = "education", values_to = "n")


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-13"
edu |> 
  pivot_longer(elem4:coll4, names_to = "education", values_to = "n") |> 
  mutate(education = recode(education, 
                            elem4 = "Elementary 4", elem8 = "Elementary 8", 
                            hs3 = "High School 3", hs4 = "High School 4",
                            coll3 = "College 3", coll4 = "College 4"))
  


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-14"
gapminder


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-15"
gapminder |> 
  select(country, continent, year, lifeExp) |> 
  pivot_wider(names_from = year, values_from = lifeExp) #<<


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-16"
gapminder |> 
  pivot_wider(names_from = year, values_from = lifeExp) 


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-17"
gapminder |> 
  select(country, continent, year, lifeExp, gdpPercap) |> 
  pivot_wider(names_from = year, values_from = c(lifeExp, gdpPercap)) #<<


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-18"
#| echo: FALSE
gen_cats <- function(x, N = 1000) {
    sample(x, N, replace = TRUE)
}

set.seed(101)
N <- 1000

income <- rnorm(N, 100, 50)

vars <- list(stratum = c(1:8),
          sex = c("M", "F"),
          race =  c("B", "W"),
          educ = c("HS", "BA"))

dfstrat <- as_tibble(map_dfc(vars, gen_cats))
dfstrat <- add_column(dfstrat, income)
write_csv(dfstrat, here::here("data", "dfstrat.csv"))


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-19"

# Some made-up data
dfstrat <- read_csv(here::here("data", "dfstrat.csv"))
dfstrat 


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-20"
#| include: FALSE
dfstrat <- read_csv(here::here("data", "dfstrat.csv"))
dfstrat |>
    group_by(sex, race, stratum, educ) |> 
    summarize(mean_inc = mean(income),
              n = n()) |>
    pivot_wider(names_from = (educ),
                values_from = c(mean_inc, n)) |> 
    ungroup()


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-21"
## tribble() lets you make tibbles by hand
df <- tribble(
  ~name, ~occupation,
  "Nero.Wolfe", "Private Detective",
  "Archie.Goodwin", "Personal Assistant",
  "Fritz.Brenner", "Cook and Butler",
  "Theodore.Horstmann", "Orchid Expert"
)

df



## -----------------------------------------------------------------------------
#| label: "04-tidy-data-21a"
## tribble() lets you make tibbles by hand
df <- tribble(
  ~name, ~occupation,
  "Nero.Wolfe", "Private Detective",
  "Archie.Goodwin", "Personal Assistant",
  "Fritz.Brenner", "Cook and Butler",
  "Theodore.Horstmann", "Orchid Expert"
)

df



## -----------------------------------------------------------------------------
#| label: "04-tidy-data-22"
#| include: FALSE
df |> 
  separate(name, into = c("first", "last")) |> 
  unite("full_name", first:last, sep = " ") |> 
  unite("both_together", full_name:occupation, 
        sep = ", ", remove = FALSE)



## -----------------------------------------------------------------------------
#| label: "04-tidy-data-23"
#| include: FALSE
df |> 
  separate(name, into = c("first", "last")) |> 
  unite("full_name", first:last) |> 
  separate(full_name, into = c("first", "last"))



## -----------------------------------------------------------------------------
#| label: "04-tidy-data-24"
#| include: FALSE
gss_sm |>
    select(race, degree) |> 
    mutate(racedeg = interaction(race, degree)) |>
    group_by(racedeg) |> 
    tally() |> 
    separate(racedeg, sep = "\\.", into = c("race", "degree"))


## -----------------------------------------------------------------------------
billboard


## -----------------------------------------------------------------------------
billboard |>  
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )


## -----------------------------------------------------------------------------
billboard  |>  
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    names_transform = readr::parse_number,
    values_to = "rank",
    values_drop_na = TRUE,
  )


## -----------------------------------------------------------------------------
who


## -----------------------------------------------------------------------------
who  |>  
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )


## -----------------------------------------------------------------------------
who |>  
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    names_transform = list(
      gender = ~ readr::parse_factor(.x, levels = c("f", "m")),
      age = ~ readr::parse_factor(
        .x,
        levels = c("014", "1524", "2534", "3544", "4554", "5564", "65"), 
        ordered = TRUE
      )
    ),
    values_to = "count"
)


## -----------------------------------------------------------------------------
## Get the data and normalize the column names
df <- read_csv("http://kjhealy.co/MVOtestdata.csv") |>
  janitor::clean_names()

## Starting point
df



## -----------------------------------------------------------------------------
colnames(df)


## -----------------------------------------------------------------------------
df_lon <- df |>
  pivot_longer(right_shoe_color:left_glove_size,
               names_to = c("side", "item", ".value"),
               names_pattern = "(.*)_(.*)_(.*)")

df_lon


## -----------------------------------------------------------------------------
df_superwide <- df_lon |>
  group_by(id, date) |>
  mutate(id = as.character(id),
         date = lubridate::mdy(date),
         seq_id = cur_group_rows()) |>
  relocate(seq_id, .after = id) |>
  ungroup() |>
  pivot_wider(names_from = seq_id, values_from =  date:size)


## -----------------------------------------------------------------------------
df_superwide

# Sheer madness
colnames(df_superwide)


## -----------------------------------------------------------------------------
## Examples of recursive lists and nested/split data frames
# install.packages("repurrsive")
library(repurrrsive)

chars <- tibble(char = got_chars)
chars


## -----------------------------------------------------------------------------
chars2 <- chars |> 
  unnest_wider(char)

chars2


## -----------------------------------------------------------------------------
chars2 |> 
  select(where(is.list))


## -----------------------------------------------------------------------------
chars2 


## -----------------------------------------------------------------------------
#| label: "04-tidy-data-got"
#| include: FALSE

chars2  |>  
  select(name, books, tvSeries) |>
  pivot_longer(c(books, tvSeries), 
               names_to = "media", 
               values_to = "value") |> 
  unnest_longer(value)


## -----------------------------------------------------------------------------
# install.packages("jsonlite")
jsonlite::fromJSON("https://api.github.com/users/kjhealy/repos") |> 
  as_tibble()


## -----------------------------------------------------------------------------
gh_raw <- jsonlite::read_json("https://api.github.com/users/kjhealy/repos") 

gh_tb <- tibble(gh = gh_raw)

gh_tb



## -----------------------------------------------------------------------------
gh_tb |> 
  unnest_wider(gh)


## -----------------------------------------------------------------------------
gh_tb |> 
  unnest_wider(gh) |>
  pull(name)


## -----------------------------------------------------------------------------
gh_tb |> 
  unnest_wider(gh) |>
  select(id, name, ends_with("count")) |>
  arrange(desc(watchers_count))


## -----------------------------------------------------------------------------
bikes <- jsonlite::read_json("https://gbfs.citibikenyc.com/gbfs/2.3/gbfs.json")

bikes


## -----------------------------------------------------------------------------
bikes_tib <- tibble(bikes = bikes$data$en$feeds) |> 
  unnest_wider(bikes)

bikevec <- bikes_tib$url |> 
  set_names(bikes_tib$name)

## Available feeds
bikevec


## -----------------------------------------------------------------------------
## Q: Why do we write it like this?
nyc_stations <- tibble(stations = jsonlite::read_json(bikevec["station_status"])$data) 

nyc_stations


## -----------------------------------------------------------------------------
## Live! (At the time of rendering)
nyc_stations |> 
  unnest_wider(stations, names_sep = "_") 


## -----------------------------------------------------------------------------

nyc_stations |> 
  unnest_wider(stations, names_sep = "_") |> 
  pivot_longer(starts_with("stations")) 


## -----------------------------------------------------------------------------

nyc_stations |> 
  unnest_wider(stations, names_sep = "_") |> 
  pivot_longer(starts_with("stations")) |>   
  unnest_wider(value)


## -----------------------------------------------------------------------------
## Q: Why do we write it like this?
nyc_stations_info <- tibble(stations = jsonlite::read_json(bikevec["station_information"])$data[[1]])

nyc_stations_info |>
  unnest_wider(stations)

