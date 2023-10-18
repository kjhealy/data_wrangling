
## ----04-tidy-data-3, message = TRUE-------------------------------------------
library(tidyverse) # your friend and mine
library(gapminder) # gapminder data


## ----04-tidy-data-6-----------------------------------------------------------
gapminder


## ----04-tidy-data-7-----------------------------------------------------------
library(covdata)
covus |> 
  filter(state == "NY") |> 
  select(date:fips, measure:count)


## ----04-tidy-data-8-----------------------------------------------------------
library(palmerpenguins)
penguins |> 
  group_by(species, island, year) |> 
  summarize(bill = round(mean(bill_length_mm, na.rm = TRUE),2)) |> 
  knitr::kable()


## ----04-tidy-data-9-----------------------------------------------------------
penguins |> 
  group_by(species, island, year) |> 
  summarize(bill = round(mean(bill_length_mm, na.rm = TRUE), 2)) |> 
  pivot_wider(names_from = year, values_from = bill) |> 
  knitr::kable()


## ----04-tidy-data-10----------------------------------------------------------
edu


## ----04-tidy-data-11----------------------------------------------------------
edu |> 
  pivot_longer(elem4:coll4, names_to = "education")


## ----04-tidy-data-12----------------------------------------------------------
edu |> 
  pivot_longer(elem4:coll4, names_to = "education", values_to = "n")


## ----04-tidy-data-13----------------------------------------------------------
edu |> 
  pivot_longer(elem4:coll4, names_to = "education", values_to = "n") |> 
  mutate(education = recode(education, 
                            elem4 = "Elementary 4", elem8 = "Elementary 8", 
                            hs3 = "High School 3", hs4 = "High School 4",
                            coll3 = "College 3", coll4 = "College 4"))
  


## ----04-tidy-data-14----------------------------------------------------------
gapminder


## ----04-tidy-data-15----------------------------------------------------------
gapminder |> 
  select(country, continent, year, lifeExp) |> 
  pivot_wider(names_from = year, values_from = lifeExp) #<<


## ----04-tidy-data-16----------------------------------------------------------
gapminder |> 
  pivot_wider(names_from = year, values_from = lifeExp) 


## ----04-tidy-data-17----------------------------------------------------------
gapminder |> 
  select(country, continent, year, lifeExp, gdpPercap) |> 
  pivot_wider(names_from = year, values_from = c(lifeExp, gdpPercap)) #<<


## ----04-tidy-data-18, echo = FALSE--------------------------------------------
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


## ----04-tidy-data-19----------------------------------------------------------
# Some made-up data
dfstrat <- read_csv(here::here("data", "dfstrat.csv"))
dfstrat 


## ----04-tidy-data-20, include = FALSE-----------------------------------------
dfstrat <- read_csv(here::here("data", "dfstrat.csv"))
dfstrat |>
    group_by(sex, race, stratum, educ) |> 
    summarize(mean_inc = mean(income),
              n = n()) |>
    pivot_wider(names_from = (educ),
                values_from = c(mean_inc, n)) |> 
    ungroup()


## ----04-tidy-data-21----------------------------------------------------------
## tribble() lets you make tibbles by hand
df <- tribble(
  ~name, ~occupation,
  "Nero.Wolfe", "Private Detective",
  "Archie.Goodwin", "Personal Assistant",
  "Fritz.Brenner", "Cook and Butler",
  "Theodore.Horstmann", "Orchid Expert"
)

df



## ----04-tidy-data-22, include=FALSE-------------------------------------------
df |> 
  separate(name, into = c("first", "last")) |> 
  unite("full_name", first:last, sep = " ") |> 
  unite("both_together", full_name:occupation, 
        sep = ", ", remove = FALSE)



## ----04-tidy-data-23, include=FALSE-------------------------------------------
df |> 
  separate(name, into = c("first", "last")) |> 
  unite("full_name", first:last) |> 
  separate(full_name, into = c("first", "last"))



## ----04-tidy-data-24, include=FALSE-------------------------------------------
gss_sm |>
    select(race, degree) |> 
    mutate(racedeg = interaction(race, degree)) |>
    group_by(racedeg) |> 
    tally() |> 
    separate(racedeg, sep = "\\.", into = c("race", "degree"))

