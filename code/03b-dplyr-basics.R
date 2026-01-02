#| label: "03b-dplyr-basics-2"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-3"
## Data on COVID-19
library(covdata)

covnat_weekly 


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-4"
covnat_weekly |> 
  filter(iso3 == "FRA") |> 
  select(date, cname, iso3, cases) |> 
  mutate(cases = ifelse(is.na(cases), 0, cases), # convert NA vals in `cases` to 0
         cumulative = cumsum(cases)) 



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-5"
covnat_weekly |> 
  select(date, cname, iso3, deaths) |> 
  filter(iso3 == "FRA") |> 
  filter(cume_dist(desc(deaths)) < 0.1) # i.e. Top 10%



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-6"
covus |> 
  filter(measure == "death") |> 
  group_by(state) |> 
  arrange(state, desc(date)) |> 
  filter(state %in% "NY")


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-7"
my_vec <- c(1:20)
my_vec
lag(my_vec) # first element has no lag

my_vec - lag(my_vec)



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-8"
covus |>
  select(-data_quality_grade) |> 
  filter(measure == "death") |>
  group_by(state) |>
  arrange(date) |> 
  mutate(deaths_daily = count - lag(count, order_by = date)) |> 
  arrange(state, desc(date)) |> 
  filter(state %in% "NY")
  


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-9"
my_fun <- function(x) {
  x + 1
}

my_fun # we've created the function; it's just an object

my_fun(x = 1) # But we can supply it with an input!

my_fun(10)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-10"
get_daily_count <- function(count, date){
  count - lag(count, order_by = date)
}


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-11"
covus |>
  filter(measure == "death") |>
  select(-data_quality_grade) |> 
  group_by(state) |>
  arrange(date) |> 
  mutate(deaths_daily = get_daily_count(count, date)) |> 
  arrange(state, desc(date)) |> 
  filter(state %in% "NY")
  


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-12"
# install.packages("slider")
library(slider)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-13"
covus |>
  filter(measure == "death") |>
  select(-data_quality_grade) |> 
  group_by(state) |>
  arrange(date) |> 
  mutate(
    deaths_daily = get_daily_count(count, date), 
    deaths7 = slide_mean(deaths_daily, #<<
                         before = 7, #<<
                         na_rm = TRUE)) |> #<<
  arrange(state, desc(date)) |> 
  filter(state %in% "NY")


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-14"
gss_sm


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-15"
#| include: FALSE
gss_sm |> 
  select(region, bigregion, year, 
         id:region, 
         starts_with("p"), 
         contains("income")) |> 
  rename(children = childs, 
         siblings = sibs) |> 
  relocate(id) |> 
  select(-ballot) |> 
  relocate(where(is.numeric), 
           .before = where(is.factor)) |> 
  relocate(contains("region"), 
           .after = year) 


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-16"
library(ukelection2019)

ukvote2019


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-17"
library(ukelection2019)

ukvote2019 |> 
  sample_n(10)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-18"
ukvote2019 |> 
  distinct(constituency)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-19"
ukvote2019 |> 
  distinct(constituency) |> 
  tally()


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-20"
# Base R / non-pipeline version

length(unique(ukvote2019$constituency))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-21"
ukvote2019 |> 
  count(party_name) |> 
  arrange(desc(n))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-22"
ukvote2019 |> 
  count(party_name) |> 
  slice_max(order_by = n, n = 5)



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-22a"
ukvote2019 |> 
  count(party_name) |> 
  slice_max(order_by = n, n = 5)



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-23"
ukvote2019 |> 
  count(party_name) |> 
  slice_min(order_by = n, n = 5)



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-24"
ukvote2019 |> 
  count(constituency) 


## -----------------------------------------------------------------------------
ukvote2019 |> 
  distinct(constituency) |> 
  count()


## -----------------------------------------------------------------------------
# Base R style ...
length(unique(ukvote2019$constituency))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-25"
ukvote2019 |> 
  count(constituency) |> 
  count(n)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-26"
#| include: FALSE

ukvote2019 |> 
  count(constituency, name = "n_cands") |> 
  count(n_cands, name = "n_const")


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-by-1"
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n())


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-by-2"
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally()


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-by-3"
gss_sm |> 
  count(bigregion, religion) 


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-by-4"
gss_sm |> 
  summarize(total = n(), .by = c(bigregion, religion))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-by-5"
gss_sm |> 
  summarize(total = n(), .by = c(bigregion, religion))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-by-6"
summarize(gss_sm, total = n(), .by = c(bigregion, religion))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-27"
#| echo: FALSE

# Make some sample data with tribb
df <- tribble(~id, ~ prop1, ~prop2,
              "A", 0.1,      0.2,
              "B", 0.1,      0.21, 
              "C", 0.11,     0.2,
              "D", 0.1,      0.1)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-28"
df


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-29"
df |> 
  filter(prop1 + prop2 > 0.3)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-30"
df |> 
  filter(prop1 + prop2 == 0.3)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-31"
df |> 
  mutate(prop3 = prop1 + prop2) |> 
  filter(prop3 == 0.3)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-32"
df |> 
  filter(prop1*100 + prop2*100 == 0.3*100)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-33"
df |> 
  filter(near(prop1 + prop2, 0.3))


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-34"
df <- read_csv(here("data", "first_terms.csv"))

df


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-35"
#| echo: FALSE
## Hex colors for sex
sex_colors <- c("#E69F00", "#993300")

## Group labels
mf_labs <- tibble(M = "Men", F = "Women")



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-36"
df |>
    group_by(start_year, party, sex) |>
    summarize(N = n()) |>
    mutate(freq = N / sum(N))



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-37"
p_col <- df |>
    group_by(start_year, party, sex) |>
    summarize(N = n()) |>
    mutate(freq = N / sum(N)) |>
    ggplot(aes(x = start_year,
               y = freq,
               fill = sex)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = sex_colors, labels = c("Women", "Men")) +
    labs(x = "Year", y = "Percent", fill = "Group") +
    facet_wrap(~ party)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-38"
#| fig.height: 6
#| fig.width: 10
p_col


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-39"
p_line <- df |>
    group_by(start_year, party, sex) |>
    summarize(N = n()) |>
    mutate(freq = N / sum(N)) |>
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-40"
#| fig.height: 6
#| fig.width: 9
p_line


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-41"
df_f <- df |> 
  mutate(party_f = factor(party))

df_f


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-42"
df_f |> 
  group_by(party_f) |> 
  tally()


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-43"
typeof(df_f$party_f)
levels(df_f$party_f)



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-44"
df_f <- df |> 
  mutate(party_f = factor(party, 
                          levels = c("Democratic", 
                                     "Republican", 
                                     "Libertarian")))
df_f |> 
  group_by(party_f) |> 
  tally()

levels(df_f$party_f)



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-45"
df |> 
  mutate(across(where(is.character), as_factor)) |> 
  group_by(start_year, party, sex) |>
  summarize(N = n()) |>
  mutate(freq = N / sum(N))



## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-46"
df |> 
  mutate(across(where(is.character), as_factor)) |> 
  group_by(start_year, party, sex, .drop = FALSE) |> #<<
  summarize(N = n()) |>
  mutate(freq = N / sum(N))
  


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-47"
df_c <- df |>
    group_by(start_year, party, sex) |>
    summarize(N = n()) |>
    mutate(freq = N / sum(N)) |>
    ungroup() |>#<<
    complete(start_year, party, sex,#<<
             fill = list(N = 0, freq = 0))#<<


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-48"
df_c


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-49"
p_out <- df_c |> 
  ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)


## -----------------------------------------------------------------------------
#| label: "03b-dplyr-basics-50"
#| fig.height: 6
#| fig.width: 9
p_out

