#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(haven)     # for Stata, SAS, and SPSS files


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-3"
here() # this path will be different for you


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-4"
#| echo: FALSE
fs::dir_tree(here(), recurse = 0)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-5"
## Load the file relative to the path from the top of the project, without separators, etc
organs <- read_csv(file = here("data", "organdonation.csv"))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-6"
organs


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-7"
organs <- read_csv(file = here("data", "organdonation.csv"))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-8"
organ_remote <- read_csv("http://kjhealy.co/organdonation.csv")

organ_remote


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-9"
engmort <- read_table(here("data", "mortality.txt"), 
                      skip = 2, na = ".")

engmort


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-10"
#| message: TRUE
engmort <- read_table(here("data", "mortality.txt"), 
                      skip = 2, na = ".")



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-11"
#| include: FALSE
read_table(here("data", "mortality.txt"), 
           skip = 2, na = ".") |> 
  janitor::clean_names() |> 
  mutate(age = as.integer(recode(age, "110+" = "110")))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-12"
#| warning: TRUE
nchs <- with_edition(1, read_csv(here("data", "SAS_on_2021-04-13.csv")))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-13"
problems(nchs)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-13b"
problems(nchs)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-14"
head(nchs)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-15"
tail(nchs)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-16"
nchs |> 
  slice_sample(n = 10)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-17"
nchs |> 
  slice(2750:2760) 


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-18"
nchs |> 
  slice(2750:2760) |> 
  select(Year, Month, State)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-19"
nchs |> 
  select(Year, Month, State) |> 
  filter(State == "New York")



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-20"
nchs |> 
  select(Year, Month, State) |> 
  filter(!is.na(Year)) #<<


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-21"
nchs |> 
  select(Year) |> 
  distinct(Year)



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-22"
read_lines(here("data", "SAS_on_2021-04-13.csv"), n_max = 10)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-23"
raw_file <- read_lines(here("data", "SAS_on_2021-04-13.csv"))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-24"
# reminder: indexing 1D vectors
letters[5:6]


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-25"
# This is not a tibble; we have to index it the basic way
raw_file[2753:2758] 


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-26"
#| message: TRUE
nchs <- with_edition(1, read_csv(here("data", "SAS_on_2021-04-13.csv"))) 


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-27"
# Date format
us_style <-  "%m/%d/%Y" #<<

nchs <- with_edition(1, read_csv(
  here("data", "SAS_on_2021-04-13.csv"),
  col_types = cols(
    `Data As Of` = col_date(format = us_style),#<<
    `Start Date` = col_date(format = us_style),#<<
    `End Date` = col_date(format = us_style),#<<
    Group = col_character(),
    Year = col_character(),#<<
    Month = col_character(),#<<
    State = col_character(),
    Sex = col_character(),
    `Age Group` = col_character(),
    `COVID-19 Deaths` = col_integer(),
    `Total Deaths` = col_integer(),
    `Pneumonia Deaths` = col_integer(),
    `Pneumonia and COVID-19 Deaths` = col_integer(),
    `Influenza Deaths` = col_integer(),
    `Pneumonia, Influenza, or COVID-19 Deaths` = col_integer(),
    Footnote = col_character()
  )) |> 
  janitor::clean_names() |> 
  select(-footnote) |>
  mutate(age_group = stringr::str_to_sentence(age_group)) |>
  filter(!stringr::str_detect(state, "Total"))
)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-28"
dim(nchs)

nchs |> 
  select(year, month, state) |> 
  filter(!is.na(year)) #<<


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-29"
nchs |> 
  distinct(year) #<<



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-30"
#| echo: FALSE
nchs_fmt <- nchs |> 
  select(-year, -month) |> 
  pivot_longer(covid_19_deaths:pneumonia_influenza_or_covid_19_deaths, 
               names_to = "outcome", 
               values_to = "n") |> 
  mutate(outcome = str_to_sentence(outcome), 
         outcome = str_replace_all(outcome, "_", " "),
         outcome = str_replace(outcome, "(C|c)ovid 19", "COVID-19"))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-31"
#| include: FALSE
library(stringr) # it's back!

nchs |> 
  select(!(c(data_as_of:end_date, year, month))) |> 
  pivot_longer(covid_19_deaths:pneumonia_influenza_or_covid_19_deaths, 
               names_to = "outcome", 
               values_to = "n") |> 
  mutate(outcome = str_to_sentence(outcome), 
         outcome = str_replace_all(outcome, "_", " "),
         outcome = str_replace(outcome, "(C|c)ovid 19", "COVID-19"))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-32"
nchs_fmt |> 
  select(state, age_group, outcome, n)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-33"
nchs_fmt |> 
  distinct(group)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-33b"
nchs_fmt |> 
  distinct(group)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-34"
nchs_fmt |> 
  distinct(age_group)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-35"
p_out <- nchs_fmt |> 
  filter(group %in% "By Total", 
         sex %in% "All Sexes", 
         state %in% "United States", 
         age_group %in% c("0-17 years", 
                          "18-29 years",
                          "30-39 years",
                          "40-49 years",
                          "50-64 years",
                          "65-74 years",
                          "85 years and over"),  
         outcome %in% "COVID-19 deaths") |>
  mutate(age_group = str_replace(age_group, "years", "yrs"),#<<
         age_group = str_replace(age_group, " and over", ""),#<<
         age_group = str_replace(age_group, "85", "85+")) |> #<<
  ggplot(mapping = aes(x = n, y = age_group)) +
  geom_col() + scale_x_continuous(labels = scales::comma) + 
  labs(x = "Deaths", y = NULL, title = "U.S. COVID-19 mortality totals by age group")


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-36"
#| fig.height: 5
#| fig.width: 12
p_out


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-37"
df <- tribble(
  ~a, ~b, ~c,
  1, NA, 2,
  NA, NA, NA, 
  2, 2, 2
)

df



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-38"
# 2 Convenience function
df |> 
  drop_na()



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-39"
# 3
df |> 
  # Anonymous function \(x)
  filter(!if_all(everything(), \(x) is.na(x))) 

# 4 Convenience function from janitor
df |> 
  janitor::remove_empty("rows")



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-40"
#| include: FALSE

read_csv(here("data", "rfm_table.csv")) |> 
  janitor::clean_names() |>
  janitor::remove_empty("rows") |> 
  pivot_longer(cols = r:m) |> 
  separate(col = value, into = c("lo", "hi"), 
           remove = FALSE, convert = TRUE, 
           fill = "left") |>
  select(-value) |>
  pivot_wider(names_from = name, 
              values_from = lo:hi) |>
  mutate(across(where(is.integer), replace_na, 0)) |> 
  select(segment, 
         lo_r, hi_r, 
         lo_f, hi_f, 
         lo_m, hi_m, 
         description)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-41"
#| echo: FALSE
rfm_table <- read_csv(here("data", "rfm_table.csv")) |> 
  janitor::clean_names() |>
  janitor::remove_empty("rows") |> 
  pivot_longer(cols = r:m) |> 
  separate(col = value, into = c("lo", "hi"), 
           remove = FALSE, convert = TRUE, 
           fill = "left") |>
  select(-value) |>
  pivot_wider(names_from = name, 
              values_from = lo:hi) |>
  mutate(across(where(is.integer), replace_na, 0)) |> 
  select(segment, 
         lo_r, hi_r, 
         lo_f, hi_f, 
         lo_m, hi_m, 
         description)



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-42"
rfm_table


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-43"
rfm_table |> 
  mutate(sum_lo = lo_r + lo_f + lo_m,#<<
         sum_hi = hi_r + hi_f + hi_m) |> #<<
  select(segment, sum_lo, sum_hi, everything())


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-44"
rfm_table |> 
  mutate(sum_lo = sum(lo_r, lo_f, lo_m),#<<
         sum_hi = sum(hi_r, hi_f, hi_m)) |>#<<
  select(segment, sum_lo, sum_hi, everything())




## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-45"
rfm_table |> 
  mutate(mean_lo = mean(c(lo_r, lo_f, lo_m)),#<<
         mean_hi = mean(c(hi_r, hi_f, hi_m))) |>#<<
  select(segment, mean_lo, mean_hi, everything())


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-46"
rfm_table |> 
  rowwise() |> #<<
  mutate(mean_lo = mean(c(lo_r, lo_f, lo_m)),#<<
         mean_hi = mean(c(hi_r, hi_f, hi_m))) |>#<<
  select(segment, mean_lo, mean_hi, everything())



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-47"
rfm_table |> 
  group_by(segment) |> 
  mutate(mean_lo = mean(c(lo_r, lo_f, lo_m)),#<<
         mean_hi = mean(c(hi_r, hi_f, hi_m))) |>#<<
  select(segment, mean_lo, mean_hi, everything())



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-48"
rfm_table |> 
  group_by(segment) |> 
  mutate(sum_lo = sum(lo_r, lo_f, lo_m),#<<
         sum_hi = sum(hi_r, hi_f, hi_m)) |>#<<
  select(segment, sum_lo, sum_hi, everything())



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-49"
library(haven)

# This will take a moment
gss_panel <- read_stata(here("data", "gss_panel_long.dta"))



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-50"
gss_panel


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-51"
gss_panel |> 
  select(degree) |> 
  group_by(degree) |> 
  tally()


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-52"
gss_panel |> 
  select(sex, degree) |> 
  group_by(sex, degree) |> 
  tally() |> 
  pivot_wider(names_from = sex, values_from = n)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-53"
gss_panel |> 
  zap_missing() |> 
  zap_labels()


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-54"

## Categorical vars
cat_vars <- c("race", "sex", "degree", "relig", "income", "polviews", "fefam")

## Integer vars
int_vars <- c("year", "id", "ballot", "age", "tvhours")


## Survey design
wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssall",             # weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method


my_gss_vars <- c(int_vars, cat_vars, wt_vars)



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-55"
gss_sub <- gss_panel |> 
  select(all_of(my_gss_vars))

gss_sub


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-56"

gss_sub |> 
  mutate(across(everything(), zap_missing)) |> 
  mutate(across(all_of(wt_vars), as.numeric)) |> 
  mutate(across(all_of(int_vars), as.integer)) |> 
  mutate(across(all_of(cat_vars), as_factor)) |>
  mutate(across(all_of(cat_vars), fct_relabel, tolower)) |> 
  mutate(across(all_of(cat_vars), fct_relabel, tools::toTitleCase)) |> 
  mutate(income = stringr::str_replace(income, " - ", "-")) 



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-57"
gss_sub <- gss_sub |> 
  mutate(across(everything(), zap_missing), 
         across(all_of(wt_vars), as.numeric), 
         across(all_of(int_vars), as.integer), 
         across(all_of(cat_vars), as_factor),
         across(all_of(cat_vars), fct_relabel, tolower), 
         across(all_of(cat_vars), fct_relabel, tools::toTitleCase), 
         income = stringr::str_replace(income, " - ", "-")) 


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-58"
# seq can make all kinds of sequences
seq(from = 0, to = 1, by = 0.2)

age_quintiles <- quantile(as.numeric(gss_panel$age), 
                      probs = seq(0, 1, 0.2), 
                      na.rm = TRUE)

## These are the quintile cutpoints
age_quintiles


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-59"
## Apply the cut
gss_sub |> 
  mutate(agequint = cut(x = age, 
                          breaks = unique(age_quintiles), 
                          include.lowest = TRUE)) |> 
  pull(agequint) |> # grab a column and make it an ordinary vector
  table()



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-60"

convert_agegrp <- function(x){
    x <- stringr::str_remove(x, "\\(")  # Remove open paren
    x <- stringr::str_remove(x, "\\[")  # Remove open bracket
    x <- stringr::str_remove(x, "\\]")  # Remove close bracket
    x <- stringr::str_replace(x, ",", "-") # Replace comma with dash
    x <- stringr::str_replace(x, "-89", "+") # Replace -89 with + 
    regex <- "^(.*$)" # Matches everything in string to end of line
    x <- stringr::str_replace(x, regex, "Age \\1") # Preface string with "Age" 
    x
}



## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-61"
#| include: FALSE
gss_sub |>
    mutate(agequint = cut(x = age, 
                          breaks = unique(age_quintiles), 
                          include.lowest = TRUE)) |> 
    mutate(agequint = fct_relabel(agequint, convert_agegrp)) |> 
    mutate(year_f = droplevels(factor(year))) |> 
    mutate(young = ifelse(age < 26, "Yes", "No")) |> 
    mutate(fefam_d = fct_recode(fefam,
                                Agree = "Strongly Agree",
                                Disagree = "Strongly Disagree")) |> 
    mutate(degree = factor(degree, 
                           levels = levels(gss_sub$degree), #<<
                           ordered = TRUE))#<<


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-62"
gss_sub <- gss_sub |>
      mutate(agequint = cut(x = age, 
                            breaks = unique(age_quintiles), 
                            include.lowest = TRUE), 
              agequint = fct_relabel(agequint, convert_agegrp), 
              year_f = droplevels(factor(year)),#<< 
              young = ifelse(age < 26, "Yes", "No"), 
              fefam_d = fct_recode(fefam,
                                  Agree = "Strongly Agree",
                                  Disagree = "Strongly Disagree"),
              degree = factor(degree, 
                             levels = levels(gss_sub$degree), 
                             ordered = TRUE))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-63"
gss_sub <- gss_sub |>
    mutate(agequint = cut(x = age, 
                          breaks = unique(age_quintiles), 
                          include.lowest = TRUE), 
            agequint = fct_relabel(agequint, convert_agegrp), 
            year_f = factor(year),  
            young = ifelse(age < 26, "Yes", "No"),#<<
            fefam_d = fct_recode(fefam,
                                Agree = "Strongly Agree",
                                Disagree = "Strongly Disagree"),
            degree = factor(degree, 
                           levels = levels(gss_sub$degree), 
                           ordered = TRUE))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-64"
gss_sub <- gss_sub |>
    mutate(agequint = cut(x = age, 
                          breaks = unique(age_quintiles), 
                          include.lowest = TRUE), 
            agequint = fct_relabel(agequint, convert_agegrp), 
            year_f = droplevels(factor(year)),  
            young = ifelse(age < 26, "Yes", "No"), 
            fefam_d = fct_recode(fefam,
                                Agree = "Strongly Agree",
                                Disagree = "Strongly Disagree"),
            degree = factor(degree, 
                           levels = levels(gss_sub$degree), 
                           ordered = TRUE))#<<


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-65"
gss_sub |> 
  select(sex, year, year_f, age, young, fefam, fefam_d) |> 
  sample_n(15)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-66"
gss_sub |> 
  select(sex, degree) |> 
  group_by(sex, degree) |> 
  tally() |> 
  pivot_wider(names_from = sex, values_from = n)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-67"
gss_sub |> 
  count(degree)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-68"
levels(gss_sub$degree)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-69"
gss_sub |> 
  mutate(degree_na = fct_explicit_na(degree)) |> 
  count(degree_na)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-70"
gss_sub |> 
  mutate(degree_freq = fct_infreq(degree)) |> 
  count(degree_freq) 


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-71"
is.ordered(gss_sub$sex)
levels(gss_sub$sex)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-72"
summary(lm(age ~ sex, data = gss_sub))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-73"
gss_sub <- gss_sub |> 
  mutate(sex = fct_relevel(sex, "Female"))

levels(gss_sub$sex)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-74"
summary(lm(age ~ sex, data = gss_sub))


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-75"
gss_sub <- gss_sub |> 
  mutate(degree_by_race = fct_cross(race, degree))

gss_sub |> 
  count(degree_by_race)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-76"
gss_sub |> 
  mutate(degree_n = fct_lump_n(degree, n = 3)) |> 
  count(degree_n)


## -----------------------------------------------------------------------------
#| label: "06-getting-data-in-77"
gss_sub |> 
  mutate(degree_o = fct_other(degree, 
                              keep = c("Lt High School", 
                                       "High School"))) |> 
  count(degree_o)

