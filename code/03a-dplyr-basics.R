#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-3"
## library(socviz) # if not loaded
gss_sm


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-4"
gss_sm |> 
  select(id, bigregion, religion)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-5"

gss_sm |> 
  group_by(bigregion)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-6"
#| include: FALSE
gss_sm |> 
  group_by(bigregion) |> 
  summarize(total = n())


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-7"
#| include: FALSE
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n())


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-8"
#| include: FALSE
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-9"
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-10"
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-11"
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(total = n()) |> #<<
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-12"
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(n = n()) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-13"
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-14"
gss_sm |> 
  count(bigregion, religion) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-15"
#| eval: FALSE
## gss_sm |>
##   count(bigregion, religion) |>
##   pivot_wider(names_from = bigregion, values_from = n) |>  #<<
##   knitr::kable()


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-16"
#| echo: FALSE
gss_sm |> 
  count(bigregion, religion) |> 
  pivot_wider(names_from = bigregion, values_from = n) |> 
  knitr::kable()  


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-17"
#| fig.height: 4
#| fig.width: 15
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() |> 
  mutate(pct = round((n/sum(n))*100), 1) |> 
  drop_na() |> 
  ggplot(mapping = aes(x = pct, y = reorder(religion, -pct), fill = religion)) + #<<
  geom_col() + #<<
    labs(x = "Percent", y = NULL) +
    guides(fill = "none") + 
    facet_wrap(~ bigregion, nrow = 1)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-18"
rel_by_region <- gss_sm |> #<<
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-18b"
rel_by_region <- gss_sm |> #<<
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-19"
gss_sm |> 
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) -> #<<
rel_by_region #<<

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-20"
gss_tab <- gss_sm |> 
  count(bigregion, religion) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-21"
gss_sm |> 
  count(bigregion, religion) -> gss_tab  
  


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-22"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-22b"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-23"
## Each region should sum to ~100
rel_by_region |> 
  group_by(bigregion) |> 
  summarize(total = sum(pct)) 



## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-24b"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-25"
rel_by_region |> 
  summarize(total = sum(pct))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-24"
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-25b"
rel_by_region |> 
  summarize(total = sum(pct))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-26"
rel_by_region <- gss_sm |> 
  group_by(bigregion, religion) |> #<<
  tally() |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-27"
# Check
rel_by_region |> 
  group_by(bigregion) |> 
  summarize(total = sum(pct))



## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-28"
#| echo: FALSE
#theme_set(cowplot::theme_minimal_grid())


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-29"
#| fig.height: 4
#| fig.width: 10
gss_sm |> 
  group_by(race, sex, degree) |> 
  summarize(n = n(), 
            mean_age = mean(age, na.rm = TRUE), 
            mean_kids = mean(childs, na.rm = TRUE)) |> 
  mutate(pct = n/sum(n)*100) |> 
  filter(race !="Other") |> 
  drop_na() |> 
  ggplot(mapping = aes(x = mean_kids, y = degree)) + # Some ggplot ...
  geom_col() + facet_grid(sex ~ race) + 
  labs(x = "Average number of Children", y = NULL)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-30"
#| include: FALSE
gss_sm |> 
  group_by(race, sex, degree) |> 
  summarize(n = n(), 
    mean_age = mean(age, na.rm = TRUE), 
    mean_kids = mean(childs, na.rm = TRUE)) |> 
  mutate(pct = n/sum(n)*100) |> 
  filter(race !="Other") |> 
  drop_na() |> 
  summarize(grp_totpct = sum(pct))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-31"
# library(socviz)
organdata


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-32"
organdata |> 
  filter(consent_law == "Informed" & donors > 15) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-33"
organdata |> 
  select(country, year, where(is.integer)) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-34"
organdata |> 
  select(country, year, where(is.character))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-35"
organdata |> 
  select(country, year, starts_with("gdp")) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-36"
organdata |> 
  filter(country == "Australia" | country == "Canada") 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-37"
my_countries <- c("Australia", "Canada", "United States", "Ireland")

organdata |> 
  filter(country %in% my_countries) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-38"
my_countries <- c("Australia", "Canada", "United States", "Ireland")

organdata |> 
  filter(!(country %in% my_countries)) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-39"
`%nin%` <- Negate(`%in%`) # this operator is included in the socviz package


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-40"
organdata |> 
  filter(country %nin% my_countries) #<<


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-41"
gss_sm |> 
  group_by(race, sex, degree) |> 
  summarize(n = n(), 
            mean_age = mean(age, na.rm = TRUE), 
            mean_kids = mean(childs, na.rm = TRUE))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-42"
organdata |>  
  group_by(consent_law, country) |>
  summarize(donors_mean = mean(donors, na.rm = TRUE),
            donors_sd = sd(donors, na.rm = TRUE),
            gdp_mean = mean(gdp, na.rm = TRUE),
            health_mean = mean(health, na.rm = TRUE),
            roads_mean = mean(roads, na.rm = TRUE))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-43"

my_vars <- c("gdp", "donors", "roads")

## nested parens again, but it's worth it
organdata |> 
  group_by(consent_law, country) |>
  summarize(across(all_of(my_vars),           
                   list(avg = mean),  
                   na.rm = TRUE))     


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-44"

my_vars <- c("gdp", "donors", "roads")

organdata |> 
  group_by(consent_law, country) |>
  summarize(across(my_vars,           
                   list(avg = mean, #<<
                        sd = var, #<<
                        md = median),#<<
                   na.rm = TRUE))     


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-45"

my_vars <- c("gdp", "donors", "roads")

organdata |> 
  group_by(consent_law, country) |>
  summarize(across(my_vars,           
                   list(mean = mean, #<<
                        var = var, #<<
                        median = median),#<<
                   na.rm = TRUE))     


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-46"
organdata |> 
  group_by(consent_law, country) |>
  summarize(across(where(is.numeric), #<<
                   list(mean = mean, 
                        var = var, 
                        median = median),
                   na.rm = TRUE)) |> 
    print(n = 3) # just to save slide space


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-47"
organdata |> 
  group_by(consent_law, country) |>
  summarize(across(where(is.numeric),        
                   list(mean = mean, 
                        var = var, 
                        median = median),
                   na.rm = TRUE, 
                   .names = "{fn}_{col}")) |> #<<
  print(n = 3) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-47b"
organdata |> 
  group_by(consent_law, country) |>
  summarize(across(where(is.numeric),        
                   list(mean = mean, 
                        var = var, 
                        median = median),
                   na.rm = TRUE, 
                   .names = "{fn}_{col}")) |> #<<
  print(n = 3) 


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-48"
organdata |> 
  mutate(across(where(is.character), toupper)) |> 
  select(where(is.character))


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-49"
organdata |> 
  group_by(consent_law, country) |>
  summarize(donors = mean(donors, na.rm = TRUE)) |> 
  arrange(donors) |> ##<
  print(n = 5)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-49b"
organdata |> 
  group_by(consent_law, country) |>
  summarize(donors = mean(donors, na.rm = TRUE)) |> 
  arrange(donors) |> ##<
  print(n = 5)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-50"
organdata |> 
  group_by(consent_law, country) |>
  summarize(donors = mean(donors, na.rm = TRUE)) |> 
  arrange(desc(donors)) |>  ##<
  print(n = 5)


## -----------------------------------------------------------------------------
#| label: "03a-dplyr-basics-51"
organdata |> 
  group_by(consent_law, country) |>
  summarize(donors = mean(donors, na.rm = TRUE)) |> 
  slice_max(donors, n = 5) #<<

