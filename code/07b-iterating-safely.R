## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-2
#| message: true
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-3
library(survey)
library(srvyr)
library(broom)
library(gssr) # https://kjhealy.github.io/gssr


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-4
data(gss_all)

gss_all



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-5
cont_vars <- c("year", "id", "ballot", "age")
cat_vars <- c("race", "sex", "fefam")
wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssall",             # main weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method
my_vars <- c(cont_vars, cat_vars, wt_vars)



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-6
gss_df <- gss_all |>
  filter(year > 1974 & year < 2021) |> 
  select(all_of(my_vars)) |> 
  mutate(across(everything(), haven::zap_missing), # Convert labeled missing to regular NA
         across(all_of(wt_vars), as.numeric),
         across(all_of(cat_vars), as_factor), 
         across(all_of(cat_vars), fct_relabel, tolower),
         across(all_of(cat_vars), fct_relabel, tools::toTitleCase),
         compwt = oversamp * formwt * wtssall)



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-7
gss_df


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-8
gss_df |> 
  count(fefam) 


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-9
gss_df <- gss_df |> 
  mutate(fefam_d = forcats::fct_recode(fefam,
                                  Agree = "Strongly Agree",
                                  Disagree = "Strongly Disagree"),
    fefam_n = recode(fefam_d, "Agree" = 1, "Disagree" = 0))

# factor version
gss_df |> 
  count(fefam_d) 

# numeric version, 1 is "Agree"
gss_df |> 
  count(fefam_n) 



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-10
out_all <- glm(fefam_n ~ age + sex + race, 
              data = gss_df, 
              family="binomial", 
              na.action = na.omit)

summary(out_all)



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-11
tidy(out_all)


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-12
out_yr <- gss_df |> 
  group_by(year) |> 
  group_map_dfr(possibly(~ tidy(glm(fefam_n ~ age + sex + race, 
                       data = .x, 
                       family = "binomial", 
                       na.action = na.omit), 
                       conf.int = TRUE), 
                     otherwise = NULL))

out_yr



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-13
#| echo: true
#| eval: false
# possibly(~ tidy(glm(...)), otherwise = NULL)


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-14
#| fig-width: 12
#| fig-height: 5
out_yr |> 
  filter(term == "sexFemale") |> 
  ggplot(mapping = aes(x = year, y = estimate,
                       ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_line() + 
  geom_pointrange()


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-15
options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

gss_svy <- gss_df |>
  filter(year > 1974) |>  
  mutate(stratvar = interaction(year, vstrat)) |>
  as_survey_design(id = vpsu,
                     strata = stratvar,
                     weights = wtssall,
                     nest = TRUE)
gss_svy



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-16
gss_svy |>
  drop_na(fefam_d) |> 
  group_by(year, sex, race, fefam_d) |>
  summarize(prop = survey_mean(na.rm = TRUE, 
                               vartype = "ci"))



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-17
out_svy_all <- svyglm(fefam_n ~ age + sex + race, 
                  design = gss_svy, 
                  family = quasibinomial(),
                  na.action = na.omit)

tidy(out_svy_all)


## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-18
out_svy_yrs <- gss_svy |> 
  group_by(year) |> 
  group_map_dfr(possibly(~ tidy(svyglm(fefam_n ~ age + sex + race, 
                       design = .x, 
                       family = quasibinomial(),
                       na.action = na.omit),
                       conf.int = TRUE), 
                     otherwise = NULL))

out_svy_yrs



## -----------------------------------------------------------------------------
#| label: 07b-iterating-safely-19
#| fig-height: 5
#| fig-width: 12
out_svy_yrs |> 
  filter(term == "sexFemale") |> 
  ggplot(mapping = aes(x = year, 
                       y = estimate,
                       ymin = conf.low,
                       ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_line() + 
  geom_pointrange()

