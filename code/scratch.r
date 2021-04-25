## ---- include = FALSE--------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(sociz)


## ----------------------------------------------------------------------------------------------------------------
as_tibble(mtcars) %>%
  nest_by(cyl) %>%
  mutate(model = list(lm(mpg ~ wt, data = data))) %>%
  summarize(broom::glance(model))



## ----------------------------------------------------------------------------------------------------------------

gss_sm %>% 
  group_by(bigregion, religion) %>% 
  summarize(total = n()) %>% 
  mutate(freq = total / sum(total),
         pct = round(freq*100, digits = 1)) 



## ----------------------------------------------------------------------------------------------------------------
gss_sm %>%
  group_by(race, sex, degree) %>%
  summarize(n = n(),
            mean_age = mean(age, na.rm = TRUE),
            mean_kids = mean(childs, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100) %>%
  filter(race !="Other") %>% 
  print(n = 23)


## ----------------------------------------------------------------------------------------------------------------
# install.packages("survey")
# install.packages("srvyr")

library(survey)
library(srvyr)

## Unweighted estimates
gss_sm %>% 
  group_by(race, sex, degree) %>% 
  tally() %>% 
  mutate(prop = n/sum(n))

## gss_lon has the survey design vars
gss_lon

options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

gss_wt <- subset(gss_lon, year > 1974) %>%
    mutate(stratvar = interaction(year, vstrat)) %>%
    as_survey_design(ids = vpsu,
                     strata = stratvar,
                     weights = wtssall,
                     nest = TRUE)

## compare to the unweighted estimates above
gss_wt %>% 
    filter(year == 2016) %>%
    group_by(race, sex, degree) %>%
    summarize(prop = survey_mean(na.rm = TRUE))



## ----------------------------------------------------------------------------------------------------------------
library(stringr)
library(lubridate)

chat <- read_lines(here("data", "chat.txt"))

chat %>% 
  
chat_df <- tibble(  
  time = as_datetime(paste(today(), 
                           str_extract(chat, "\\d{2}:\\d{2}:\\d{2}")), tz = "EST"),
  from = str_replace(chat, 
                     "(\\d{2}:\\d{2}:\\d{2} From )(.+?)( to Everyone )(.+?$)", "\\2"),
  message = str_replace(chat, 
                     "(\\d{2}:\\d{2}:\\d{2} From )(.+?)( to Everyone )(.+?$)", "\\4"))

chat_df

chat_df %>% 
  group_by(from) %>% 
  tally() %>% 
  arrange(desc(n))



## ----------------------------------------------------------------------------------------------------------------
qualtrics <- readxl::read_xlsx(here("data", "qualtrics.xlsx")) %>% 
  slice(-(1:2))


## ----------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(stringr)

## Tibble of respondents and goals
df <- tribble(
  ~respondent, ~goal_statement, 
  1, "Beer, hotdogs, good grades, sports",
  2, "GPA matters more than anything",
  3, "GPA, GRADE GRADE GRADE, work",
  4, "Live, laugh, love",
  5, "I drift alone on a sea of confusion",
  6, "Cake only",
  7, "I care nothing for grades"
)

df

## Academic search terms, nb lower case
acad_goals <- c("gpa", "grade")
fun_goals <- c("beer", "hotdogs", "cake")

## Construct "any of these terms" regex using paste()
acad_regex <- paste(acad_goals, collapse = "|") # The `|` means "or"
fun_regex <- paste(fun_goals, collapse = "|")

## We can just detect terms, not meaning! Notice the false positive
df %>% 
  mutate(acad_yn = str_detect(tolower(goal_statement), acad_regex), 
         fun_yn = str_detect(tolower(goal_statement), fun_regex), 
         acad_words_n = str_count(tolower(goal_statement), acad_regex), 
         fun_words_n = str_count(tolower(goal_statement), fun_regex)) 





## ----------------------------------------------------------------------------------------------------------------

# install.packages("qualtRics")
library(tidyverse)
library(stringr)
library(qualtRics)
library(here)

df <- read_survey(here("data", "qualtrics-sample.csv"))  %>% 
  janitor::clean_names()

df

## The rename() lines below are just renaming the g variables
## consistently, so they all have the form g1_something_1,
## g1_something_2, g1_something_3 for however many gs and somethings
## and suffix numbers there are. The key to lengthening multiple values
## is to have a consistent naming scheme like this.

## The pivot operation takes all the "gX_something_X" variables and puts them in three
## new columns: g_number for g1, g2; g_name for the middle part, and n_measure
## for the number suffix. Then score gets whatever the value of the original 
## was, with e.g. row one g1_ext_1 = 1

df %>% 
  select(start_date, id_v2, gender, consent, 
         matches("^g")) %>%  
         rename_with(~ str_replace(.x, "(\\d{1}$)", "_\\1"), matches("^g")) %>% 
         rename_with(~ str_replace(.x, "([a-z]$)", "\\1_1"), matches("^g")) %>% 
    pivot_longer(cols = g1_ext_1:g2_support_3,
    names_to = c("g_number", "g_name", "n_measure"),
    names_sep = "_",
    values_to = "score") %>% 
  rename(gender = gender_1) # whoops, gender starts with a g
  




## ----------------------------------------------------------------------------------------------------------------
c(0, 20, 40, 60, 80)

## seq is surprisingly powerful btw
seq(0, 80, by = 20)

## creating random data 
tmp <- tibble(
  id = 1:1000,
  age = sample(1:85, 1000, replace = TRUE)
)

tmp <-  tmp %>% 
  mutate(agegrp = cut(age, 
                      breaks = c(0, 20, 40, 60, 80, 110),
                      ordered = TRUE)) # order your measure if you wish

tmp %>% 
  count(agegrp)



## ----------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)

## Make some data:
## Three subjects, each with five observations 
## on random (but sequential within-subject) 
## dates between Jan 1 2020 and Jan 1 2021
df <- expand_grid(
  subject = c("A", "B", "C"),
  observation = 1:5) %>% 
  group_by(subject) %>% 
  mutate(date_obs = sort(sample(seq(as_date("2020-01-01"), 
                       as_date("2021-01-01"), by="day"), 5))) %>% 
  ungroup()

## Use lag() to calculate days elapsed between 
## observations within subject.
df %>% 
  group_by(subject) %>% 
  mutate(gap = date_obs - lag(date_obs, order_by = observation)) 


