## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, 
                      fig.retina = 3, fig.align = "center")


## ----packages-data, include=FALSE----------------------------------------------------------
library(flipbookr)
library(cowplot)
ggplot2::theme_set(theme_cowplot())



## ----xaringanExtra, echo=FALSE-------------------------------------------------------------
xaringanExtra::use_xaringan_extra(c("tile_view"))
xaringanExtra::use_animate_css()
xaringanExtra::use_animate_all("fade")
xaringanExtra::use_clipboard()


## ----03-dplyr-basics-1, message = TRUE-----------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine


## ----03-dplyr-basics-2---------------------------------------------------------------------
## library(socviz) # if not loaded
gss_sm


## ----03-dplyr-basics-3---------------------------------------------------------------------
## Just take a look at the columns we will work on
gss_sm %>% 
  select(id, bigregion, religion)


## ----03-dplyr-basics-4---------------------------------------------------------------------

gss_sm %>% 
  group_by(bigregion)


## ----03-dplyr-onecol-summary, include = FALSE----------------------------------------------
gss_sm %>% 
  group_by(bigregion) %>% 
  summarize(total = n())


## ----03-dplyr-twocol-summary, include = FALSE----------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% 
  summarize(total = n())


## ----03-dplyr-freq, include = FALSE--------------------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% 
  summarize(total = n()) %>% 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## ----03-dplyr-basics-5---------------------------------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% #<<
  summarize(total = n()) %>% 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## ----03-dplyr-basics-6---------------------------------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% 
  summarize(total = n()) %>% 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) #<<


## ----03-dplyr-basics-7---------------------------------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% #<<
  summarize(total = n()) %>% #<<
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) 


## ----03-dplyr-basics-8---------------------------------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% #<<
  summarize(n = n()) #<<


## ----03-dplyr-basics-9---------------------------------------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% 
  tally() #<<


## ----03-dplyr-basics-10--------------------------------------------------------------------
gss_sm %>% 
  count(bigregion, religion) #<<


## ----03-dplyr-basics-11, eval = FALSE------------------------------------------------------
## gss_sm %>%
##   count(bigregion, religion) %>%
##   pivot_wider(names_from = bigregion, values_from = n) %>%  #<<
##   kable()


## ----03-dplyr-basics-12, echo = FALSE------------------------------------------------------
gss_sm %>% 
  count(bigregion, religion) %>% 
  pivot_wider(names_from = bigregion, values_from = n) %>% 
  kable()  


## ----03-dplyr-basics-13, echo = FALSE------------------------------------------------------
theme_set(cowplot::theme_cowplot())


## ----03-dplyr-basics-14, fig.height=4, fig.width=15----------------------------------------
gss_sm %>% 
  group_by(bigregion, religion) %>% 
  tally() %>% 
  mutate(pct = round((n/sum(n))*100), 1) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = pct, y = reorder(religion, -pct), fill = religion)) + 
  geom_col() +
    labs(x = "Percent", y = NULL) +
    guides(fill = FALSE) + 
    facet_wrap(~ bigregion, nrow = 1)


## ----03-dplyr-basics-15--------------------------------------------------------------------
rel_by_region <- gss_sm %>% #<<
  count(bigregion, religion) %>% 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## ----03-dplyr-basics-16--------------------------------------------------------------------
gss_sm %>% 
  count(bigregion, religion) %>% 
  mutate(pct = round((n/sum(n))*100, 1)) -> #<<
rel_by_region #<<

rel_by_region


## ----03-dplyr-basics-17--------------------------------------------------------------------
gss_tab <- gss_sm %>% 
  count(bigregion, religion) 


## ----03-dplyr-basics-18--------------------------------------------------------------------
gss_sm %>% 
  count(bigregion, religion) -> gss_tab  
  


## ----03-dplyr-basics-19--------------------------------------------------------------------
rel_by_region <- gss_sm %>% 
  count(bigregion, religion) %>% 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## ----03-dplyr-basics-20--------------------------------------------------------------------
## Each region should sum to ~100
rel_by_region %>% 
  group_by(bigregion) %>% 
  summarize(total = sum(pct)) 



## ----03-dplyr-basics-21--------------------------------------------------------------------
rel_by_region <- gss_sm %>% 
  count(bigregion, religion) %>% #<< 
  mutate(pct = round((n/sum(n))*100, 1)) 


## ----03-dplyr-basics-22--------------------------------------------------------------------
rel_by_region %>% 
  summarize(total = sum(pct))


## ----03-dplyr-basics-23--------------------------------------------------------------------
rel_by_region <- gss_sm %>% 
  group_by(bigregion, religion) %>% #<<
  tally() %>% #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## ----03-dplyr-basics-24--------------------------------------------------------------------
# Check
rel_by_region %>% 
  group_by(bigregion) %>% 
  summarize(total = sum(pct))



## ----03-dplyr-basics-25, echo = FALSE------------------------------------------------------
theme_set(cowplot::theme_minimal_grid())


## ----03-dplyr-basics-26, fig.height = 4, fig.width=10--------------------------------------
gss_sm %>% 
  group_by(race, sex, degree) %>% 
  summarize(n = n(), 
            mean_age = mean(age, na.rm = TRUE), 
            mean_kids = mean(childs, na.rm = TRUE)) %>% 
  mutate(pct = n/sum(n)*100) %>% 
  filter(race !="Other") %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = mean_kids, y = degree)) + # I'm sorry I can't talk more about the graphs
  geom_col() + facet_grid(sex ~ race) + 
  labs(x = "Average number of Chidren", y = NULL)


## ----03-kid-pipeline, include = FALSE------------------------------------------------------
gss_sm %>% 
  group_by(race, sex, degree) %>% 
  summarize(n = n(), 
            mean_age = mean(age, na.rm = TRUE), 
            mean_kids = mean(childs, na.rm = TRUE)) %>% 
  mutate(pct = n/sum(n)*100) %>% 
  filter(race !="Other") %>% 
  drop_na() %>% 
  summarize(grp_totpct = sum(pct))


## ----03-dplyr-basics-27--------------------------------------------------------------------
# library(socviz)
organdata


## ----03-dplyr-basics-28--------------------------------------------------------------------
organdata %>% 
  filter(consent_law == "Informed" & donors > 15) 


## ----03-dplyr-basics-29--------------------------------------------------------------------
organdata %>% 
  select(country, year, where(is.integer)) #<<


## ----03-dplyr-basics-30--------------------------------------------------------------------
organdata %>% 
  select(country, year, where(is.character))


## ----03-dplyr-basics-31--------------------------------------------------------------------
organdata %>% 
  select(country, year, starts_with("gdp")) #<<


## ----03-dplyr-basics-32--------------------------------------------------------------------
organdata %>% 
  filter(country == "Australia" | country == "Canada") 


## ----03-dplyr-basics-33--------------------------------------------------------------------
my_countries <- c("Australia", "Canada", "United States", "Ireland")

organdata %>% 
  filter(country %in% my_countries) #<<


## ----03-dplyr-basics-34--------------------------------------------------------------------
my_countries <- c("Australia", "Canada", "United States", "Ireland")

organdata %>% 
  filter(!(country %in% my_countries)) #<<


## ----03-dplyr-basics-35--------------------------------------------------------------------
`%nin%` <- Negate(`%in%`) # this operator is included in the socviz package


## ----03-dplyr-basics-36--------------------------------------------------------------------
organdata %>% 
  filter(country %nin% my_countries) #<<


## ----03-dplyr-basics-37--------------------------------------------------------------------
gss_sm %>% 
  group_by(race, sex, degree) %>% 
  summarize(n = n(), 
            mean_age = mean(age, na.rm = TRUE), 
            mean_kids = mean(childs, na.rm = TRUE))


## ----03-dplyr-basics-38--------------------------------------------------------------------
organdata %>%  
  group_by(consent_law, country) %>%
  summarize(donors_mean = mean(donors, na.rm = TRUE),
            donors_sd = sd(donors, na.rm = TRUE),
            gdp_mean = mean(gdp, na.rm = TRUE),
            health_mean = mean(health, na.rm = TRUE),
            roads_mean = mean(roads, na.rm = TRUE))


## ----03-across-pipeline-1------------------------------------------------------------------

my_vars <- c("gdp", "donors", "roads")

## nested parens again, but it's worth it
organdata %>% 
  group_by(consent_law, country) %>%
  summarize(across(my_vars,           
                   list(avg = mean),  
                   na.rm = TRUE))     


## ----03-dplyr-basics-40--------------------------------------------------------------------

my_vars <- c("gdp", "donors", "roads")

organdata %>% 
  group_by(consent_law, country) %>%
  summarize(across(my_vars,           
                   list(avg = mean, #<<
                        sd = var, #<<
                        md = median),#<<  
                   na.rm = TRUE))     


## ----03-dplyr-basics-41--------------------------------------------------------------------

my_vars <- c("gdp", "donors", "roads")

organdata %>% 
  group_by(consent_law, country) %>%
  summarize(across(my_vars,           
                   list(mean = mean, #<<
                        var = var, #<<
                        median = median),#<<  
                   na.rm = TRUE))     


## ----03-dplyr-basics-42--------------------------------------------------------------------
organdata %>% 
  group_by(consent_law, country) %>%
  summarize(across(where(is.numeric), #<<       
                   list(mean = mean, 
                        var = var, 
                        median = median),
                   na.rm = TRUE)) %>% 
    print(n = 3) # just to save slide space


## ----03-dplyr-basics-43--------------------------------------------------------------------
organdata %>% 
  group_by(consent_law, country) %>%
  summarize(across(where(is.numeric),        
                   list(mean = mean, 
                        var = var, 
                        median = median),
                   na.rm = TRUE, 
                   .names = "{fn}_{col}")) %>% #<<
  print(n = 3) 


## ----03-dplyr-basics-44--------------------------------------------------------------------
organdata %>% 
  mutate(across(where(is.character), toupper)) %>% 
  select(where(is.character))


## ----03-dplyr-basics-45--------------------------------------------------------------------
organdata %>% 
  group_by(consent_law, country) %>%
  summarize(donors = mean(donors, na.rm = TRUE)) %>% 
  arrange(donors) %>% ##< 
  print(n = 5)


## ----03-dplyr-basics-46--------------------------------------------------------------------
organdata %>% 
  group_by(consent_law, country) %>%
  summarize(donors = mean(donors, na.rm = TRUE)) %>% 
  arrange(desc(donors)) %>%  ##<
  print(n = 5)


## ----03-dplyr-basics-47--------------------------------------------------------------------
organdata %>% 
  group_by(consent_law, country) %>%
  summarize(donors = mean(donors, na.rm = TRUE)) %>% 
  slice_max(donors, n = 5) #<<


## ----03-dplyr-basics-48--------------------------------------------------------------------
## Data on COVID-19
library(covdata)

covnat_weekly 


## ----03-dplyr-basics-49--------------------------------------------------------------------
covnat_weekly %>% 
  filter(cname == "United States") %>% 
  select(date, cname, iso3, cases) %>% 
  mutate(cumulative = cumsum(cases)) 



## ----03-dplyr-basics-50--------------------------------------------------------------------
covnat_weekly %>% 
  select(date, cname, iso3, deaths) %>% 
  filter(cname == "United States") %>% 
  filter(cume_dist(desc(deaths)) < 0.1) # i.e. Top 10%



## ----03-dplyr-basics-51--------------------------------------------------------------------
covus %>% 
  filter(measure == "death") %>% 
  group_by(state) %>% 
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")


## ----03-dplyr-basics-52--------------------------------------------------------------------
my_vec <- c(1:20)
my_vec
lag(my_vec) # first element has no lag

my_vec - lag(my_vec)



## ----03-dplyr-basics-53--------------------------------------------------------------------
covus %>%
  select(-data_quality_grade) %>% 
  filter(measure == "death") %>%
  group_by(state) %>%
  arrange(date) %>% 
  mutate(deaths_daily = count - lag(count, order_by = date)) %>% 
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")
  


## ----03-dplyr-basics-54--------------------------------------------------------------------
my_fun <- function(x) {
  x + 1
}

my_fun # we've created the function; it's just an object

my_fun(x = 1) # But we can supply it with an input!

my_fun(10)


## ----03-dplyr-basics-55--------------------------------------------------------------------
get_daily_count <- function(count, date){
  count - lag(count, order_by = date)
}


## ----03-dplyr-basics-56--------------------------------------------------------------------
covus %>%
  filter(measure == "death") %>%
  select(-data_quality_grade) %>% 
  group_by(state) %>%
  arrange(date) %>% 
  mutate(deaths_daily = get_daily_count(count, date)) %>% 
  arrange(state, desc(date)) %>% 
  filter(state %in% "NY")
  


## ----03-dplyr-basics-57--------------------------------------------------------------------
# install.packages("slider")
library(slider)


## ----03-dplyr-basics-58--------------------------------------------------------------------
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


## ----03-dplyr-basics-59--------------------------------------------------------------------
gss_sm


## ----03-relocate-pipeline, include = FALSE-------------------------------------------------
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


## ---- echo = FALSE-------------------------------------------------------------------------

# Make some sample data with tribb
df <- tribble(~id, ~ prop1, ~prop2,
              "A", 0.1,      0.2,
              "B", 0.1,      0.21, 
              "C", 0.11,     0.2,
              "D", 0.1,      0.1)


## ------------------------------------------------------------------------------------------
df


## ------------------------------------------------------------------------------------------
df %>% 
  filter(prop1 + prop2 > 0.3)


## ------------------------------------------------------------------------------------------
df %>% 
  filter(prop1 + prop2 == 0.3)


## ------------------------------------------------------------------------------------------
df %>% 
  mutate(prop3 = prop1 + prop2) %>% 
  filter(prop3 == 0.3)


## ------------------------------------------------------------------------------------------
df %>% 
  filter(prop1*100 + prop2*100 == 0.3*100)


## ------------------------------------------------------------------------------------------
df %>% 
  filter(near(prop1 + prop2, 0.3))


## ------------------------------------------------------------------------------------------
df <- read_csv(here("data", "first_terms.csv"))

df


## ---- echo = FALSE-------------------------------------------------------------------------
## Hex colors for sex
sex_colors <- c("#E69F00", "#993300")

## Group labels
mf_labs <- tibble(M = "Men", F = "Women")

theme_set(cowplot::theme_cowplot())



## ------------------------------------------------------------------------------------------
df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N))



## ------------------------------------------------------------------------------------------
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


## ---- fig.height = 6, fig.width=10---------------------------------------------------------
p_col


## ------------------------------------------------------------------------------------------
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


## ---- fig.height = 6, fig.width=9----------------------------------------------------------
p_line


## ------------------------------------------------------------------------------------------
df_f <- df %>% 
  mutate(party_f = factor(party))

df_f


## ------------------------------------------------------------------------------------------
df_f %>% 
  group_by(party_f) %>% 
  tally()


## ------------------------------------------------------------------------------------------
typeof(df_f$party_f)
levels(df_f$party_f)



## ------------------------------------------------------------------------------------------
df_f <- df %>% 
  mutate(party_f = factor(party, 
                          levels = c("Democrat", 
                                     "Republican", 
                                     "Libertarian")))
df_f %>% 
  group_by(party_f) %>% 
  tally()

levels(df_f$party_f)



## ------------------------------------------------------------------------------------------
df %>% 
  mutate(across(where(is.character), as_factor)) %>% 
  group_by(start_year, party, sex) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))



## ------------------------------------------------------------------------------------------
df %>% 
  mutate(across(where(is.character), as_factor)) %>% 
  group_by(start_year, party, sex, .drop = FALSE) %>% #<<
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))
  


## ------------------------------------------------------------------------------------------
df_c <- df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ungroup() %>%#<<
    complete(start_year, party, sex,#<<
             fill = list(N = 0, freq = 0))#<<


## ------------------------------------------------------------------------------------------
df_c


## ------------------------------------------------------------------------------------------
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


## ---- fig.height = 6, fig.width=9----------------------------------------------------------
p_out

