## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------
## Graphics options and theme-matching
source(here::here("slides", "R", "ragg_and_theme.R"))


## ----packages-data, include=FALSE--------------------------------------------------------------------------------------------------------------------------
library(flipbookr)


## ----xaringanExtra, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------
xaringanExtra::use_xaringan_extra(c("tile_view"))
xaringanExtra::use_animate_css()
xaringanExtra::use_animate_all("fade")
xaringanExtra::use_clipboard()


## ----05-regular-expressions-1, message = FALSE-------------------------------------------------------------------------------------------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions


## ----05-regular-expressions-2, message = TRUE--------------------------------------------------------------------------------------------------------------
library(tidyverse) # your friend and mine
library(gapminder) # gapminder data
library(stringr)


## ----05-regular-expressions-3------------------------------------------------------------------------------------------------------------------------------
library(stringr)


## ----05-regular-expressions-4------------------------------------------------------------------------------------------------------------------------------
x <- c("apple", "banana", "pear")

str_view(x, "an")


## ----05-regular-expressions-5------------------------------------------------------------------------------------------------------------------------------
# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")


## ----05-regular-expressions-6------------------------------------------------------------------------------------------------------------------------------

x <- "a\\b"
writeLines(x)
#> a\b

str_view(x, "\\\\") # you need four!



## ----05-regular-expressions-7------------------------------------------------------------------------------------------------------------------------------
x <- c("apple", "banana", "pear")
str_view(x, "^a")


## ----05-regular-expressions-8------------------------------------------------------------------------------------------------------------------------------
str_view(x, "a$")


## ----05-regular-expressions-9------------------------------------------------------------------------------------------------------------------------------
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")



## ----05-regular-expressions-10-----------------------------------------------------------------------------------------------------------------------------
str_view(x, "^apple$")


## ----05-regular-expressions-11-----------------------------------------------------------------------------------------------------------------------------

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")



## ----05-regular-expressions-12-----------------------------------------------------------------------------------------------------------------------------
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")


## ----05-regular-expressions-13-----------------------------------------------------------------------------------------------------------------------------
str_view(c("groy", "grey", "griy", "gray"), "gr(e|a)y")


## ----05-regular-expressions-14-----------------------------------------------------------------------------------------------------------------------------
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")


## ----05-regular-expressions-15-----------------------------------------------------------------------------------------------------------------------------
str_view(x, "CC+")


## ----05-regular-expressions-16-----------------------------------------------------------------------------------------------------------------------------
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, 'C[LX]+')


## ----05-regular-expressions-17-----------------------------------------------------------------------------------------------------------------------------
str_view(x, "C{2}")


## ----05-regular-expressions-18-----------------------------------------------------------------------------------------------------------------------------
str_view(x, "C{2,}")


## ----05-regular-expressions-19-----------------------------------------------------------------------------------------------------------------------------
str_view(x, "C{2,3}")


## ----05-regular-expressions-20-----------------------------------------------------------------------------------------------------------------------------
str_view(x, 'C{2,3}?')


## ----05-regular-expressions-21-----------------------------------------------------------------------------------------------------------------------------
str_view(x, 'C[LX]+?')


## ----05-regular-expressions-22-----------------------------------------------------------------------------------------------------------------------------
fruit # built into stringr


## ----05-regular-expressions-23-----------------------------------------------------------------------------------------------------------------------------
str_view(fruit, "(..)\\1", match = TRUE)


## ----05-election-01, include=FALSE-------------------------------------------------------------------------------------------------------------------------
library(ukelection2019)

ukvote2019 %>% 
  group_by(constituency) %>% 
  slice_max(votes) %>% 
  ungroup() %>% 
  select(constituency, party_name) %>% 
  mutate(shire = str_detect(constituency, "shire"),
         field = str_detect(constituency, "field"),
         dale = str_detect(constituency, "dale"),
         pool = str_detect(constituency, "pool"),
         ton = str_detect(constituency, "(ton$)|(ton )"),
         wood = str_detect(constituency, "(wood$)|(wood )"),
         saint = str_detect(constituency, "(St )|(Saint)"),
         port = str_detect(constituency, "(Port)|(port)"),
         ford = str_detect(constituency, "(ford$)|(ford )"),
         by = str_detect(constituency, "(by$)|(by )"),
         boro = str_detect(constituency, "(boro$)|(boro )|(borough$)|(borough )"),
         ley = str_detect(constituency, "(ley$)|(ley )|(leigh$)|(leigh )")) %>% 
  pivot_longer(shire:ley, names_to = "toponym")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
place_tab <- ukvote2019 %>% 
  group_by(constituency) %>% 
  slice_max(votes) %>% 
  ungroup() %>% 
  select(constituency, party_name) %>% 
  mutate(shire = str_detect(constituency, "shire"),
         field = str_detect(constituency, "field"),
         dale = str_detect(constituency, "dale"),
         pool = str_detect(constituency, "pool"),
         ton = str_detect(constituency, "(ton$)|(ton )"),
         wood = str_detect(constituency, "(wood$)|(wood )"),
         saint = str_detect(constituency, "(St )|(Saint)"),
         port = str_detect(constituency, "(Port)|(port)"),
         ford = str_detect(constituency, "(ford$)|(ford )"),
         by = str_detect(constituency, "(by$)|(by )"),
         boro = str_detect(constituency, "(boro$)|(boro )|(borough$)|(borough )"),
         ley = str_detect(constituency, "(ley$)|(ley )|(leigh$)|(leigh )")) %>% 
  pivot_longer(shire:ley, names_to = "toponym")


## ----05-election-02, include=FALSE-------------------------------------------------------------------------------------------------------------------------
place_tab %>% 
  group_by(party_name, toponym) %>% 
  filter(party_name %in% c("Conservative", "Labour")) %>% 
  group_by(toponym, party_name) %>% 
  summarize(freq = sum(value)) %>% 
  mutate(pct = freq/sum(freq)) %>% 
  filter(party_name == "Conservative") %>% 
  arrange(desc(pct))
  

