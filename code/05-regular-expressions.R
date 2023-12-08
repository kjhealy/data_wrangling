#| message: FALSE
library(here)      # manage file paths
library(socviz)    # data and some useful functions


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-3"
#| message: TRUE
library(tidyverse) # your friend and mine
library(gapminder) # gapminder data
library(stringr)


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-4"
library(stringr) # It's loaded by default with library(tidyverse)


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-5"

x <- c("apple", "banana", "pear")

str_view(x, "an", html=FALSE)


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-6"
# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-7"

x <- "a\\b"
writeLines(x)
#> a\b

str_view(x, "\\\\") # you need four!



## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-8"
x <- c("apple", "banana", "pear")
str_view(x, "^a")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-8b"
x <- c("apple", "banana", "pear")
str_view(x, "^a")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-9"
str_view(x, "a$")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-10"
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")



## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-11"
str_view(x, "^apple$")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-12"

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")



## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-13"
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-14"
str_view(c("groy", "grey", "griy", "gray"), "gr(e|a)y")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-15"
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-16"
str_view(x, "CC+")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-17"
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, 'C[LX]+')


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-18"
str_view(x, "C{2}")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-19"
str_view(x, "C{2,}")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-20"
str_view(x, "C{2,3}")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-21"
str_view(x, 'C{2,3}?')


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-22"
str_view(x, 'C[LX]+?')


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-23"
fruit # built into stringr


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-24"
str_view(fruit, "(..)\\1", match = TRUE)


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-25"
#| include: FALSE
library(ukelection2019)

ukvote2019 |> 
  group_by(constituency) |> 
  slice_max(votes) |> 
  ungroup() |> 
  select(constituency, party_name) |> 
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
         ley = str_detect(constituency, "(ley$)|(ley )|(leigh$)|(leigh )")) |> 
  pivot_longer(shire:ley, names_to = "toponym")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-26"
place_tab <- ukvote2019 |> 
  group_by(constituency) |> 
  slice_max(votes) |> 
  ungroup() |> 
  select(constituency, party_name) |> 
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
         ley = str_detect(constituency, "(ley$)|(ley )|(leigh$)|(leigh )")) |> 
  pivot_longer(shire:ley, names_to = "toponym")


## -----------------------------------------------------------------------------
#| label: "05-regular-expressions-27"
#| include: FALSE
place_tab |> 
  group_by(party_name, toponym) |> 
  filter(party_name %in% c("Conservative", "Labour")) |> 
  group_by(toponym, party_name) |> 
  summarize(freq = sum(value)) |> 
  mutate(pct = freq/sum(freq)) |> 
  filter(party_name == "Conservative") |> 
  arrange(desc(pct))
  

