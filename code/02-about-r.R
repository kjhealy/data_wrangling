#| label: "02-about-r-3"
## Inside code chunks, lines beginning with a # character are comments
## Comments are ignored by R

my_numbers <- c(1, 1, 2, 4, 1, 3, 1, 5) # Anything after a # character is ignored as well



## -----------------------------------------------------------------------------
#| label: "02-about-r-4"
my_numbers 


## -----------------------------------------------------------------------------
#| label: "02-about-r-6"
letters


## -----------------------------------------------------------------------------
#| label: "02-about-r-7"
(31 * 12) / 2^4


## -----------------------------------------------------------------------------
#| label: "02-about-r-8"
sqrt(25)


## -----------------------------------------------------------------------------
#| label: "02-about-r-9"
log(100)

log10(100)



## -----------------------------------------------------------------------------
#| label: "02-about-r-7a"
(31 * 12) / 2^4


## -----------------------------------------------------------------------------
#| label: "02-about-r-8a"
sqrt(25)


## -----------------------------------------------------------------------------
#| label: "02-about-r-9a"
log(100)

log10(100)



## -----------------------------------------------------------------------------
#| label: "02-about-r-10"
4 < 10
4 > 2 & 1 > 0.5 # The "&" means "and"
4 < 2 | 1 > 0.5 # The "|" means "or"
4 < 2 | 1 < 0.5


## -----------------------------------------------------------------------------
#| label: "02-about-r-11"
## A logical test
2 == 2 # Write `=` twice


## -----------------------------------------------------------------------------
#| label: "02-about-r-12"
#| eval: FALSE
## ## This will cause an error, because R will think you are trying to assign a value
## 2 = 2
## 
## ## Error in 2 = 2 : invalid (do_set) left-hand side to assignment


## -----------------------------------------------------------------------------
#| label: "02-about-r-13"
3 != 7 # Write `!` and then `=` to make `!=`


## -----------------------------------------------------------------------------
#| label: "02-about-r-14"
3 < 5 & 7


## -----------------------------------------------------------------------------
#| label: "02-about-r-15"
3 < 5 & 1


## -----------------------------------------------------------------------------
#| label: "02-about-r-16"
TRUE & as.logical(1)


## -----------------------------------------------------------------------------
#| label: "02-about-r-17"
3 < 5 & 3 < 1


## -----------------------------------------------------------------------------
#| label: "02-about-r-18"
0.6 + 0.2 == 0.8


## -----------------------------------------------------------------------------
#| label: "02-about-r-18a"
0.6 + 0.2 == 0.8


## -----------------------------------------------------------------------------
#| label: "02-about-r-18b"
0.6 + 0.2 == 0.8


## -----------------------------------------------------------------------------
#| label: "02-about-r-19"
0.6 + 0.3 == 0.9


## -----------------------------------------------------------------------------
#| label: "02-about-r-20"
print(.1 + .2)
print(.1 + .2, digits=18)

all.equal(.1 + .2, 0.3)


## -----------------------------------------------------------------------------
#| label: "02-about-r-21"
my_numbers # We created this a few minutes ago

letters  # This one is built-in

pi  # Also built-in


## -----------------------------------------------------------------------------
#| label: "02-about-r-22"
#| eval: FALSE
## TRUE
## FALSE
## Inf
## NaN
## NA
## NULL
## 
## for
## if
## while
## break
## function


## -----------------------------------------------------------------------------
#| label: "02-about-r-23"
letters


## -----------------------------------------------------------------------------
#| label: "02-about-r-24"
pi


## -----------------------------------------------------------------------------
#| label: "02-about-r-25"

LETTERS


## -----------------------------------------------------------------------------
#| label: "02-about-r-26"
## name... gets ... this stuff
my_numbers <- c(1, 2, 3, 1, 3, 5, 25, 10)

## name ... gets ... the output of the function `c()`
your_numbers <- c(5, 31, 71, 1, 3, 21, 6, 52)


## -----------------------------------------------------------------------------
#| label: "02-about-r-27"
my_numbers = c(1, 2, 3, 1, 3, 5, 25)

my_numbers


## -----------------------------------------------------------------------------
#| label: "02-about-r-28"

## this object... gets ... the output of this function
my_numbers <- c(1, 2, 3, 1, 3, 5, 25, 10)

your_numbers <- c(5, 31, 71, 1, 3, 21, 6, 52)


## -----------------------------------------------------------------------------
#| label: "02-about-r-29"
my_numbers


## -----------------------------------------------------------------------------
#| label: "02-about-r-30"
my_numbers 


## -----------------------------------------------------------------------------
#| label: "02-about-r-31"
#| eval: FALSE
## ## If you run this you'll get an error
## mean()


## -----------------------------------------------------------------------------
#| label: "02-about-r-32"
## Get the mean of what? Of x.
## You need to tell the function what x is
mean(x = my_numbers)

mean(x = your_numbers)


## -----------------------------------------------------------------------------
#| label: "02-about-r-33"
mean(your_numbers)


## -----------------------------------------------------------------------------
#| label: "02-about-r-34"
#| eval: FALSE
## help(mean)


## -----------------------------------------------------------------------------
#| label: "02-about-r-35"
#| eval: FALSE
## ## quicker
## ?mean


## -----------------------------------------------------------------------------
#| label: "02-about-r-36"
missing_numbers <- c(1:10, NA, 20, 32, 50, 104, 32, 147, 99, NA, 45)

mean(missing_numbers)

mean(missing_numbers, na.rm = TRUE)


## -----------------------------------------------------------------------------
#| label: "02-about-r-37"
## Look at ?mean to see what `trim` does
mean(missing_numbers, na.rm = TRUE, trim = 0.1)


## -----------------------------------------------------------------------------
#| label: "02-about-r-38"
summary(my_numbers)


## -----------------------------------------------------------------------------
#| label: "02-about-r-39"
my_summary <- summary(my_numbers)

my_summary


## -----------------------------------------------------------------------------
#| label: "02-about-r-40"
#| eval: FALSE
## ## rm() function removes objects
## rm(my_summary)
## 
## my_summary
## 
## ## Error: object 'my_summary' not found


## -----------------------------------------------------------------------------
#| label: "02-about-r-41"
c(1:20)


## -----------------------------------------------------------------------------
#| label: "02-about-r-42"
mean(c(1:20))


## -----------------------------------------------------------------------------
#| label: "02-about-r-43"
summary(mean(c(1:20)))


## -----------------------------------------------------------------------------
#| label: "02-about-r-44"
names(summary(mean(c(1:20))))


## -----------------------------------------------------------------------------
#| label: "02-about-r-45"
length(names(summary(mean(c(1:20)))))


## -----------------------------------------------------------------------------
#| label: "02-about-r-46"
c(1:20) |> mean() |> summary() |> names() |>  length()


## -----------------------------------------------------------------------------
#| label: "02-about-r-47"
c(1:20) |> 
  mean() |> 
  summary() |> 
  names() |> 
  length()


## -----------------------------------------------------------------------------
#| label: "02-about-r-48"
## A package containing a dataset rather than functions
library(gapminder)

gapminder


## -----------------------------------------------------------------------------
#| label: "02-about-r-49"
#| eval: FALSE
## ## Do at least once for each package. Once done, not needed each time.
## install.packages("palmerpenguins", repos = "http://cran.rstudio.com")
## 
## ## Needed sometimes, especially after an R major version upgrade.
## update.packages(repos = "http://cran.rstudio.com")
## 


## -----------------------------------------------------------------------------
#| label: "02-about-r-50"
## To load a package, usually at the start of your RMarkdown document or script file
library(palmerpenguins)
penguins


## -----------------------------------------------------------------------------
#| label: "02-about-r-51"
#| message: FALSE
#| echo: FALSE
gtsummary::theme_gtsummary_journal(journal = "jama")
#gtsummary::theme_gtsummary_compact()


## -----------------------------------------------------------------------------
#| label: "02-about-r-52"
## A little glimpse of what we'll do soon
penguins |> 
  select(species, body_mass_g, sex) |> 
  gtsummary::tbl_summary(by = species) #<<


## -----------------------------------------------------------------------------
#| label: "02-about-r-53"
x <- c(1:10)
y <- c(90:100)

x

y


## -----------------------------------------------------------------------------
#| label: "02-about-r-54"
mean(x) # argument names are internal to functions

mean(x = x)

mean(x = y)

x

y



## -----------------------------------------------------------------------------
#| label: "02-about-r-55"
class(my_numbers)
typeof(my_numbers)


## -----------------------------------------------------------------------------
#| label: "02-about-r-56"
summary(my_numbers)

my_smry <- summary(my_numbers) # remember, outputs can be assigned to a name, creating an object

class(summary(my_numbers)) # functions can be nested, and are evaluated from the inside out

class(my_smry) # equivalent to the previous line


## -----------------------------------------------------------------------------
#| label: "02-about-r-57"
typeof(my_smry)
attributes(my_smry)

## In this case, the functions extract the corresponding attribute
class(my_smry)
names(my_smry)


## -----------------------------------------------------------------------------
#| label: "02-about-r-58"
my_int <- c(1, 3, 5, 6, 10)
is.integer(my_int)
is.double(my_int)

my_int <- as.integer(my_int)
is.integer(my_int)

my_chr <- c("Mary", "had", "a", "little", "lamb")
is.character(my_chr)

my_lgl <- c(TRUE, FALSE, TRUE)
is.logical(my_lgl)



## -----------------------------------------------------------------------------
#| label: "02-about-r-59"
## Factors are for storing undordered or ordered categorical variables
x <- factor(c("Yes", "No", "No", "Maybe", "Yes", "Yes", "Yes", "No"))
x

summary(x) # Alphabetical order by default

typeof(x)       # Underneath, a factor is a type of integer ...
attributes(x)   # ... with labels for its numbers, or "levels" 
levels(x)
is.ordered(x)



## -----------------------------------------------------------------------------
#| label: "02-about-r-60"
class(my_numbers)

my_new_vector <- c(my_numbers, "Apple")

my_new_vector # vectors are homogeneous/atomic

class(my_new_vector)


## -----------------------------------------------------------------------------
#| label: "02-about-r-61"
my_dbl <- c(2.1, 4.77, 30.111, 3.14519)
is.double(my_dbl)

my_dbl <- as.integer(my_dbl)

my_dbl



## -----------------------------------------------------------------------------
#| label: "02-about-r-62"
gapminder # tibbles and data frames can contain vectors of different types

class(gapminder)
typeof(gapminder) # hmm



## -----------------------------------------------------------------------------
#| label: "02-about-r-63a"
library(socviz)
titanic
class(titanic)


## -----------------------------------------------------------------------------
#| label: "02-about-r-64a"
## The `$` idiom picks out a named column here; 
## more generally, the named element of a list
titanic$percent  


## -----------------------------------------------------------------------------
#| label: "02-about-r-63"
library(socviz)
titanic
class(titanic)


## -----------------------------------------------------------------------------
#| label: "02-about-r-64"
## The `$` idiom picks out a named column here; 
## more generally, the named element of a list
titanic$percent  


## -----------------------------------------------------------------------------
#| label: "02-about-r-65"
## tibbles are build on data frames 
titanic_tb <- as_tibble(titanic) 
titanic_tb
class(titanic_tb)


## -----------------------------------------------------------------------------
#| label: "02-about-r-66"
gss_sm


## -----------------------------------------------------------------------------
#| label: "02-about-r-67"
my_numbers

result1 <- my_numbers + 1



## -----------------------------------------------------------------------------
#| label: "02-about-r-67a"
my_numbers

result1 <- my_numbers + 1



## -----------------------------------------------------------------------------
#| label: "02-about-r-68"
result1


## -----------------------------------------------------------------------------
#| label: "02-about-r-69"
result2 <- my_numbers + my_numbers



## -----------------------------------------------------------------------------
#| label: "02-about-r-69a"
result2 <- my_numbers + my_numbers



## -----------------------------------------------------------------------------
#| label: "02-about-r-70"
result2


## -----------------------------------------------------------------------------
#| label: "02-about-r-71"
#| warning: TRUE
two_nums <- c(5, 10)

result3 <- my_numbers + two_nums



## -----------------------------------------------------------------------------
#| label: "02-about-r-71a"
#| warning: TRUE
two_nums <- c(5, 10)

result3 <- my_numbers + two_nums



## -----------------------------------------------------------------------------
#| label: "02-about-r-72"
result3


## -----------------------------------------------------------------------------
#| label: "02-about-r-73"
#| warning: TRUE
three_nums <- c(1, 5, 10)

result4 <- my_numbers + three_nums



## -----------------------------------------------------------------------------
#| label: "02-about-r-73a"
#| warning: TRUE
three_nums <- c(1, 5, 10)

result4 <- my_numbers + three_nums



## -----------------------------------------------------------------------------
#| label: "02-about-r-74"
result4

