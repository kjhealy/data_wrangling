#' Starting with the data, SOMETHING is passed from stage
#' to stage of the pipe:

library(dplyr)

mtcars %>%
  filter(mpg > 20) %>%
  select(mpg:wt)

#' Tidyverse-friendly functions implicitly understand what
#' THE THING is, because they take as their first argument
#' `.data`, and the pipe `%>%` can be thought of as passing
#' along a thing or object, to it---"this", "what we're computing
#' on right now", "the thing", ".", etc. In this case, what gets
#' passed on is `mtcars`, then a filtered version of it etc. We can
#' make this explicit if we want, though we normally don't have to:

mtcars %>%
  filter(., mpg > 20) %>%
  select(., mpg:wt)

#' This is the same as nesting the functions:
#' Here filter(mtcars, mpg > 20) will be evaluated first,
#' and the result is then evaluated by select()

select(filter(mtcars, mpg > 20), mpg:wt)

#' But while putting nrow() at the end of the pipeline by itself will work,
#' wrapping it in print() will cause R to lose track
#' of what THE THING is:

mtcars %>%
  filter(mpg > 20) %>%
  select(mpg:wt) %>%
  nrow() ##' works

mtcars %>%
  filter(mpg > 20) %>%
  select(mpg:wt) %>%
  print(nrow())  #' Fails

#' So we are obliged to remind it explicitly:

mtcars %>%
  filter(mpg > 20) %>%
  select(mpg:wt) %>%
  print(nrow(.)) #' explicitly mention/pass along THE THING

#' Wrapping a series of instructions in braces allows them to be
#' evaluated together while in the pipeline:

mtcars %>%
  filter(mpg > 20) %>%
  select(mpg:wt) %>%
  {
  print(nrow(.))       #' Print the actual number of rows in the THING
  if(nrow(.) > 10)     #' If the THING has more than 10 rows,
     head(., 10)       #' show the first ten rows (and then stop)
  else if(nrow(.) > 5) #' But if it has more than 5 rows
     head(., 5)        #' show the first five rows (and then stop)
  else                 #' But if it has between 0 and 4 rows,
    .                  #' Just print the thing.
  }

#' This kind of a weird bit of code to write as-is, though.
#' This is not the way we'd write a proper function. It seems like more of demo of something?
