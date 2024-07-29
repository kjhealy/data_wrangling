#| label: "07-iterating-on-data-2"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(gapminder) # inescapable

library(DBI) # DBMS interface layer
library(duckdb) # Local database server



## -----------------------------------------------------------------------------
gapminder


## -----------------------------------------------------------------------------
gapminder |> 
  select(gdpPercap, lifeExp)


## -----------------------------------------------------------------------------
gapminder |> 
  filter(continent == "Europe", 
         year == 1977)


## -----------------------------------------------------------------------------
gapminder |> 
  group_by(continent) |> 
  summarize(lifeExp = mean(lifeExp), 
            pop = mean(pop), 
            gdpPercap = mean(gdpPercap))


## -----------------------------------------------------------------------------
gapminder_xtra <- read_csv(here("data", "gapminder_xtra.csv"))
gapminder_xtra


## -----------------------------------------------------------------------------
gapminder_xtra


## -----------------------------------------------------------------------------
gapminder


## -----------------------------------------------------------------------------
continent_tbl <- read_tsv(here("data", "continent_tab.tsv")) 
country_tbl <- read_tsv(here("data", "country_tab.tsv"))
year_tbl <-  read_tsv(here("data", "year_tab.tsv"))  
  
continent_tbl

gapminder


## -----------------------------------------------------------------------------
continent_tbl

country_tbl



## -----------------------------------------------------------------------------
country_tbl

year_tbl


## -----------------------------------------------------------------------------
# library(DBI)

con <- dbConnect(duckdb::duckdb(), path = ":memory:")


## -----------------------------------------------------------------------------
copy_to(
  dest = con, 
  df = nycflights13::flights, 
  name = "flights",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum",
    "dest"
    )
  )


## -----------------------------------------------------------------------------
flights_db <- tbl(con, "flights")

flights_db


## -----------------------------------------------------------------------------
flights_db |> select(year:day, dep_delay, arr_delay)



## -----------------------------------------------------------------------------
flights_db |> filter(dep_delay > 240) 



## -----------------------------------------------------------------------------
flights_db |>
  group_by(dest) |>
  summarise(mean_dep_delay = mean(dep_delay))


## -----------------------------------------------------------------------------
tailnum_delay_db <-  
  flights_db |> 
  group_by(tailnum) |>
  summarise(
    mean_dep_delay = mean(dep_delay),
    mean_arr_delay = mean(arr_delay),
    n = n()) |>
  filter(n > 100) |> 
  arrange(desc(mean_arr_delay))


## -----------------------------------------------------------------------------
tailnum_delay_db


## -----------------------------------------------------------------------------
tailnum_delay <- 
  tailnum_delay_db |>  
  collect()

tailnum_delay


## -----------------------------------------------------------------------------
## Copy over the "planes" dataset to the same "con" DuckDB connection.
copy_to(
    dest = con, 
    df = nycflights13::planes, 
    name = "planes",
    temporary = FALSE, 
    indexes = "tailnum"
    )

## List tables in our "con" database connection (i.e. now "flights" and "planes")
dbListTables(con)

## Reference from dplyr
planes_db <-  tbl(con, 'planes')


## -----------------------------------------------------------------------------
# Still not done for realsies!
left_join(
    flights_db,
    planes_db %>% rename(year_built = year),
    by = "tailnum" ## Important: Be specific about the joining column
) |> 
    select(year, month, day, dep_time, arr_time, carrier, flight, tailnum,
           year_built, type, model) 


## -----------------------------------------------------------------------------
dbDisconnect(con)

