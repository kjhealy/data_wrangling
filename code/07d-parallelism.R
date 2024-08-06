#| label: "08-parallel-01"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine

## Magic new package
# install.packages("furrr")
library(furrr) # Also loads `future`


## -----------------------------------------------------------------------------
gss_sm |> 
  count(bigregion)


## -----------------------------------------------------------------------------
out <- mtcars |> 
  split(mtcars$cyl)
summary(out) # mtcars split into a list of data frames by the `cyl` variable


## -----------------------------------------------------------------------------
out <- mtcars |> 
  group_split(cyl)
out


## -----------------------------------------------------------------------------
out <- mtcars |> 
  group_split(cyl) |> 
  map(\(df) lm(mpg ~ wt + hp + gear, data = df))  
  



## -----------------------------------------------------------------------------
mtcars |> 
  group_split(cyl) |> 
  map(\(df) lm(mpg ~ wt + hp + gear, data = df))  |> 
  map(summary) |> 
  map_dbl("r.squared")
  



## -----------------------------------------------------------------------------
mtcars |> 
  group_split(cyl) |> 
  map(\(df) lm(mpg ~ wt + hp + gear, data = df))  |> 
  map(summary) |> 
  map_dbl("r.squared")


## -----------------------------------------------------------------------------
mtcars |>
  group_by(cyl) |> 
  nest() |> 
  mutate(model = map(data, \(df) lm(mpg ~ wt + hp + gear, data = df)), 
         perf = map(model, broom::glance)) |> 
  unnest(perf)


## -----------------------------------------------------------------------------
## From Henrik Bengtsson's documentation for future/furr
slow_sum <- function(x) {
  sum <- 0
  for (value in x) {
    Sys.sleep(1.0)  ## one-second slowdown per value
    sum <- sum + value
  }
  sum
}

# This takes > ten seconds to run.
tictoc::tic()
slow_sum(1:10)
tictoc::toc()


## -----------------------------------------------------------------------------
## How many cores do we have?
parallelly::availableCores()


## -----------------------------------------------------------------------------
# library(furrr) # We did this already
plan(multisession) # Default will use the available resources
## Note difference between multisession and multicore 
## [tl;dr: the latter is faster, but you can't use it on Windows]



## -----------------------------------------------------------------------------
# Another slow function (from 
# Grant McDermott this time)
slow_square <- function(x = 1) {
    x_sq <- x^2 
    out <-  tibble(value = x, value_sq = x_sq)
    Sys.sleep(2) # Zzzz
    out
}

tictoc::tic("Serially")
## This is of course way slower than just writing 
## slow_square(1:15) but nvm that for now
serial_out <- map(1:15, slow_square) |> 
  list_rbind()
tictoc::toc()



## -----------------------------------------------------------------------------
tictoc::tic("Parallelized")
parallel_out <-  future_map_dfr(1:15, slow_square)
tictoc::toc()



## -----------------------------------------------------------------------------
identical(serial_out, parallel_out)


## -----------------------------------------------------------------------------
basename(fs::dir_ls(here::here("avhrr")))


## -----------------------------------------------------------------------------
basename(fs::dir_ls(here::here("avhrr", "202402")))


## -----------------------------------------------------------------------------
## Seasons for plotting
season <-  function(in_date){
  br = yday(as.Date(c("2019-03-01",
                      "2019-06-01",
                      "2019-09-01",
                      "2019-12-01")))
  x = yday(in_date)
  x = cut(x, breaks = c(0, br, 366))
  levels(x) = c("Winter", "Spring", "Summer", "Autumn", "Winter")
  x
}

season_lab <-  tibble(yrday = yday(as.Date(c("2019-03-01",
                                             "2019-06-01",
                                             "2019-09-01",
                                             "2019-12-01"))),
                      lab = c("Spring", "Summer", "Autumn", "Winter"))




## -----------------------------------------------------------------------------
#install.packages("ncdf4")
#install.packages("terra")
library(terra)

## For the filename processing
## This one gives you an unknown number of chunks each with approx n elements
chunk <- function(x, n) split(x, ceiling(seq_along(x)/n))

## This one gives you n chunks each with an approx equal but unknown number of elements
chunk2 <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))

## All the daily .nc files:
all_fnames <- as.character(fs::dir_ls(here("avhrr"), recurse = TRUE, glob = "*.nc"))
chunked_fnames <- chunk(all_fnames, 25)

length(all_fnames)

length(chunked_fnames)


## -----------------------------------------------------------------------------
tmp <- ncdf4::nc_open(all_fnames[10000])
tmp


## -----------------------------------------------------------------------------
tmp <- terra::rast(all_fnames[10000])
tmp


## -----------------------------------------------------------------------------
tmp <- terra::rast(all_fnames[10000])["sst"]
tmp


## -----------------------------------------------------------------------------
tmp2 <- terra::rast(chunked_fnames[[10]])
tmp2



## -----------------------------------------------------------------------------
process_raster <- function(fnames, crop_area = c(-80, 0, 0, 60), var = "sst") {

  tdf <- terra::rast(as.character(fnames))[var] |>
    terra::rotate() |>   # Fix 0-360 lon
    terra::crop(crop_area) # Manually crop to a defined box. Default is Atlantic lat/lon box

  wts <- terra::cellSize(tdf, unit = "km") # For scaling

  data.frame(
    date = terra::time(tdf),
    # global() calculates a quantity for the whole grid on a particular SpatRaster
    # so we get one weighted mean per file that comes in
    wt_mean_sst = terra::global(tdf, "mean", weights = wts, na.rm=TRUE)$weighted_mean
  )
}


## -----------------------------------------------------------------------------
process_raster(all_fnames[1000]) |> 
  as_tibble()


## -----------------------------------------------------------------------------
process_raster(chunked_fnames[[500]]) |> 
  as_tibble()



## -----------------------------------------------------------------------------
# library(furrr) # We did this already
# plan(multisession) 

tictoc::tic("Terra Method")
df <- future_map(chunked_fnames, process_raster) |>
  list_rbind() |>
  as_tibble() |> 
  mutate(date = ymd(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         yrday = lubridate::yday(date),
         season = season(date))
tictoc::toc()

dim(df)


## -----------------------------------------------------------------------------
df |> 
  slice_sample(n = 10)


## -----------------------------------------------------------------------------
## Seas of the world polygons
seas <- sf::read_sf(here("seas")) 

seas


## -----------------------------------------------------------------------------
## Rasterize the seas polygons using one of the nc files
## as a reference grid for the rasterization process
## They're all on the same grid so it doesn't matter which one
one_raster <- all_fnames[1]
seas_vect <- terra::vect(seas)
tmp_tdf_seas <- terra::rast(one_raster)["sst"] |>
  terra::rotate()
seas_zonal <- terra::rasterize(seas_vect, tmp_tdf_seas, "NAME")



## -----------------------------------------------------------------------------
plot(seas_zonal, legend = FALSE)


## -----------------------------------------------------------------------------
# If we don't do this it can't be passed around
# across the processes that future_map() will spawn
seas_zonal_wrapped <- terra::wrap(seas_zonal)


## -----------------------------------------------------------------------------
process_raster_zonal <- function(fnames, var = "sst") {

  tdf_seas <- terra::rast(as.character(fnames))[var] |>
    terra::rotate() |>
    terra::zonal(terra::unwrap(seas_zonal_wrapped), mean, na.rm = TRUE)

  colnames(tdf_seas) <- c("ID", paste0("d_", terra::time(terra::rast(as.character(fnames))[var])))
  tdf_seas |>
    pivot_longer(-ID, names_to = "date", values_to = "sst_wt_mean")

}


## -----------------------------------------------------------------------------
process_raster_zonal(all_fnames[10000])


## -----------------------------------------------------------------------------
process_raster_zonal(chunked_fnames[[1]])


## -----------------------------------------------------------------------------
tictoc::tic("Big op")
seameans_df <- future_map(chunked_fnames, process_raster_zonal) |>
  list_rbind() |>
  mutate(date = str_remove(date, "d_"),
         date = ymd(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         yrday = lubridate::yday(date),
         season = season(date)) |>
  rename(sea = ID)
tictoc::toc()



## -----------------------------------------------------------------------------
seameans_df

