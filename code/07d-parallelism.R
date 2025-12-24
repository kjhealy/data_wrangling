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
## From Henrik Bengtsson's documentation for future/furrr
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
library(tidyverse)
library(here)

# Set up parallel processing
library(mirai)

status()

# Summon daemons. Do not summon more daemons than you have cores. People often choose one less than the number of cores.
daemons(15)

status()



## -----------------------------------------------------------------------------
# Another slow function (from
# Grant McDermott this time)
slow_square <- function(x = 1) {
    x_sq <- x^2
    ## We explicitly namespace all our package function calls
    out <-  tibble::tibble(value = x, value_sq = x_sq)
    Sys.sleep(2) # Zzzz
    out
}

tictoc::tic("Serially")
## This is of course way slower than just writing
## slow_square(1:20) but nvm that for now
serial_out <- map(1:20, slow_square) |>
  list_rbind()
tictoc::toc()



## -----------------------------------------------------------------------------
tictoc::tic("Parallelized")
parallel_out <- 1:20 |>
  map(in_parallel(\(x) slow_square(x),
      slow_square = slow_square)) |>
        list_rbind()
tictoc::toc()



## -----------------------------------------------------------------------------
identical(serial_out, parallel_out)


## -----------------------------------------------------------------------------
#| eval: FALSE

# # ❌ This won't work - external dependencies not declared
# my_data <- c(1, 2, 3)
# map(1:3, in_parallel(\(x) mean(my_data)))
# 
# # ✅ This works - dependencies explicitly provided
# my_data <- c(1, 2, 3)
# map(1:3, in_parallel(\(x) mean(my_data), my_data = my_data))
# 
# # ✅ Package functions need explicit namespacing
# map(1:3, in_parallel(\(x) vctrs::vec_init(integer(), x)))
# 
# # ✅ Or load packages within the function
# map(
#   1:3,
#   in_parallel(\(x) {
#     library(vctrs)
#     vec_init(integer(), x)
#   })
# 
# 


## -----------------------------------------------------------------------------
#| eval: false

# #' Get a year-month folder of NCDF Files from NOAA
# #'
# #' @param url The endpoint URL of the AVHRR data, <https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/>
# #' @param local A local file path for the raw data folders, i.e. where all the year-month dirs go. Defaults to a local version under raw/ of the same path as the NOAA website.
# #' @param subdir The subdirectory of monthly data to get. A character string of digits, of the form "YYYYMM". No default. Gets appended to `local`
# #'
# #' @return  A directory of NCDF files.
# #' @export
# #'
# #'
# get_nc_files <- function(
#   url = "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/",
#   local = here::here(
#     "raw/www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
#   ),
#   subdir
# ) {
#   localdir <- here::here(local, subdir)
# 
#   if (!fs::dir_exists(localdir)) {
#     fs::dir_create(localdir)
#   }
# 
#   files <- rvest::read_html(paste0(url, subdir)) |>
#     rvest::html_elements("a") |>
#     rvest::html_text2()
#   files <- subset(files, str_detect(files, "nc"))
# 
#   full_urls <- paste0(url, subdir, "/", files)
#   full_outpaths <- paste0(localdir, "/", files)
# 
#   walk2(full_urls, full_outpaths, \(x, y) {
#     httr::GET(x, httr::write_disk(y, overwrite = TRUE))
#   })
# }
# 


## -----------------------------------------------------------------------------
#| eval: FALSE

# ## Functions, incl. actual get_nc_files() function to get 1 year-month's batch of files.
# source(here("R", "functions.R"))
# 
# ### Initial get. Only have to do this once.
# ## We try to be nice.
# 
# # Data collection starts in September 1981
# first_yr <- paste0("1981", sprintf('%0.2d', 9:12))
# yrs <- 1982:2024
# months <- sprintf('%0.2d', 1:12)
# subdirs <- c(first_yr, paste0(rep(yrs, each = 12), months))
# 
# slowly_get_nc_files <- slowly(get_nc_files)
# 
# walk(subdirs, \(x) slowly_get_nc_files(subdir = x))


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
#' Process NCDF temperature rasters
#'
#' Use terra to process a temperature NCDF file
#'
#' @param fnames
#' @param crop_area
#' @param layerinfo
#'
#' @returns
#' @export
#'
#' @examples
process_raster <- function(
  fnames,
  crop_area = c(-80, 0, 0, 60),
  layerinfo = NULL
) {
  nc_layerinfo <- tibble::tibble(
    num = c(1:4),
    raw_name = c("anom_zlev=0", "err_zlev=0", "ice_zlev=0", "sst_zlev=0"),
    name = c("anom", "err", "ice", "sst")
  )

  if (!is.null(layerinfo)) {
    nc_layerinfo <- layerinfo
  }

  tdf <- terra::rast(fnames) |>
    terra::rotate() |> # Convert 0 to 360 lon to -180 to +180 lon
    terra::crop(crop_area) # Manually crop to a defined box.  Default is roughly N. Atlantic lat/lon box

  wts <- terra::cellSize(tdf, unit = "km") # For scaling

  # global() calculates a quantity for the whole grid on a particular SpatRaster
  # so we get one weighted mean per file that comes in
  out <- data.frame(
    date = terra::time(tdf),
    means = terra::global(tdf, "mean", weights = wts, na.rm = TRUE)
  )
  out$var <- rownames(out)
  out$var <- gsub("_.*", "", out$var)
  out <- reshape(out, idvar = "date", timevar = "var", direction = "wide")

  colnames(out) <- gsub("weighted_mean\\.", "", colnames(out))
  out
}


## -----------------------------------------------------------------------------
process_raster(all_fnames[1000]) |>
  as_tibble()


## -----------------------------------------------------------------------------
process_raster(chunked_fnames[[500]]) |>
  as_tibble()



## -----------------------------------------------------------------------------
tictoc::tic("Terra Method")
df <- chunked_fnames |>
  map(in_parallel(
    \(x) process_raster(x),
    process_raster = process_raster
  )) |>
  list_rbind() |>
  as_tibble() |>
  mutate(
    date = ymd(date),
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date),
    yrday = lubridate::yday(date),
    season = season(date)
  )

dim(df)
tictoc::toc()


## -----------------------------------------------------------------------------
df |>
  slice_sample(n = 10)


## -----------------------------------------------------------------------------
# All the seas with rasterize() and zonal()
# Seas of the world polygons from https://www.marineregions.org/downloads.php,
# IHO Sea Areas V3 shapefile.
seas <- sf::read_sf(here("seas"))

seas


## -----------------------------------------------------------------------------
## Rasterize the seas polygons using one of the nc files
## as a reference grid for the rasterization process
one_raster <- all_fnames[1]
seas_vect <- terra::vect(seas)
tmp_tdf_seas <- terra::rast(one_raster)["sst"] |>
  rotate()
seas_zonal <- rasterize(seas_vect, tmp_tdf_seas, "NAME")



## -----------------------------------------------------------------------------
plot(seas_zonal, legend = FALSE)


## -----------------------------------------------------------------------------
# If we don't do this it can't be passed around
# across the processes that in_parallel() will spawn
seas_zonal_wrapped <- terra::wrap(seas_zonal)


## -----------------------------------------------------------------------------
#' Process raster zonally
#'
#' Use terra to process the NCDF files
#'
#' @param fnames Vector of filenames
#' @param wrapped_seas wrapped object containing rasterized seas data
#'
#' @returns
#' @export
#'
#' @examples
process_raster_zonal <- function(fnames, wrapped_seas = seas_zonal_wrapped) {
  d <- terra::rast(fnames)
  wts <- terra::cellSize(d, unit = "km") # For scaling

  layer_varnames <- terra::varnames(d) # vector of layers
  date_seq <- rep(terra::time(d)) # vector of dates

  # New colnames for use post zonal calculation below
  new_colnames <- c("sea", paste(layer_varnames, date_seq, sep = "_"))

  # Better colnames
  tdf_seas <- d |>
    terra::rotate() |> # Convert 0 to 360 lon to -180 to +180 lon
    terra::zonal(terra::unwrap(wrapped_seas), mean, na.rm = TRUE)
  colnames(tdf_seas) <- new_colnames

  # Reshape to long
  tdf_seas |>
    tidyr::pivot_longer(
      -sea,
      names_to = c("measure", "date"),
      values_to = "value",
      names_pattern = "(.*)_(.*)"
    ) |>
    tidyr::pivot_wider(names_from = measure, values_from = value)
}



## -----------------------------------------------------------------------------
process_raster_zonal(all_fnames[10000])


## -----------------------------------------------------------------------------
process_raster_zonal(chunked_fnames[[1]])


## -----------------------------------------------------------------------------
tictoc::tic("Big op")
seameans_df <- chunked_fnames |>
  map(in_parallel(
    \(x) process_raster_zonal(x),
    process_raster_zonal = process_raster_zonal,
    seas_zonal_wrapped = seas_zonal_wrapped
  )) |>
  list_rbind() |>
  mutate(
    date = ymd(date),
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date),
    yrday = lubridate::yday(date),
    season = season(date)
  )
tictoc::toc()



## -----------------------------------------------------------------------------
seameans_df

