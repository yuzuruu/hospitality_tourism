#########################################################################
# Moving speed transition and factors affecting the speed
# by Yuzuru Utsunomiya
# First: 20th. February 2025
# Revised: 16th. March 2025
#########################################################################
#
# Note
# Computation processes are substituted by python codes partly.
# For revising the part, refer to the python code.
# 
# ----- read.library -----
library(tidyverse)
library(khroma)
library(gtsummary)
library(cmdstanr)
library(posterior)
library(sf)
library(gt)
library(knitr)
library(kableExtra)
# 
# magic word
options(digits.secs = 5)
# 
# ----- read.data -----
# speed (km/sec) by GPS 
temples_feb_2025_speed <- 
  readxl::read_excel(
    "temples_feb_2025.xlsx",
    sheet = "speed"
  ) |> 
  dplyr::mutate(
    id_all = factor(id_all),
    id_track = factor(id_track),
    track = factor(track),
    # convert character to ymd_hms. 
    # Somehow the datetime data is read as character...
    time = lubridate::ymd_hms(time)
  ) |> 
  group_by(track)  |> 
  # add variables
  dplyr::mutate(
    # replace the track number into mode
    mode = dplyr::case_when(
      (track == "1" | track == "3" | track == "5") ~ "wheelchair",
      (track == "2" | track == "4" | track == "6") ~ "walk",
      TRUE ~ "hoge"
    ),
    occasion = dplyr::case_when(
      (track == "1" | track == "2") ~ "morning",
      (track == "3" | track == "4") ~ "afternoon",
      (track == "5" | track == "6") ~ "evening",
      TRUE ~ "hoge"
    )
  ) |> 
  # complete missing observation
  # DO NOT BE CARELESS!! Even thought the data is collected by the GPS, 
  # the logging data includes missing values, which inhibit analyses normally.
  tidyr::complete(
    time = tidyr::full_seq(time, 1), mode, occasion, 
    # for the missing values, place NA
    fill = list(lat = NA, lon = NA, speed = NA)
  ) |> 
  dplyr::mutate(
    # Compute differences between start time and a reference time
    difference = lubridate::time_length(
      lubridate::interval(
        # set a reference time deep in the past
        lubridate::ymd_hms("2022-01-01 00:00:00"), 
        # minimum utc = start time
        # utc refers to times GPS logged with locations and other information.
        min(time)
      )
    ),
    # Compute the gap between utc and difference.
    # We use the standard time to compare speed with variety of backgrounds with each other. 
    standard_time = (time - seconds(difference))
  ) |>
  # Transform those which are character.
  dplyr::mutate(
    across(where(is.character), 
           factor)
  ) |> 
  dplyr::select(-difference) %>% 
  ungroup() |>  
  # group_by(track) |> 
  # tidyr::complete(
  #   # Fill data into missing values.
  #   # To avoid malfunction, even for 5-seconds-frequency data, 
  #   # we fill variables every 1 second.
  #   standard_time = tidyr::full_seq(standard_time, period = 1)
  # ) |>  
  # # convert the standard time in dttm format into one in hms format.
  dplyr::mutate(
    standard_time = hms::as_hms(standard_time)
  )
# 
# ----- detected.objects -----
# detected objects from six movies
# make a file list to read the target files
file_list <- fs::dir_ls("temples_log", glob = "*.csv")
# read the files
detected_objects <- 
  vroom::vroom(
    file_list,
    # add file names into saved csv files for convenience
    id = 'filename',
    # select necessary variables
    col_select = c(timestamp_ms, class_name, confidence, frame_number)
  ) |> 
  dplyr::mutate(
    # pick up mode names (walk / wheelchair) from target files' name
    # Explanation:
    # ^[^_]+_ → Matches everything up to and including the first underscore.
    # [^_]+_ → Matches everything up to and including the second underscore.
    # ([^_]+) → Captures the string between the second and third underscores.
    # _ → Ensures the match stops at the third underscore.
    # str_match() returns a matrix, and [,2] extracts only the captured group.
    mode = stringr::str_match(filename, "^[^_]+_[^_]+_([^_]+)_")[,2],
    # 
    # pick up mode names (walk / wheelchair) from target files' name
    # Explanation:
    #   (?:[^_]*_){4} → Matches and skips the first four underscores.
    # ([^_]*) → Captures everything up to the next underscore (the 5th underscore).
    # str_match() returns a matrix, and [,2] extracts the captured group.
    occasion = stringr::str_match(filename, "^(?:[^_]*_){4}([^_]*)")[,2]
  ) |> 
  dplyr::select(-filename) |> 
  dplyr::mutate(
    frame_number = factor(frame_number) 
  ) |> 
  dplyr::mutate(across(where(is.character), factor)) |> 
  # tibble!
  dplyr::tibble()
# 
# ----- combine.gps.objects -----
# combine GPS logging data and detected objects lists
# Procedure
# 1. Make a table of started time (temples_feb_2025_speed_key). 
# As a key, we use timestamps of the files. The detected objects files,
# however, include merely timestamps in millisecond from time started recording.
# To combine the file, using the timestamps and starting time, we need to make
# a variable indicating recording moment. 
# 2. Aggregate the N. of detected objects by object
# The timestamps in millisecond do not meet existing GPS loggind data in second.
# By aggregating the millisecond variable by second, we can merge the two data
# 3. Complete missing values
# 
# 1. make a key table
temples_feb_2025_speed_key <- 
  temples_feb_2025_speed |> 
  dplyr::group_by(mode, occasion) |> 
  # obtain the first observation
  slice_head(n = 1) |> 
  ungroup() |> 
  dplyr::select(mode, occasion, time) |> 
  data.table::setnames(c("mode","occasion","time_start"))
# merge the two dataset
object_time_millisec <- 
  detected_objects |>
  # merge!!
  dplyr::left_join(
    temples_feb_2025_speed_key, 
    by = join_by(mode, occasion
    )
  ) |> 
  dplyr::mutate(
    time_millisec = format(
      lubridate::milliseconds(timestamp_ms) + time_start, 
      format = "%Y-%m-%d %H:%M:%OS"
    ),
    time = format(
      time_millisec, 
      format = "%Y-%m-%d %H:%M:%S"
    ) |> lubridate::ymd_hms() |> lubridate::round_date("second")
  ) |> 
  dplyr::select(class_name, mode, occasion, time, time_millisec, frame_number)
# 2. aggregate
object_time <- 
  temples_feb_2025_speed |> 
  dplyr::left_join(
    object_time_millisec |>　select(time, class_name, time_millisec), 
    by = join_by(time)
  ) |>
  dplyr::select(id_all, mode, occasion, class_name, time, time_millisec, standard_time, lat, lon, speed) |> 
  # to confirm progress of here
  dplyr::arrange(id_all) |> 
  dplyr::mutate(
    time_millisec = lubridate::ymd_hms(time_millisec)
  ) |> 
  dplyr::mutate(
    across(where(is.character), factor)
  )
# 3. complete missing values
object_time_second <- 
  object_time |> 
  dplyr::filter(class_name %in% c("car", "motorcycle", "person")) |>
  # omit levels filtered
  # Otherwise, the removed levels remain.
  droplevels() |>
  dplyr::mutate(counter = as.numeric(1)) |> 
  dplyr::group_by(
    mode, occasion, class_name, 
    # to group by second
    time = lubridate::floor_date(time, unit = "1 second")
  ) |>
  dplyr::summarise(
    N = sum(counter),
    Mean_speed = mean(speed)
  ) |>
  ungroup() |> 
  dplyr::left_join(
    temples_feb_2025_speed |> dplyr::select(time, lat, lon),
    by = join_by("time")
  ) |> 
  group_by(mode, occasion) |>
  # COMPLETE!!
  # HERE!!
  tidyr::complete(
    time = tidyr::full_seq(time, 1), 
    class_name, 
    fill = list(N = NA, Mean_speed = NA, lat = NA, lon = NA)
  ) |>
  dplyr::mutate(
    difference = lubridate::time_length(
      lubridate::interval(
        # set a reference time deep in the past
        lubridate::ymd_hms("2022-01-01 00:00:00"),
        # minimum utc = start time
        # utc refers to times GPS logged with locations and other information.
        min(time)
      )
    )
  ) |> 
  dplyr::mutate(
    # Compute the gap between utc and difference.
    # We use the standard time to compare speed with variety of backgrounds with each other.
    standard_time = (time - seconds(difference)) |> hms::as_hms(),
    occasion = factor(occasion, levels = c("morning","afternoon","evening"))
  ) |> 
  ungroup()
# save the results
# well done
readr::write_rds(object_time_second, "object_time_second.rds")
# 
# ----- table.1 -----
# (Table 1)
# read data
object_time_second <- readr::read_csv("object_time_second.csv")

# table 1
object_time_tableone <- 
  object_time_second |> 
  dplyr::select(mode, occasion, class_name, N, Mean_speed) |> 
  gtsummary::tbl_strata(
    strata = mode,
    ~.x |> 
      gtsummary::tbl_summary(
        by = occasion,
        statistic = list(all_continuous() ~ "{mean} ({sd})")
      )
  ) 
# 
# ----- make.data.for.stan -----
# STEP 1: Load the raw data
df <- 
  readr::read_csv(
  "object_time_second_stan.csv"
  )
df$id <- seq_len(nrow(df))  # assign unique ID for merging
# Extract rows with lat/lon to compute spatial coordinates
# sf() does not accept NA
df_with_coords <- 
  df  |> 
  dplyr::filter(!is.na(lat), !is.na(lon))
# Convert lat/lon to x/y using EPSG:3857 (meters)
# set a normal crs 
points_sf <- 
  sf::st_as_sf(
    df_with_coords, 
    coords = c("lon", "lat"), 
    # normal crs
    crs = 4326
    )
# convert crs
points_proj <- 
  sf::st_transform(points_sf, crs = 3857)
coords <- 
  sf::st_coordinates(points_proj)
# Add x/y back to coordinate-ready rows
df_with_coords$x <- coords[, 1]
df_with_coords$y <- coords[, 2]
# Merge x/y back to full dataset
df_full <- 
  df %>%
  dplyr::left_join(
    df_with_coords  |>  dplyr::select(id, x, y), 
    by = "id"
    )
# Scale predictors (even with NAs)
# scaling function
scale_column <- 
  function(x){
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    return((x - m) / s)
    }
# scaling
df_full <- df_full %>%
  dplyr::mutate(
    x_std = scale_column(x),
    y_std = scale_column(y),
    N_car_std = scale_column(N_car),
    N_person_std = scale_column(N_person),
    N_motorcycle_std = scale_column(N_motorcycle)
  )
readr::write_excel_csv(df_full, "df_full.csv")
# Identify observed and missing speed rows
obs_idx_speed <- which(!is.na(df_full$Mean_speed))
miss_idx_speed <- which(is.na(df_full$Mean_speed))
obs_idx_lon <- which(!is.na(df_full$x_std))
miss_idx_lon <- which(is.na(df_full$x_std))
obs_idx_lat <- which(!is.na(df_full$y_std))
miss_idx_lat <- which(is.na(df_full$y_std))
# Build stan_data for your Stan model
# Step 1: Define T
T <- length(levels(factor(df_full$standard_time_order)))
# Step 2: Handle indexing
obs_idx_speed <- which(!is.na(df_full$Mean_speed))
miss_idx_speed <- which(is.na(df_full$Mean_speed))
obs_idx_lon <- which(!is.na(df_full$x_std))
miss_idx_lon <- which(is.na(df_full$x_std))
obs_idx_lat <- which(!is.na(df_full$y_std))
miss_idx_lat <- which(is.na(df_full$y_std))
# Step 3: Filter all indices to be <= T
obs_idx <- obs_idx_speed[obs_idx_speed <= T]
miss_idx <- miss_idx_speed[miss_idx_speed <= T]
obs_idx_lon <- obs_idx_lon[obs_idx_lon <= T]
miss_idx_lon <- miss_idx_lon[miss_idx_lon <= T]
obs_idx_lat <- obs_idx_lat[obs_idx_lat <= T]
miss_idx_lat <- miss_idx_lat[miss_idx_lat <= T]
# Step 4: Build stan_data
stan_data <- list(
  T = T,
  # speed
  N_obs = length(obs_idx),
  N_miss = length(miss_idx),
  obs_idx = obs_idx,
  miss_idx = miss_idx,
  speed_obs = df_full$Mean_speed[obs_idx],
  # longitude
  N_obs_lon = length(obs_idx_lon),
  N_miss_lon = length(miss_idx_lon),
  obs_idx_lon = obs_idx_lon,
  miss_idx_lon = miss_idx_lon,
  lon_obs = df_full$x_std[obs_idx_lon],
  # latitude
  N_obs_lat = length(obs_idx_lat),
  N_miss_lat = length(miss_idx_lat),
  obs_idx_lat = obs_idx_lat,
  miss_idx_lat = miss_idx_lat,
  lat_obs = df_full$y_std[obs_idx_lat],
  # predictors (must be length T)
  N_car = df_full$N_car_std[1:T],
  N_person = df_full$N_person_std[1:T],
  N_motorcycle = df_full$N_motorcycle_std[1:T],
  mode_id = as.integer(df_full$mode_id[1:T]),
  occasion_id = as.integer(df_full$occasion_id[1:T]),
  # factor levels
  J_mode = max(df_full$mode_id, na.rm = TRUE),
  J_occasion = max(df_full$occasion_id, na.rm = TRUE),
  rho_gp = 5
)
# Final checks
stopifnot(length(stan_data$speed_obs) == stan_data$N_obs)
stopifnot(all(stan_data$obs_idx <= T))
stopifnot(all(stan_data$miss_idx <= T))
stopifnot(all(stan_data$obs_idx_lon <= T))
stopifnot(all(stan_data$obs_idx_lat <= T))
str(stan_data)
# 
# ----- inference.model.with.stan -----
# compile models
# 1. basic model
# stan_model <- cmdstanr::cmdstan_model("moving_speed_spatial_base.stan")
# 2. AR(1) + interaction
# The best model at the moment
stan_model <- 
  cmdstanr::cmdstan_model(
    "moving_speed_spatial_ar1_interaction.stan"
    )
# kick the stan model
# WARNING
# HERE NEEDS LOOOONG COMPUTATION PERIOD.
# COMMENT OUT WHEN NOT IN USE.
fit <- 
  stan_model$sample(
    data = stan_data,
    # For test: 1,000
    # For finishing: 5,000
    # NOTE
    # When the N. of iter_sampling is smaller, loo() often includes bad results.
    # Compute the loo using final version.
    iter_sampling = 5000,
    # 1,000 is enough.
    iter_warmup = 1000,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    adapt_delta = 0.95
  )
# save
fit$save_object(
  file = "fit_moving_speed_spatial_ar1_interaction.rds")
# assign the saved results
fit_moving_speed_spatial_ar1_interaction <- 
  readr::read_rds(
    "fit_moving_speed_spatial_ar1_interaction.rds"
    )
# make summary table
# Extract draws and convert to draws_df
draws_df <- 
  fit_moving_speed_spatial_ar1_interaction$draws() |>
  posterior::as_draws_df()
# Summarize using correct syntax (functions passed unquoted)
fit_moving_speed_spatial_ar1_interaction_summary <- 
  summarise_draws(
    draws_df, mean, sd, median, ~ quantile(.x, probs = c(0.025, 0.975)), rhat, ess_bulk, ess_tail
  )
# save the summary table in a .csv file
readr::write_excel_csv(
  fit_moving_speed_spatial_ar1_interaction_summary, 
  "fit_moving_speed_spatial_ar1_interaction_summary.csv"
)
# calculate loo
loo_ar1_interaction <- 
  loo::loo(fit_moving_speed_spatial_ar1_interaction$draws())


