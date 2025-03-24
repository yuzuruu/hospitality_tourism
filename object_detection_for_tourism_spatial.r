library(dplyr)
library(readr)
library(sf)
library(cmdstanr)

# STEP 1: Load the raw data
df <- read_csv("object_time_second_stan.csv")
df$id <- seq_len(nrow(df))  # assign unique ID for merging

# STEP 2: Extract rows with lat/lon to compute spatial coordinates
df_with_coords <- df %>%
  filter(!is.na(lat), !is.na(lon))

# Convert lat/lon to x/y using EPSG:3857 (meters)
points_sf <- st_as_sf(df_with_coords, coords = c("lon", "lat"), crs = 4326)
points_proj <- st_transform(points_sf, crs = 3857)
coords <- st_coordinates(points_proj)

# Add x/y back to coordinate-ready rows
df_with_coords$x <- coords[, 1]
df_with_coords$y <- coords[, 2]

# STEP 3: Merge x/y back to full dataset
df_full <- df %>%
  left_join(df_with_coords %>% select(id, x, y), by = "id")

# STEP 4: Standardize predictors (even with NAs)
scale_column <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  return((x - m) / s)
}

df_full <- df_full %>%
  mutate(
    x_std = scale_column(x),
    y_std = scale_column(y),
    N_car_std = scale_column(N_car),
    N_person_std = scale_column(N_person),
    N_motorcycle_std = scale_column(N_motorcycle)
  )

# STEP 5: REMOVE rows with NA in x or y (Stan can't handle NA in predictors)
# df_model <- df_full %>%
#   filter(!is.na(x_std), !is.na(y_std))

# STEP 6: Identify observed and missing speed rows
obs_idx_speed <- which(!is.na(df_full$Mean_speed))
miss_idx_speed <- which(is.na(df_full$Mean_speed))
obs_idx_lon <- which(!is.na(df_full$x_std))
miss_idx_lon <- which(is.na(df_full$x_std))
obs_idx_lat <- which(!is.na(df_full$y_std))
miss_idx_lat <- which(is.na(df_full$y_std))

# STEP 7: Build stan_data for your Stan model
stan_data <- list(
  # T = nrow(df_full),
  T = length(levels(factor(df_full$standard_time_order))),
  # speed
  N_obs_speed = length(obs_idx_speed),
  N_miss_speed = length(miss_idx_speed),
  obs_idx_speed = obs_idx_speed,
  miss_idx_speed = miss_idx_speed,
  speed_obs = df_full$Mean_speed[obs_idx_speed],
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
  # barriers and factors
  N_car = df_full$N_car_std,
  N_person = df_full$N_person_std,
  N_motorcycle = df_full$N_motorcycle_std,
  mode_id = df_full$mode_id,
  occasion_id = df_full$occasion_id,
  J_mode = max(df_full$mode_id, na.rm = TRUE),
  J_occasion = max(df_full$occasion_id, na.rm = TRUE)
)

# 
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
  J_occasion = max(df_full$occasion_id, na.rm = TRUE)
)

# Final checks
stopifnot(length(stan_data$speed_obs) == stan_data$N_obs)
stopifnot(all(stan_data$obs_idx <= T))
stopifnot(all(stan_data$miss_idx <= T))
stopifnot(all(stan_data$obs_idx_lon <= T))
stopifnot(all(stan_data$obs_idx_lat <= T))


str(stan_data)
# readr::write_excel_csv(df_full, "df_full.csv")

# ----- models -----
# basic model
# done
# stan_model <- cmdstan_model("moving_speed_for_tourism_model_03_spatial.stan")
# 
stan_model <- cmdstan_model("moving_speed_for_tourism_model_03_spatial_latentstate.stan")
# 
stan_model <- cmdstan_model("moving_speed_for_tourism_model_03_spatial_latentstate_overtime.stan")  
# 
stan_model <- cmdstan_model("moving_speed_for_tourism_model_03_spatial_interaction.stan")  
# 
stan_model <- cmdstan_model("moving_speed_for_tourism_model_03_spatial_interaction_overtime.stan")  



# Run the Stan model
fit <- 
  stan_model$sample(
    data = stan_data,
    iter_sampling = 1000,
    iter_warmup = 500,
    chains = 4,
    parallel_chains = 4,
    adapt_delta = 0.9
  )
# save
fit$save_object(file = "fit_moving_speed_for_tourism_model_03_spatial_latentstate.rds")
# 
fit_moving_speed_for_tourism_model_03_spatial_latentstate <- readr::read_rds("fit_moving_speed_for_tourism_model_03_spatial_latentstate.rds")
# make summary table
# model 3 spatial
fit_moving_speed_for_tourism_model_03_spatial_latentstate_summary <-
  fit_moving_speed_for_tourism_model_03_spatial_latentstate |>
  (\(.) .$draws())() |>  # Extract draws in a separate line
  posterior::as_draws_df() |>
  posterior::summarise_draws(
    mean, sd, median, ~quantile(.x, probs = c(0.025, 0.975)), rhat, ess_bulk, ess_tail
  )
# model 3 spatial
readr::write_excel_csv(
  fit_moving_speed_for_tourism_model_03_spatial_latentstate_summary, 
  "fit_moving_speed_for_tourism_model_03_spatial_latentstate_summary.csv"
)





