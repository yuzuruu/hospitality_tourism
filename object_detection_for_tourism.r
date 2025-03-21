#########################################################################
# Diurnal variation of moving speed by wheelchair tourist
# by Yuzuru Utsunomiya
# First: 20th. February 2025
# Revised: 16th. March 2025
#########################################################################
#
# Note
# Computation processes are substituted by python codes partly.
# For revising the part, refer to the python code.
# location
# .py
# 
# ----- read.library -----
library(tidyverse)
library(khroma)
library(gtsummary)
library(cmdstanr)
library(posterior)
# magic word
options(digits.secs = 5)
# plan(multisession, workers = 16)
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
# 4. 
# 5. 
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

# ------ figure.and.table -----
# read data
object_time_second <- readr::read_rds("object_time_second.rds")
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
# line plot of speed over time
# N. of detected items over time
line_n_object_by_occasion_classname <- 
  object_time_second |> 
  ggplot(aes(x=standard_time, y = log(N+0.5), color = class_name)) + 
  geom_line() + 
  labs(
    x = "Elapsed time (Unit:Sec.)",
    y = "N. of detected itmes (log Trans.)",
    color = "Objects"
  ) + 
  scale_color_light(reverse = FALSE) +
  scale_x_time(labels = \(x) format(as_datetime(x, tz = "UTC"), "%H:%M")) +
  facet_wrap(~ occasion + mode, scale = "free", dir = "v", ncol = 3) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
# speed
line_speed <- 
  temples_feb_2025_speed |> 
  dplyr::mutate(occasion = factor(occasion, levels = c("morning","afternoon","evening"))) |> 
  ggplot(aes(x = standard_time, y = speed, color = mode)) + 
  geom_line() + 
  labs(
    x = "Elapsed time (Unit: km/s)",
    y = "Speed (Unit: km/h)",
    color = "Mode"
  ) + 
  scale_color_okabeito(reverse = FALSE) +
  scale_x_time(labels = \(x) format(as_datetime(x, tz = "UTC"), "%H:%M")) +
  facet_wrap(~ occasion + mode, scale = "free", dir = "v", ncol = 3) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  )
# save
ggsave("line_n_object_by_occasion_classname.pdf", plot = line_n_object_by_occasion_classname, width = 240, height = 160, units = "mm")
ggsave("line_speed.pdf", plot = line_speed, width = 240, height = 160, units = "mm")


# ----- state.space.method.with.persons.only -----
# # read data
# object_time_second <- readr::read_rds("object_time_second.rds")
# # make a special dataset for stan computing
# object_time_second_stan <- 
#   object_time_second |> 
#   # Here, as a representative object, we chose person.
#   # When some others might be chosen, they should be added.
#   dplyr::filter(class_name == "person") |> 
#   group_by(mode, occasion) |> 
#   dplyr::mutate(
#     standard_time_order = order(standard_time)
#   ) |> 
#   ungroup() |> 
#   dplyr::select(-time, -standard_time, -difference) |> 
#   # complete missing obsservations
#   tidyr::complete(
#     mode, occasion,
#     standard_time_order,
#     fill = list(lat = NA, lon = NA, Mean_speed = NA, N = 0)
#   ) |> 
#   # add ID number to the dataset
#   {\(.) dplyr::mutate(., id=1:nrow(x=.))}() |> 
#   # replace name into number. The stan cannot understand any data other than numbers.
#   dplyr::mutate(
#     mode_id = dplyr::case_when(
#       mode == "walk" ~ "1",
#       mode == "wheelchair" ~ "2",
#       TRUE ~ "hoge"
#     ),
#     occasion_id = dplyr::case_when(
#       occasion == "morning" ~ "1",
#       occasion == "afternoon" ~ "2",
#       occasion == "evening" ~ "3",
#       TRUE ~ "hoge"
#     )
#   )
# # split the variables for data list.
# # for stan, it is better to provide data as list.
# # total length of observations (4837)
# T <- length(levels(factor(object_time_second_stan$standard_time_order)))
# # N. of mode type (walk | wheelchair, 2)
# number_mode <- length(levels(factor(object_time_second_stan$mode))) 
# # N. of occasion type (morning | afternoon | evening, 3)
# number_occasion <- length(levels(factor(object_time_second_stan$occasion))) 
# # N. of observed observation in the dataset (N. of row, 18791)
# N_obs <- length(which(!is.na(object_time_second_stan$Mean_speed)))
# # (N. of row, 18791)
# obs_time <- object_time_second_stan |> drop_na(Mean_speed) |> select(standard_time_order) |> _$standard_time_order |> as.numeric(as.character())
# # (N. of row, 18791)
# obs_mode <- object_time_second_stan |> drop_na(Mean_speed) |> select(mode_id) |> _$mode_id |> as.numeric(as.character())
# # (N. of row, 18791)
# obs_occasion <- object_time_second_stan |> drop_na(Mean_speed) |> select(occasion_id) |> _$occasion_id |> as.numeric(as.character())
# # (N. of row, 18791)
# speed_obs <- purrr::discard(object_time_second_stan$Mean_speed, is.na)
# # (N. of row, 18791)
# N_barriers <- object_time_second_stan |> filter(!is.na(Mean_speed)) |> select(N) |>  _$N |> as.numeric(as.character())
# # make a data list
# data <- list(
#   T = T,
#   number_mode = number_mode,
#   number_occasion = number_occasion,
#   N_obs = N_obs,
#   obs_time = obs_time,
#   obs_mode = obs_mode,
#   obs_occasion = obs_occasion,
#   speed_obs = speed_obs,
#   N_barriers = N_barriers,
#   K_max = 10  # Maximum number of lags
# )
# # computation
# # Here is a trial part to calibrate parameter and codes.
# # For confirmation, instead, we use purrr::map() to compute at a time.
# # Use CPU as many CPU cores as possible
# options(mc.cores = parallel::detectCores())
# # compile the model
# stanmodel <- cmdstanr::cmdstan_model("moving_speed_for_tourism_model_02.stan")
# # Model executable is up to date!
# fit <-
#   stanmodel$sample(
#     data = data,     # data
#     seed = 123,          # random seed
#     chains = 4,          # N. of chains
#     refresh = 500,      # span displaying computation results
#     iter_warmup = 2000,  # burn-in period
#     iter_sampling = 5000 # sampling period
#   )
# # save
# fit$save_object(file = "fit_moving_speed_for_tourism_model_02.rds")

# ----- state.space.method.with.all.barriers -----
# make a different data to avoid confusion
object_time_second <- 
  readr::read_rds("object_time_second.rds")
object_time_second_stan_temp <- 
  object_time_second |> 
  group_by(mode, occasion, class_name) |>
  dplyr::mutate(
    standard_time_order = order(standard_time) |> factor()
  ) |>
  ungroup() |>
  dplyr::select(-time, -lat, -lon) |>
  # complete missing obsservations
  tidyr::complete(
    mode, occasion, class_name,
    standard_time_order,
    fill = list(lat = NA, lon = NA, Mean_speed = NA, N = 0)
  )
# split the data by speed and N. of barriers
# We cannot deal with data including the two variables at a time.
# 1/2 for the N. of barriers
df_n <- 
  object_time_second_stan_temp %>%
  select(mode, occasion, standard_time_order, class_name, N) %>%
  pivot_wider(
    names_from = class_name,
    values_from = N,
    names_prefix = "N_",
    values_fill = 0
  )
# 2/2 for speed
df_meta <- 
  object_time_second_stan_temp %>%
  group_by(mode, occasion, standard_time_order) %>%
  summarise(
    Mean_speed = mean(Mean_speed, na.rm = TRUE),
    difference = first(difference),
    standard_time = first(standard_time),
    .groups = "drop"
  )
# join the two data above altogether
object_time_second_stan <- 
  left_join(df_meta, df_n, by = c("mode", "occasion", "standard_time_order")) |> 
  # add ID number to the dataset
  {\(.) dplyr::mutate(., id=1:nrow(x=.))}() |> 
  # replace name into number. The stan cannot understand any data other than numbers.
  dplyr::mutate(
    mode_id = dplyr::case_when(
      mode == "walk" ~ "1",
      mode == "wheelchair" ~ "2",
      TRUE ~ "hoge"
    ),
    occasion_id = dplyr::case_when(
      occasion == "morning" ~ "1",
      occasion == "afternoon" ~ "2",
      occasion == "evening" ~ "3",
      TRUE ~ "hoge"
    )
  ) |> 
  dplyr::mutate(across(where(is.character), factor))
# save the data
readr::write_excel_csv(object_time_second_stan, "object_time_second_stan_all.csv")
# read the dataset
object_time_second_stan_all <- 
  read_csv("object_time_second_stan_all.csv") |> 
  dplyr::mutate(across(where(is.character), factor))
# make data for stan
stan_data <- 
  list(
    T = length(levels(factor(object_time_second_stan$standard_time_order))),
    # N. of mode type (walk | wheelchair, 2)
    number_mode = length(levels(factor(object_time_second_stan$mode))), 
    # N. of occasion type (morning | afternoon | evening, 3)
    number_occasion = length(levels(factor(object_time_second_stan$occasion))), 
    # N. of observed observation in the dataset (N. of row, 18791)
    N_obs = length(which(!is.na(object_time_second_stan$Mean_speed))),
    # (N. of row, 18791)
    obs_time = object_time_second_stan |> drop_na(Mean_speed) |> dplyr::pull(standard_time_order) |> as.numeric(as.character()),
    # (N. of row, 18791)
    obs_mode = object_time_second_stan |> drop_na(Mean_speed) |> dplyr::pull(mode_id) |> as.numeric(as.character()),
    # (N. of row, 18791)
    obs_occasion = object_time_second_stan |> drop_na(Mean_speed) |> dplyr::pull(occasion_id) |>as.numeric(as.character()),
    # (N. of row, 18791)
    speed_obs = purrr::discard(object_time_second_stan$Mean_speed, is.na),
    # (N. of row, 18791)
    N_cars = object_time_second_stan |> filter(!is.na(Mean_speed)) |> dplyr::pull(N_car) |> as.numeric(as.character()),
    N_motorcycles = object_time_second_stan |> filter(!is.na(Mean_speed)) |> dplyr::pull(N_motorcycle) |> as.numeric(as.character()),
    N_persons = object_time_second_stan |> filter(!is.na(Mean_speed)) |> dplyr::pull(N_person) |> as.numeric(as.character())
  )
# scale the data for stan
# In terms of prior distribution use and MCMC process by stan, the stan prefer
# scaled data.
# ChatGPT noticed importance of scaling as follows:
# Why scale variables before passing to Stan?
# Stan uses Hamiltonian Monte Carlo (HMC) for sampling, and this algorithm is highly sensitive to the scale of parameters and predictors. 
# Here’s why standardizing helps:
# 1. Numerical stability & efficiency：Stan works best when all inputs (parameters, predictors, outcomes) are roughly on the same scale 
# — ideally between -2 and 2. If one predictor ranges from 0 to 300 and another from 0 to 1, the sampler has to explore vastly different regions of 
# parameter space at different scales, which slows convergence and can lead to poor mixing of chains high R-hat values divergent transitions
# 2. Prior interpretation becomes easier: Once your predictors are standardized (mean = 0, sd = 1), the regression coefficients (beta) represent the 
# change in outcome per 1 standard deviation increase in the predictor — which is often more interpretable and comparable across predictors.
# 3. Default priors behave better: If you're using weakly informative priors like normal(0, 1) or normal(0, 2), they make much more sense if the 
# predictor is scaled. If a predictor ranges from 0 to 500, normal(0, 1) becomes unrealistically tight.
scale_vec <- function(x) as.vector(scale(x))
stan_data$N_cars <- scale_vec(stan_data$N_car)
stan_data$N_motorcycles <- scale_vec(stan_data$N_motorcycle)
stan_data$N_persons <- scale_vec(stan_data$N_person)
# Check the data list
str(stan_data)
# Compile the Stan model
# Replace with your actual Stan model filename
stan_model <- cmdstan_model("moving_speed_for_tourism_model_03_01.stan")  
# Run the Stan model
fit <- 
  stan_model$sample(
    data = stan_data,
    iter_sampling = 2000,
    iter_warmup = 500,
    chains = 4,
    parallel_chains = 4,
    adapt_delta = 0.9
    )
# save
fit$save_object(file = "fit_moving_speed_for_tourism_model_03_01.rds")
# 
# ----- model.summary -----
# Load your CmdStanR fit object to compute 95% credible intervals and other summary statistics
# WARNING
# READING THE DATA SPENDS LOOOONG COMPUTATION PERIOD.
# COMMENT OUT WHEN NOT IN USE
# fit_moving_speed_for_tourism_model_01 <- readr::read_rds("fit_moving_speed_for_tourism_model_01.rds")
# stable model
# fit_moving_speed_for_tourism_model_02 <- readr::read_rds("fit_moving_speed_for_tourism_model_02.rds")
# 
fit_moving_speed_for_tourism_model_03_01 <- readr::read_rds("fit_moving_speed_for_tourism_model_03_01.rds")
# fit_results_022 <- readr::read_rds("fit_results_022.rds")
# 
# make summary table
# model 1
# fit_moving_speed_for_tourism_model_01_summary <-
#   fit_moving_speed_for_tourism_model_01 |>
#   (\(.) .$draws())() |>  # Extract draws in a separate line
#   posterior::as_draws_df() |>
#   posterior::summarise_draws(
#     mean, sd, median, ~quantile(.x, probs = c(0.025, 0.975)), rhat, ess_bulk, ess_tail
#   ) 
# # model 2
# fit_moving_speed_for_tourism_model_02_summary <-
#   fit_moving_speed_for_tourism_model_02 |>
#   (\(.) .$draws())() |>  # Extract draws in a separate line
#   posterior::as_draws_df() |>
#   posterior::summarise_draws(
#     mean, sd, median, ~quantile(.x, probs = c(0.025, 0.975)), rhat, ess_bulk, ess_tail
#   ) 
# model 3
fit_moving_speed_for_tourism_model_03_01_summary <-
  fit_moving_speed_for_tourism_model_03_01 |>
  (\(.) .$draws())() |>  # Extract draws in a separate line
  posterior::as_draws_df() |>
  posterior::summarise_draws(
    mean, sd, median, ~quantile(.x, probs = c(0.025, 0.975)), rhat, ess_bulk, ess_tail
  ) 

# # save the summary table
# # model 1
# readr::write_excel_csv(
#   fit_moving_speed_for_tourism_model_01_summary, 
#   "fit_moving_speed_for_tourism_model_01_summary.csv"
# )
# # model 2
# readr::write_excel_csv(
#   fit_moving_speed_for_tourism_model_02_summary, 
#   "fit_moving_speed_for_tourism_model_02_summary.csv"
# )
# model 3
readr::write_excel_csv(
  fit_moving_speed_for_tourism_model_03_01_summary, 
  "fit_moving_speed_for_tourism_model_03_01_summary.csv"
)
# waic
loo::loo(fit_moving_speed_for_tourism_model_01$draws())
loo::loo(fit_moving_speed_for_tourism_model_02$draws())
loo::loo(fit_moving_speed_for_tourism_model_03$draws())
# 
# ----- draw.state.space.results -----
# x in model 2
fit_model_02_obs_x <- 
  fit_moving_speed_for_tourism_model_02_summary |> 
  dplyr::mutate(variable = factor(variable)) |> 
  dplyr::filter(stringr::str_detect(variable, "^x\\[")) |> 
  dplyr::mutate(
    standard_time_order = as.numeric(str_extract(variable, "(?<=\\[)\\d+(?=\\])"))
  ) |> 
  dplyr::select(-sd, -`2.5%`, -`97.5%`,-variable, -median, -rhat, -ess_bulk, -ess_tail) |> 
  dplyr::mutate(
    mode = "state",
    occasion = "state"
  ) |> 
  dplyr::bind_cols(standard_time = observed_mean_speed |> dplyr::filter(mode == "walk" & occasion == "evening") |> select(standard_time)) |> 
  data.table::setnames(c("Mean_speed","standard_time_order","mode","occasion","standard_time")) |> 
  dplyr::bind_rows(observed_mean_speed)
# beta in model 2
fit_model_02_obs_beta <- 
  fit_moving_speed_for_tourism_model_02_summary |> 
  dplyr::mutate(variable = factor(variable)) |> 
  # for model 3, we need to filter differences of beta
  # (persons, cars, and motorcycles)
  dplyr::filter(stringr::str_detect(variable, "^beta_")) |> 
  dplyr::mutate(
    # Description of the following regular expression
    # (?<=\\[) preceded by left-hand-side square brank (\\[) 
    # \\d+  any digit (0, 1, ..., 9)
    # (?=\\])  followed by right-hand-side sqare brank (\\])
    standard_time_order = as.numeric(str_extract(variable, "(?<=\\[)\\d+(?=\\])"))
  ) |> 
  dplyr::select(-median,-variable, -median, -rhat, -ess_bulk, -ess_tail, -standard_time_order) |> 
  # pick up standard_time from the conditions with the longest standard_time 
  dplyr::bind_cols(standard_time = observed_mean_speed |> dplyr::filter(mode == "walk" & occasion == "evening") |> select(standard_time)) |> 
  data.table::setnames(c("beta","sd","LCI","UCI","standard_time")) |> 
  # evaluate whether the beta_ includes zero in its 95%CI or not
  dplyr::mutate(zero_between = as.integer(0 >= pmin(LCI, UCI) & 0 <= pmax(LCI, UCI)))
# draw lines 
# x
line_fit_model_01_obs_x <- 
  fit_model_01_obs_x |> 
  ggplot2::ggplot(
    aes(
      x = standard_time,
      y = Mean_speed,
      color = occasion,
      linetype = mode,
      alpha = occasion  # Add alpha inside aes()
    ) 
  ) +
  geom_line() + 
  labs(
    x = "Time (Unit: sec)",
    y = "State of the moving speed over time (x)"
  ) + 
  scale_color_manual(
    values = c(
      afternoon = "#2997E6",
      evening = "#61D04F",
      morning = "#F5C710",
      state = "black"
    )
  ) +
  scale_alpha_manual(  # Now alpha is mapped correctly
    values = c(
      afternoon = 0.2,
      evening = 0.2,
      morning = 0.5,
      state = 1.0
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
# beta
line_fit_model_02_obs_beta <- 
  fit_model_02_obs_beta |> 
  ggplot2::ggplot(
    aes(
      x = standard_time,
      y = beta
    )
  ) +
  geom_line(color = "black") +  # Line remains black
  geom_point(aes(color = "blue", alpha = factor(zero_between)), size = 3) +  # Adjust alpha
  scale_alpha_manual(
    values = c("0" = 1, "1" = 0)  # 1 = fully visible, 0 = fully transparent
  ) +
  guides(alpha = "none") +  # Remove alpha legend
  theme_classic()
