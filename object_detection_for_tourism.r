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
plan(multisession, workers = 16)
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


# ----- state.space.method -----
# read data
object_time_second <- readr::read_rds("object_time_second.rds")
# make a special dataset for stan computing
object_time_second_stan <- 
  object_time_second |> 
  # Here, as a representative object, we chose person.
  # When some others might be chosen, they should be added.
  dplyr::filter(class_name == "person") |> 
  group_by(mode, occasion) |> 
  dplyr::mutate(
    standard_time_order = order(standard_time)
  ) |> 
  ungroup() |> 
  dplyr::select(-time, -standard_time, -difference) |> 
  # complete missing obsservations
  tidyr::complete(
    mode, occasion,
    standard_time_order,
    fill = list(lat = NA, lon = NA, Mean_speed = NA, N = NA)
  ) |> 
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
  )
# split the variables for data list.
# for stan, it is better to provide data as list.
# total length of observations (4837)
T <- length(levels(factor(object_time_second_stan$standard_time_order)))
# N. of mode type (walk | wheelchair, 2)
number_mode <- length(levels(factor(object_time_second_stan$mode))) 
# N. of occasion type (morning | afternoon | evening, 3)
number_occasion <- length(levels(factor(object_time_second_stan$occasion))) 
# N. of observed observation in the dataset (N. of row, 18791)
N_obs <- length(which(!is.na(object_time_second_stan$Mean_speed)))
# (N. of row, 18791)
obs_time <- object_time_second_stan |> drop_na(Mean_speed) |> select(standard_time_order) |> _$standard_time_order |> as.numeric(as.character())
# (N. of row, 18791)
obs_mode <- object_time_second_stan |> drop_na(Mean_speed) |> select(mode_id) |> _$mode_id |> as.numeric(as.character())
# (N. of row, 18791)
obs_occasion <- object_time_second_stan |> drop_na(Mean_speed) |> select(occasion_id) |> _$occasion_id |> as.numeric(as.character())
# (N. of row, 18791)
speed_obs <- purrr::discard(object_time_second_stan$Mean_speed, is.na)
# (N. of row, 18791)
Z_obs <- object_time_second_stan |> filter(!is.na(Mean_speed)) |> select(N) |>  _$N |> as.numeric(as.character())
# make a data list
data <- list(
  T = T,
  number_mode = number_mode,
  number_occasion = number_occasion,
  N_obs = N_obs,
  obs_time = obs_time,
  obs_mode = obs_mode,
  obs_occasion = obs_occasion,
  speed_obs = speed_obs,
  Z_obs = Z_obs,
  K_max = 10  # Maximum number of lags
)
# computation
# Here is a trial part to calibrate parameter and codes.
# For confirmation, instead, we use purrr::map() to compute at a time.
# Use CPU as many CPU cores as possible
options(mc.cores = parallel::detectCores())
# compile the model
stanmodel <- cmdstanr::cmdstan_model("moving_speed_for_tourism_model_01.stan")
# Model executable is up to date!
fit <-
  stanmodel$sample(
    data = data,     # data
    seed = 123,          # random seed
    chains = 4,          # N. of chains
    refresh = 1000,      # span displaying computation results
    iter_warmup = 10000,  # burn-in period
    iter_sampling = 10000 # sampling period
  )
# save
fit$save_object(file = "fit_moving_speed_for_tourism_model_01.rds")

# ----- model.summary -----
# Load your CmdStanR fit object to compute 95% credible intervals and other summary statistics
fit_moving_speed_for_tourism_model_01 <- readr::read_rds("fit_moving_speed_for_tourism_model_01.rds")
# fit_results_022 <- readr::read_rds("fit_results_022.rds")
# 
fit_moving_speed_for_tourism_model_01_summary <-
  fit_moving_speed_for_tourism_model_01 |>
  (\(.) .$draws())() |>  # Extract draws in a separate line
  posterior::as_draws_df() |>
  posterior::summarise_draws(
    mean, sd, median, ~quantile(.x, probs = c(0.025, 0.975)), rhat, ess_bulk, ess_tail
  ) 
# save the summary table
readr::write_excel_csv(
  fit_moving_speed_for_tourism_model_01_summary, 
  "fit_moving_speed_for_tourism_model_01_summary.csv"
)
# waic
loo::loo(fit_moving_speed_for_tourism_model_01$draws())

