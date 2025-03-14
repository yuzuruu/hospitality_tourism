#########################################################################
# Diurnal variation of moving speed by wheelchair tourist
# by Yuzuru Utsunomiya
# First: 20th. February 2025
# Revised:
#
#
#
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
  tidyr::complete(time = tidyr::full_seq(time, 1), mode, occasion, fill = list(lat = NA, lon = NA, speed = NA)) |> 
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
  dplyr::mutate(across(where(is.character), factor)) |> 
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
file_list <- fs::dir_ls("temples_log", glob = "*.csv")
detected_objects <- 
  vroom::vroom(
    file_list, 
    id = 'filename',
    col_select = c(timestamp_ms, class_name, confidence, frame_number)
    ) |> 
  dplyr::mutate(
    # Explanation:
    # ^[^_]+_ → Matches everything up to and including the first underscore.
    # [^_]+_ → Matches everything up to and including the second underscore.
    # ([^_]+) → Captures the string between the second and third underscores.
    # _ → Ensures the match stops at the third underscore.
    # str_match() returns a matrix, and [,2] extracts only the captured group.
    mode = stringr::str_match(filename, "^[^_]+_[^_]+_([^_]+)_")[,2],
    # Explanation:
    #   (?:[^_]*_){4} → Matches and skips the first four underscores.
    # ([^_]*) → Captures everything up to the next underscore (the 5th underscore).
    # str_match() returns a matrix, and [,2] extracts the captured group.
    occasion = stringr::str_match(filename, "^(?:[^_]*_){4}([^_]*)")[,2]
  ) |> 
  dplyr::select(-filename) |> 
  dplyr::mutate(
    frame_number = factor(frame_number) #,
    # # NOTE
    # # adjust track number in accordance with target mode and occasion 
    # time_milliseconds = format(
    #   lubridate::milliseconds(timestamp_ms) + lubridate::ymd_hms(temples_feb_2025_speed |> _$time[1]), 
    #   format = "%Y-%m-%d %H:%M:%OS"
    #   )
  ) |> 
  # 
  # dplyr::mutate(
  #   # transform the milliseconds data into dttm-formatted data
  #   # Somehow the previous treatment convert milliseconds variable as character
  #   # and we need to transform that.
  #   time_milliseconds = lubridate::ymd_hms(time_milliseconds),
  #   # round the milliseconds variable by second
  #   # This is a key for joining this dataset into the GPS log data 
  #   time_ymd_hms = lubridate::round_date(time_milliseconds, unit = "second")
  # ) |> 
  # # transform character-formatted variables into factor
  dplyr::mutate(across(where(is.character), factor)) |> 
  # tibble!
  dplyr::tibble()
# 
temples_feb_2025_speed_key <- 
  temples_feb_2025_speed |> 
  group_by(mode, occasion) |> 
  slice_head(n=1) |> 
  ungroup() |> 
  select(mode, occasion, time) |> 
  data.table::setnames(c("mode","occasion","time_start"))
# 
object_time_millisec <- 
  detected_objects |> 
  left_join(temples_feb_2025_speed_key, by = join_by(mode, occasion)) |> 
  mutate(
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
# 
object_time <- 
  # object_time_millisec |> 
  # select(time, class_name, time_millisec) |> 
  # # dplyr::left_join(temples_feb_2025_speed, by = join_by(time, mode, occasion)) |> 
  # dplyr::left_join(temples_feb_2025_speed, by = join_by(time)) |> 
  temples_feb_2025_speed |> 
  # dplyr::left_join(temples_feb_2025_speed, by = join_by(time, mode, occasion)) |>
  dplyr::left_join(object_time_millisec |>　select(time, class_name, time_millisec), by = join_by(time)) |>
  dplyr::select(id_all, mode, occasion, class_name, time, time_millisec, standard_time, lat, lon, speed) |> 
  dplyr::arrange(id_all) |> 
  dplyr::mutate(
    time_millisec = lubridate::ymd_hms(time_millisec)
  ) |> 
  dplyr::mutate(
    across(where(is.character), factor)
  )
# 

# # for inspection
# object_time_second |> filter(mode == "walk" & occasion  == "morning" & class_name == "car")
# object_time_second |> filter(mode == "walk" & occasion  == "morning" & class_name == "motorcycle")
# object_time_second |> filter(mode == "walk" & occasion  == "morning" & class_name == "person")

object_time_second <- 
  object_time |> 
  filter(class_name %in% c("car", "motorcycle", "person")) |>
  droplevels() |> 
  group_by(mode, occasion, class_name, time = lubridate::floor_date(time, unit = "1 second")) |>
  summarise(
    N = n(),
    Mean_speed = mean(speed)
  ) |>
  ungroup() |> 
  group_by(mode, occasion) |>
  # HERE!!
  tidyr::complete(time = tidyr::full_seq(time, 1), class_name, fill = list(N = NA, Mean_speed = NA))|>
  mutate(
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
  mutate(
    # Compute the gap between utc and difference.
    # We use the standard time to compare speed with variety of backgrounds with each other.
    standard_time = (time - seconds(difference)) |> hms::as_hms(),
    occasion = factor(occasion, levels = c("morning","afternoon","evening"))
    ) |> 
  ungroup()
# save the results
readr::write_rds(object_time_second, "object_time_second.rds")

# ------ figure.and.table -----
# table 1
# line plot of speed over time
# read data
object_time_second <- readr::read_rds("object_time_second.rds")
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


ggsave("line_n_object_by_occasion_classname.pdf", plot = line_n_object_by_occasion_classname, width = 240, height = 160, units = "mm")
ggsave("line_speed.pdf", plot = line_speed, width = 240, height = 160, units = "mm")




