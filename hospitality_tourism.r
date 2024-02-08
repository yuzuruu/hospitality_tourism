################################
# hospitality tourism
# Original: 12th. January 2023
# Revise: 6th. February 2024
# Yuzuru Utsunomiya, Ph. D.
# (Faculty of Economics, Nagasaki University)
################################
#
# ---- read.library ----
library(tidyverse)
library(khroma)
library(sf)
# 
# ---- read.data ----
walk_wheelchair <- 
  readxl::read_excel(
    "hospitality_tourism.xlsx",
    sheet = "Nagasaki"
  ) %>% 
  dplyr::mutate(
    round = dplyr::case_when(
      round == "1" ~ "first",
      round == "2" ~ "second",
      round == "3"  ~ "third",
      TRUE  ~ "NA"
    )
  )

# obtain data for computation
walk_wheelchair_standard_time <- 
  walk_wheelchair %>% 
  # split the data by assigned group
  # This time, because of data composition, we set "mode" as a group.
  # The "mode" refers to ways of going around; wheelchair and walk.
  # As the survey will progress, we need to add other groups including......
  # 1. course 
  # 2. round (1st., 2nd., and 3rd.)
  # 3. city
  group_by(mode, course, round) %>%
  # add variables
  dplyr::mutate(
    # Compute differences between start time and a reference time
    difference = lubridate::time_length(
      lubridate::interval(
        # set a reference time deep in the past
        lubridate::ymd_hms("2022-01-01 00:00:00"), 
        # minimum utc = start time
        # utc refers to times GPS logged with locations and other information.
        min(utc)
      )
    ),
    # Compute the gap between utc and difference.
    # We use the standard time to compare velocity with variety of backgrounds with each other. 
    standard_time = (utc - seconds(difference))
  ) %>% 
  dplyr::select(-difference) %>% 
  ungroup() %>% 
  tidyr::complete(
    # Fill data into missing values.
    # To avoid malfunction, even for 5-seconds-frequency data, 
    # we fill variables every 1 second.
    standard_time = tidyr::full_seq(standard_time, period = 1),
    mode,
    course,
    round
    ) %>% 
  # convert the standard time in dttm format into one in hms format.
  dplyr::mutate(standard_time = hms::as_hms(standard_time))
# save the results in rds format
# This section is useful for huge data which is unavailable to save in csv format.
readr::write_rds(
  walk_wheelchair_standard_time,
  "walk_wheelchair_standard_time.rds"
)
# save the results in csv format
readr::write_excel_csv(
  walk_wheelchair_standard_time, 
  "walk_wheelchair_standard_time.csv"
  )

# ----- line.plot -----
line_trial_walk_wheelchair <- 
  walk_wheelchair_standard_time %>% 
  ggplot2::ggplot(
    aes(
      x = standard_time,
      y = log(velocity+0.5)
    )
  ) +
  # add lines
  geom_line(aes(color = round)) +
  # add points
  geom_point(aes(color = round)) +
  # apply a color paletter by Prof. Okabe and Prof. Ito
  scale_color_okabeito() +
  # add names of xy axes
  labs(
    x = "Time (Unit: 1 seconds)", 
    y = "Velocity (log Trans., Unit: km/h)",
    caption = "by Yuzuru Utsunomiya, Ph. D."
  ) +
  # conventional theme named "theme=classic"
  theme_classic() + 
  # place legends under the x axis
  theme(
    legend.position = "bottom",
    strip.background = element_blank()
  ) +
  facet_wrap(~ mode + course, scales = "free_x")
line_trial_walk_wheelchair
# 
# save
ggsave(
  # file name
  "line_trial_walk_wheelchair.pdf",
  # target object
  plot = line_trial_walk_wheelchair,
  height = 500,
  width = 500,
  units = "mm"
)
# END


