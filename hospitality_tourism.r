# hospitality tourism
# 12th. January 2023
# Yuzuru Utsunomiya


# ---- read.library ----
library(tidyverse)
library(khroma)
library(sf)

# ---- read.data ----
trial_walk_wheelchair <- 
  readxl::read_excel(
    "hospitality_tourism.xlsx",
    sheet = "trial_nu"
    ) %>% 
  # The waypoints data by GPS contains ymd with the same hms and 
  # hms with the same ymd. To merge them correctly, we once 
  # obtain subset of the variables.
  dplyr::mutate(
    # obtain ymd only
    date = lubridate::as_date(
      stringr::str_sub(
        date, 
        start = 1, 
        end = 10
        )
      ),
    # hms only
    utc = hms::as_hms(
      stringr::str_sub(
        utc, 
        start = 12, 
        end = 20
        )
      ),
    mode = factor(mode)
    ) %>% 
  # merge the ymd and hms together
  dplyr::mutate(
    utc = lubridate::ymd_hms(paste(date, utc))
  ) %>% 
  dplyr::mutate(
    utc = dplyr::case_when(
      # provide the lag second (2 this time) manually.
      # In the future, we would like to do that automatically.
      mode == "walk" ~ utc + lubridate::seconds(2), 
      TRUE ~ utc
      )
  ) %>% 
  dplyr::select(-date) 

# ---- handle.data ----
# walk
# obtain 5-seconds-lagged variables to calculate distance
trial_walk <- 
  trial_walk_wheelchair %>% 
  dplyr::filter(
    mode == "walk"
    ) %>% 
  dplyr::mutate(
    lat_5sec = dplyr::lag(latitude, 1),
    lon_5sec = dplyr::lag(longitude, 1)
  ) %>% 
  na.omit()
# select overlapped period
# original
df1 <- trial_walk %>%
  # transform the pairs of lon/at into sf
  sf::st_as_sf(
    coords = c("longitude","latitude"), 
    crs = 4326
    ) 
# 5 seconds after
df2 <- trial_walk %>%
  sf::st_as_sf(
    coords = c("lon_5sec","lat_5sec"), 
    crs = 4326
    )
# calculate speed from moving distance per 5 seconds
speed_walk <- 
  # the sf::st_distance function returns distance wih unit (m by default).
  # We do not need the unit and remove the unit using as.numeric() function.
  as.numeric(
    diag(
      # compute velocity
      # GPS records positions every 5 seconds.
      # To translate the distance into velocity, we multiplied 
      # 12 times (60 min / 5 min = 12)
      # 60 minutes
      # 1/1000
      12*60*sf::st_distance(x = df1, y = df2)/1000
      )
    )
# add the calculation results above to the original data
trial_walk_speed <- 
  trial_walk %>% 
  dplyr::bind_cols(speed = speed_walk)
#
# wheelchair
trial_wheelchair <- 
  trial_walk_wheelchair %>% 
  dplyr::filter(mode == "wheelchair") %>% 
  dplyr::mutate(
    lat_5sec = lag(latitude, 1),
    lon_5sec = lag(longitude, 1)
  ) %>% 
  na.omit()
df1 <- trial_wheelchair %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) 
df2 <- trial_wheelchair %>%
  st_as_sf(coords = c("lon_5sec","lat_5sec"), crs = 4326)
speed_wheelchair <- as.numeric(diag(12*60*st_distance(x = df1, y = df2)/1000))
trial_wheelchair_speed <- 
  trial_wheelchair %>% 
  dplyr::bind_cols(
    speed = speed_wheelchair
    )
# merge the files above
df_trial_walk_wheelchair <- 
  trial_wheelchair_speed %>% 
  bind_rows(trial_walk_speed)
# save the df_trial_walk_wheelchair as .csv for reuse
write_excel_csv(
  df_trial_walk_wheelchair, 
  "df_trial_walk_wheelchair.csv"
  )

# ---- check.overlap ----
# calculate overlap period
# GPS often records points in unnecessary periods and data contains the
# unnecessary periods. We do not need it and need to remove the data.
# To remove that, we calculate period where period of waypoints of 
# wheelchair / walk overlap.
overlap_period <- 
  df_trial_walk_wheelchair %>% 
  group_by(mode) %>% 
  summarize(
    Min. = min(utc),
    Max. = max(utc)
  )
df_trial_walk_wheelchair <- 
  df_trial_walk_wheelchair %>%
  # pick overlapped period
  dplyr::filter(
    utc > max(overlap_period$Min.) & utc < min(overlap_period$Max.)
    )
# ---- draw.line ---- 
line_trial_walk_wheelchair <- 
  df_trial_walk_wheelchair %>% 
  ggplot(
    aes(
      x = utc,
      y = speed,
      color = mode
    )
  ) + 
  geom_line() +
  geom_point() +
  scale_color_okabeito() +
  labs(x = "Time (Unit: 5 seconds)", y = "Velocity (Unit: km/h)") +
  theme_classic() + 
  theme(
    legend.position = "bottom"
  )
# save the figure
ggsave(
  "line_df_trial_walk_wheelchair.pdf",
  plot = line_trial_walk_wheelchair,
  height = 200,
  width = 200,
  units = "mm"
)

