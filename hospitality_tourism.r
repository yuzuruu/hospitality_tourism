#########################################################
# hospitality tourism
# Original: 12th. January 2023
# Revise: 29th. March 2024
# Yuzuru Utsunomiya, Ph. D.
# (Faculty of Economics, Nagasaki University)
#########################################################
#
# ---- read.library ----
library(tidyverse)
library(khroma)
library(sf)
library(leaflet)
library(viridis)
library(gtsummary)
library(cmdstanr)
library(furrr)
# set status to use CPUs
# Usually, we use the CPUs in maximum status. Depending on environment,
# adjust the N. of cores 
future::plan(
  multisession, 
  workers = 16
  )
# 
# ---- read.data ----
# read.data
# We uploaded the data onto the following page.
# https://github.com/yuzuruu/hospitality_tourism
walk_wheelchair <- 
  readxl::read_excel(
    "walk_wheelchair_speed.xlsx",
    sheet = "Combined"
  ) %>% 
  dplyr::mutate(
    round = dplyr::case_when(
      round == "1" ~ "first",
      round == "2" ~ "second",
      round == "3"  ~ "third",
      TRUE  ~ "NA"
    )
  )
# 
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
    # We use the standard time to compare speed with variety of backgrounds with each other. 
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
# 
# # ----- line.plot -----
# line_speed_walk_wheelchair <- 
#   walk_wheelchair_standard_time %>% 
#   dplyr::filter(speed < 10) %>%
#   ggplot2::ggplot(
#     aes(
#       x = standard_time,
#       y = speed
#     )
#   ) +
#   # add lines
#   geom_line(aes(color = round)) +
#   # add points
#   geom_point(aes(color = round)) +
#   # apply a color paletter by Prof. Okabe and Prof. Ito
#   scale_color_okabeito() +
#   # add names of xy axes
#   labs(
#     x = "Time (Unit: 1 seconds)", 
#     y = "speed (log Trans., Unit: km/h)",
#     caption = "by Yuzuru Utsunomiya, Ph. D."
#   ) +
#   # conventional theme named "theme=classic"
#   theme_classic() + 
#   # place legends under the x axis
#   theme(
#     legend.position = "bottom",
#     strip.background = element_blank()
#   ) +
#   facet_wrap(~ course + mode, ncol = 2, scales = "free_x")
# line_speed_walk_wheelchair
# # 
# # save
# ggsave(
#   # file name
#   "line_speed_walk_wheelchair_combined.pdf",
#   # target object
#   plot = line_speed_walk_wheelchair,
#   height = 500,
#   width = 500,
#   units = "mm"
# )
# 
# ----- leaflet.map -----
# draw leaflet maps to confirm relationship between speed and place
# In detail of use of the leaflet, please refer to following.
# https://stackoverflow.com/questions/52108978/cant-set-color-parameter-in-addcirclemarkers
# 
# make color palette 
# The color indicates deg. of speed. In general, darker colors indicate 
# lower speed, and brighter colors indicate faster speed.
domain <- 
  range(
    walk_wheelchair_standard_time$speed, 
    na.rm = TRUE
    )
pal <- 
  leaflet::colorNumeric(
    palette = viridis(100), 
    domain = 1:5
    )
# draw
# walk
leaflet_map_walk <- 
  walk_wheelchair_standard_time %>% 
  drop_na(speed) %>% 
  dplyr::filter(mode == "walk" & round == "first") %>%
  leaflet::leaflet() %>% 
  # addTiles() %>% 
  addProviderTiles('Esri.WorldImagery') %>% 
  addCircleMarkers(lng =~ longitude, lat =~ latitude, color =~ pal(speed), radius = 1) %>% 
  addLegend(position = "bottomright", pal = pal, values =~ speed, title = "Posiion and speed </br> (walk, Unit: km/h)")
leaflet_map_walk
# wheelchair
leaflet_map_wheelchair <- 
  walk_wheelchair_standard_time %>% 
  drop_na(speed) %>% 
  dplyr::filter(mode == "wheelchair" & round == "first") %>%
  leaflet::leaflet() %>% 
  # addTiles() %>% 
  addProviderTiles('Esri.WorldImagery') %>% 
  addCircleMarkers(lng =~ longitude, lat =~ latitude, color =~ pal(speed), radius = 1) %>% 
  addLegend(position = "bottomright", pal = pal, values =~ speed, title = "Posiion and speed </br> (wheelchair, Unit: km/h)")
leaflet_map_wheelchair
# 
# ----- table.one -----
# make a table one
gtsummary::theme_gtsummary_mean_sd()
walk_wheelchair_tableone <- 
  walk_wheelchair_standard_time %>% 
  tidyr::drop_na(speed) %>% 
  dplyr::select(mode, course, round, city, speed) %>% 
  gtsummary::tbl_strata(
    strata = city, 
    .tbl_fun =
      ~ .x %>% 
      gtsummary::tbl_summary(
        by = mode,
        type = list(
          course ~ "categorical", 
          round ~ "categorical", 
          mode ~ "categorical"
        ),
        include = c(course, round, speed)
        # label = list(
        #   HICOV = "Any health insurance",
        #   ESRG = "Employment",
        #   EXPANSION = "Expansion"
      )
  )
# save the table
walk_wheelchair_tableone %>% gtsummary::as_tibble() %>% writexl::write_xlsx(., "walk_wheelchair_tableone.xlsx")
# 
# ------ change.point.detection -----
# make a data set
standard_time_filter <- 
  walk_wheelchair_standard_time %>% 
  group_by(mode, course, round) %>% 
  nest() %>% 
  dplyr::left_join(
    walk_wheelchair %>% group_by(mode, course, round) %>% nest() %>% dplyr::mutate(last_time_utc = purrr::map_dbl(data, ~ dplyr::last(.$utc)) %>% lubridate::as_datetime()) %>% dplyr::select(mode, course, round, last_time_utc) %>% unnest() %>% ungroup(), 
    join_by(mode==mode, course==course, round==round)
  ) %>% 
  dplyr::mutate(
    last_time_standard_time = purrr::map(
      data, 
      ~ 
        dplyr::filter(., utc == last_time_utc)
    ),
    last_time_standard_time_filter = purrr::map_df(
      last_time_standard_time,
      ~ 
        dplyr::select(., standard_time)
    )
  ) %>% 
  dplyr::select(-data, -last_time_standard_time) %>% 
  unnest() %>% 
  ungroup() %>% 
  data.table::setnames(c("mode","course","round","last_time_utc","last_time_standard"))
# 
walk_wheelchair_analysis <- 
  walk_wheelchair_standard_time %>% 
  group_by(mode, course, round) %>% 
  nest() %>% 
  dplyr::left_join(
    standard_time_filter,
    join_by(mode==mode, course==course, round==round)
  ) %>% 
  dplyr::mutate(
    data = purrr::map(data, ~ dplyr::filter(., standard_time < last_time_standard))
  ) %>% 
  unnest() %>% 
  ungroup() 
# # complie the stan code
# # NOTE
# # This process needs long computation period. 
# # Comment out when not in use.
# model <- cmdstanr::cmdstan_model("hospitality_tourism.stan", compile = FALSE)
# # model$format(canonicalize = list("deprecations"), overwrite_file = TRUE)
# model$compile()
# 
# walk_wheelchair_analysis_model_01 <- 
#   walk_wheelchair_analysis %>%
#   # dplyr::filter(course == "Ang_kaew") %>% 
#   group_by(mode, course, round) %>% 
#   nest() %>% 
#   dplyr::mutate(
#     fit = furrr::future_map(
#       data,
#       function(data){
#         fit_01 <- model$sample(
#           data = list(
#             y = na.omit(data$speed),
#             TT = length(data$speed), 
#             N = 1, 
#             n_obs = data$speed %>% na.omit(.) %>% length(.), 
#             col_index_obs = which(!is.na(data$speed), arr.ind = TRUE)
#           ), 
#           seed = 123,
#           chains = 4,
#           iter_warmup = 4000,
#           iter_sampling = 4000,
#           threads_per_chain = 4,
#           parallel_chains = 4,
#           step_size = 10,
#           refresh = 100
#         )
#         fit_01$summary(
#           variables = NULL,
#           posterior::default_summary_measures()[1:4],
#           quantiles = ~ posterior::quantile2(., probs = c(0.025, 0.975)),
#           posterior::default_convergence_measures()
#           )
#         }
#       )
#     )
# # save
# readr::write_rds(
#   walk_wheelchair_analysis_model_01,
#   "walk_wheelchair_analysis_model_01.rds"
# )
# 
# ----- line.with.estimated.curve -----
# read the results of estimation
walk_wheelchair_analysis_model_01 <- 
  readr::read_rds(
    "walk_wheelchair_analysis_model_01.rds"
  )
# draw a line plot
walk_wheelchair_analysis_model_01_line <- 
  walk_wheelchair_analysis_model_01 %>% 
  dplyr::mutate(
    line_plot = furrr::future_map(
      fit,
      ~
        dplyr::filter(., stringr::str_detect(.$variable, "yhat"))%>%
        bind_cols(data) %>% 
        ggplot2::ggplot() +
        geom_point(aes(x = standard_time, y = speed), color = "skyblue") +
        geom_line(aes(x = standard_time, y = mean)) +
        geom_ribbon(aes(x = standard_time, ymin = q2.5, ymax = q97.5), alpha = 0.2) +
        labs(
          x = "Time (Unit: second)",
          y = "speed (Unit: km/h)",
          title = paste(course, mode, sep = " "),
          subtitle = round
        ) +
        theme_classic() + 
        theme(
          legend.position = "none"
        )
    )
  )
# save the results
pdf("walk_wheelchair_analysis_model_01_line.pdf")
walk_wheelchair_analysis_model_01_line$line_plot
dev.off()
# 
# plot transition of SD over time
walk_wheelchair_analysis_model_01_sd_line <- 
  walk_wheelchair_analysis_model_01 %>% 
  dplyr::mutate(
    line_plot = furrr::future_map(
      fit,
      ~
        dplyr::filter(., stringr::str_detect(.$variable, "yhat"))%>%
        bind_cols(data) %>% 
        ggplot2::ggplot() +
        geom_line(aes(x = standard_time, y = sd)) +
        labs(
          x = "Time (Unit: second)",
          y = "SD of estimated speed",
          title = paste(course, mode, sep = " "),
          subtitle = round
        ) +
        ylim(0, 1) +
        theme_classic() + 
        theme(
          legend.position = "none"
        )
    )
  )
# plot
pdf("walk_wheelchair_analysis_model_01_line_sd.pdf")
walk_wheelchair_analysis_model_01_sd_line$line_plot
dev.off()
