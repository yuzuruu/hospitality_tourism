#########################################################################
# Moving speed transition and factors affecting the speed
# by Yuzuru Utsunomiya
# First: 20th. February 2025
# Revised: 5th. September 2025
#########################################################################
#
# Note
# Computation processes are substituted by python codes partly.
# For revising the part, refer to the python code.
# 
# ----- read.library.figure ----- -----
library(tidyverse)
library(khroma)
library(gtsummary)
library(cmdstanr)
library(posterior)
library(sf)
library(gt)
library(knitr)
library(kableExtra)
library(osmdata)
library(ggspatial)
library(ggrepel)
library(maptiles)
library(khroma)
library(rnaturalearth)
library(av)
# 
# ----- read.orignal.data.figure ----- 
df <- readr::read_csv(
  "object_time_second_stan.csv"
)
# 
# ----- load.osm.data ----------
# set spatial data up
# load administrative boundaries
# NOTE
# Before use, download the data from GADM
# (https://gadm.org/)
# ASEAN
asean_01 <- 
  sf::st_read("../factory_Thailand/shapefiles/gadm41_THA_0.shp") |>   
  dplyr::bind_rows(sf::st_read("../factory_Thailand/shapefiles/gadm41_VNM_0.shp")) |> 
  dplyr::bind_rows(sf::st_read("../factory_Thailand/shapefiles/gadm41_KHM_0.shp")) |> 
  dplyr::bind_rows(sf::st_read("../factory_Thailand/shapefiles/gadm41_LAO_0.shp")) |> 
  dplyr::bind_rows(sf::st_read("../factory_Thailand/shapefiles/gadm41_MMR_0.shp")) |> 
  dplyr::bind_rows(sf::st_read("../factory_Thailand/shapefiles/gadm41_MYS_0.shp")) |> 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# province
adm_01 <- 
  sf::st_read("../factory_Thailand/shapefiles/gadm41_THA_1.shp") |>   
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# district
adm_02 <- 
  sf::st_read("../factory_Thailand/shapefiles/gadm41_THA_2.shp")|> 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# subdistrict
adm_03 <- 
  sf::st_read("../factory_Thailand/shapefiles/gadm41_THA_3.shp") |>  
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# Obtain target provinces, districts, and subdistricts
# province
adm_01_target <- 
  adm_01 |>  
  dplyr::filter(
    NAME_1 %in% c("Chiang Mai")
  )  |>  
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# district
adm_02_target <- 
  adm_02 |>   
  dplyr::filter(
    NAME_2 %in% c("Muang Chiang Mai")
  ) |>  
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# sub district
adm_03_target <- 
  adm_03 |>  
  dplyr::filter(
    NAME_2 %in% c(
      "Muang Chiang Mai"
                  ) 
  ) |> 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# Not target area
adm_03_not_target <- 
  adm_03_target |>   
  dplyr::filter(
    !NAME_2 %in% c("Muang Chiang Mai") 
    ) |>  
  sf::st_union() |> 
  sf::st_union(sf::st_union(adm_02 |>  dplyr::filter(NAME_2 != "Chiang Mai")))
readr::write_rds(adm_03_not_target, "adm_03_not_target.rds")

# 
# make some layers indicating road and canal
# To make the layers, we use osmdata() library
# The library has functions to check features and tags.
# Before loading data for the layer, set them while checking them.
# In detail of the features and tags, refer to the following page.
# https://wiki.openstreetmap.org/wiki/Map_features
# features
osmdata::available_features()
# tags
osmdata::available_tags("natural")
osmdata::available_tags("water")
# Your target bbox
target_bbox <- 
  sf::st_bbox(c(
    xmin = 98.975, xmax = 98.995,
    ymin = 18.775, ymax = 18.795
  ), crs = sf::st_crs(4326))

# Download a satellite tile
satellite_tiles_chiangmai <- maptiles::get_tiles(target_bbox, provider = "OpenStreetMap", crop = TRUE, zoom = 16)

# obtain features' data using osmdata()
# street
# "Street" refers to small roads excluding motorway and major road.
streets <- 
  target_bbox |> 
  osmdata::opq() |> 
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "footway", 
      "track",
      "residential", 
      "living_street",
      "service",
      "unclassified"
    )
  ) |> 
  osmdata::osmdata_sf()
streets
# road
road <- 
  target_bbox |> 
  osmdata::opq() |> 
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway", 
      "motorway_junction",
      "motorway_link",
      "primary", 
      "primary_link",
      "secondary", 
      "secondary_link",
      "tertiary",
      "tertiary_link",
      "trunk",
      "trunk_link"
    )
  ) |> 
  osmdata::osmdata_sf()
road
# 
# 
# ----- temples.maps ----------
# (Part of Figure 1)
# 
# Map of our target area and its peripheral area
temples_multilayer_map <- 
  ggplot() +
  ggspatial::layer_spatial(satellite_tiles_chiangmai) +
  geom_sf() +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "\U00a9 OpenStreetMap contributors"
  ) +
  # scale_x_continuous(breaks = seq(100.64, 100.68, by = 0.02)) +
  coord_sf(xlim = c(98.980, 98.990), ylim = c(18.785, 18.790), expand = TRUE) +
  # annotate text and points
  # WCL
  annotate("point", fill = NA, color = "black", x = 98.98658, y = 18.78697) +
  annotate("text", color = "black", x = 98.98658, y = 18.78670, size = unit(5, "pt"), label = "WCL") +
  # WPT
  annotate("point", fill = NA, color = "black", x = 98.98790, y = 18.78781) +
  annotate("text", color = "black", x = 98.98790, y = 18.78754, size = unit(5, "pt"), label = "WPT") +
  # WPS
  annotate("point", fill = NA, color = "black", x = 98.98150, y = 18.78850) +
  annotate("text", color = "black", x = 98.98150, y = 18.78823, size = unit(5, "pt"), label = "WPS") +
  # WIK
  annotate("point", fill = NA, color = "black", x = 98.98695, y = 18.78953) +
  annotate("text", color = "black", x = 98.98633, y = 18.78953, size = unit(5, "pt"), label = "WIK") +
  # TKM
  annotate("point", fill = NA, color = "black", x = 98.98693, y = 18.79010) +
  annotate("text", color = "black", x = 98.98693, y = 18.79033, size = unit(5, "pt"), label = "TKM") +
  ggspatial::annotation_scale(
    location = "bl", 
    width_hint = 0.5
    # bar_cols = c("grey","white"),
    # text_col = "white",
    # line_col = "white"
    ) +
  ggspatial::annotation_north_arrow(
    pad_x = unit(10, "mm"), 
    pad_y = unit(10, "mm")
  ) +
  # fix boundary box
  coord_sf(xlim = c(98.980, 98.991),
           ylim = c(18.785, 18.791),
           expand = TRUE
  ) +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = NA)
  )
# save the map
ggsave(
  "temples_multilayer_map.pdf",
  plot = temples_multilayer_map,
  width = 400,
  height = 200,
  units = "mm",
  device = cairo_pdf # important!!
)
# ----- chiang.mai.map ----------
# (Part of Figure 1)
# Map of Chiang Mai province
chiang_mai_map <- 
  # adm_01 |> dplyr::filter(NAME_1 == "Chiang Mai") |> 
  ggplot() +
  geom_sf(color = "black", fill = "white") +
  # Chiang Mai province whole map
  # geom_sf(
  #   data = adm_01 |> dplyr::filter(NAME_1 %in% c("Samut Prakan", "Bangkok Metropolis", "Chachoengsao")) ,
  #   inherit.aes = FALSE,
  #   color = "black",
  #   fill = "white",
  #   size = 1,
  #   alpha = 1.0
  # ) +
  # Target districts
  geom_sf(
    data = adm_03_target,
    inherit.aes = FALSE,
    color = "black",
    fill = "grey88",
    size = 5,
    alpha = 1.0
  ) +
  # label text indicating PHREAK SA district
  geom_text_repel(
    data = adm_03_target,
    size = 5,
    max.overlaps = 50,
    aes(center_x, center_y, label = NAME_3) #,
    # nudge_x = 0.2,
    # nudge_y = 0.2
  ) + 
  # add title and subtitle
  labs(
    title = "hoge",
    subtitle = "Chiang Mai province, Thailand"
  ) +
  # add a rectangle indicating approximate location of the Bang Pu target temple area
  annotate(
    "rect", fill = NA, color = "black",
    xmin = 98.975, xmax = 98.995,
    ymin = 18.785, ymax = 18.795
  ) +
  theme_classic()
# 
ggsave(
  "Chiang_Mai_map.pdf",
  plot = chiang_mai_map,
  width = 200,
  height = 400,
  units = "mm",
  device = cairo_pdf # important!!
)

# 
# ----- ASEAN map ----------
# (Part of Figure 1)
# Download medium resolution (1:50m)
# Shapefiles downloaded from GADM are too high in resolution.
# It is unable to refine a map made from the shapefiles.
# Instead, we downloaded shape of countries from below.
# library
# https://docs.ropensci.org/rnaturalearth/articles/rnaturalearth.htmlhttps://docs.ropensci.org/rnaturalearth/articles/rnaturalearth.html
# origin
# https://github.com/ropensci/rnaturalearth
asean_countries <- 
  rnaturalearth::ne_countries(
    scale = "medium", 
    returnclass = "sf"
    ) %>%
  dplyr::filter(
    admin %in% c(
      "Thailand", "Myanmar", "Laos", "Cambodia", "Vietnam","Malaysia"
      )
    ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  )
# draw
asean_map <- 
  asean_countries %>%  
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(data = asean_countries %>% dplyr::filter(name_en == "Thailand"),fill = "grey88") +
  geom_sf(data = adm_01 %>% dplyr::filter(NAME_1 == "Chiang Mai"),fill = "black") +
  geom_text_repel(data = asean_countries, aes(center_x, center_y, label = name_en)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  annotate(
    "rect", fill = NA, color = "white",
    xmin = 98.85, xmax = 99.05,
    ymin = 18.70, ymax = 18.85
  ) +
  theme_classic()
# save
ggsave(
  "asean_map.pdf",
  plot = asean_map,
  width = 200,
  height = 400,
  units = "mm"
  )
# 
# ----- pick.up.frames -----
# (Figure 2)
av_video_images(
  video = "temples_wheelchair_sunday_morning.avi",              # Path to movie
  destdir = "frames",                    # Output folder (will be created if doesn't exist)
  format = "png",                        # Output format (png, jpg, etc.)
  fps = 1                                # Frames per second (e.g., 1 = one frame per second)
)

# ----- estimated.parameters.x -----
# (Figure 3)
# read data
survey_data <- 
  readr::read_csv("object_time_second_stan.csv")
whole_model <- 
  readr::read_rds("fit_moving_speed_spatial_ar1_interaction.rds")
results <- 
  readr::read_csv("fit_moving_speed_spatial_ar1_interaction_summary.csv") |> 
  data.table::setnames(c("parameter", "mean", "sd", "median", "lcl", "ucl", "rhat", "ess_bulk", "ess_tail"))
df_full <- 
  readr::read_csv("df_full.csv")
standard_time <- 
  df_full |> 
  dplyr::select(standard_time) |> 
  slice(1:4837)
# make data for drawing
# x
results_x <- 
  results |>
  # pick up parameters including x[t]
  dplyr::filter(stringr::str_detect(parameter, "x\\[")) |> 
  dplyr::mutate(
    x_backtransformed = exp(mean),
    lcl_backtransformed = exp(lcl),
    ucl_backtransformed = exp(ucl),
    standard_time = standard_time$standard_time,
    Mean_speed = mean
    ) |> 
  dplyr::mutate(
    color_key = dplyr::case_when(
      (lcl<0 & ucl <0) ~ "exclude",
      (lcl>0 & ucl >0) ~ "exclude",
      (lcl<0 & ucl >0) ~ "include",
      TRUE ~ "hoge"
    )
  ) 

results_x %>% dplyr::filter(color_key == "exclude") %>% nrow()# 
# results_x |> filter(color_key == "include") |> summarize(Min. = min(x_backtransformed), Max. = max(x_backtransformed))
# draw
line_x <- 
  results_x |>
  ggplot2::ggplot(
    aes(
      x = standard_time,
      y = x_backtransformed
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lcl_backtransformed,
      ymax = ucl_backtransformed
      ),
    fill = "grey88"
    ) +
  geom_line(aes(color=color_key)) +
  # geom_segment(aes(x = 0, y = 2.08, xend = 3500, yend = 2.08), color = "orange") + 
  labs(
    x = "Time (Unit: Sec.)",
    y = "Estimated latent trend (x)",
    color = "Include zero in 95% CI?"
  ) +
  ylim(0,10) + 
  viridis::scale_color_viridis(discrete = TRUE, option = "plasma") +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# save
# Comment out when not in use
# ggsave(
#   "line_x.pdf",
#   plot = line_x,
#   width = 150,
#   height = 150,
#   units = "mm"
# )
# 
# ----- speed.map ----------
# (Figure 4)
# Load your location data
survey_data <- 
  readr::read_csv("object_time_second_stan.csv")
# Filter rows with valid lat/lon
survey_points <- survey_data %>%
  dplyr::filter(!is.na(lat), !is.na(lon)) %>%
  dplyr::select(lat, lon, occasion, mode, standard_time_order, Mean_speed)
# Convert to sf
survey_sf <- 
  sf::st_as_sf(
    survey_points, 
    coords = c("lon", "lat"), 
    crs = 4326
  )
# draw the map
location_map <- 
  ggplot2::ggplot() +
  # street
  geom_sf(data = streets$osm_lines, color = "grey", size = 0.2, alpha = 1.0) +
  # road
  geom_sf(data = road$osm_lines, color = "orange", size = 0.4, alpha = 1.0) +
  # points
  geom_sf(data = survey_sf, aes(color = Mean_speed), size = 0.7, alpha = 0.25) +
  # color for points
  khroma::scale_color_smoothrainbow() +
  # geom_sf(data = sig_sf, shape = 21, fill = "yellow", color = "red", size = 3, stroke = 1.2) +
  # scale
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
  # north arrow
  ggspatial::annotation_north_arrow(
    height = unit(5, "mm"), width = unit(5, "mm"), pad_x = unit(5, "mm"), pad_y = unit(10, "mm")) +
  coord_sf(xlim = c(98.980, 98.990), ylim = c(18.785, 18.790), expand = TRUE) +
  # annotate text and points
  # WCL
  annotate("point", fill = NA, color = "black", x = 98.98658, y = 18.78697) +
  annotate("text", color = "black", x = 98.98658, y = 18.78670, size = unit(5, "pt"), label = "WCL") +
  # WPT
  annotate("point", fill = NA, color = "black", x = 98.98790, y = 18.78781) +
  annotate("text", color = "black", x = 98.98790, y = 18.78754, size = unit(5, "pt"), label = "WPT") +
  # WPS
  annotate("point", fill = NA, color = "black", x = 98.98150, y = 18.78850) +
  annotate("text", color = "black", x = 98.98150, y = 18.78823, size = unit(5, "pt"), label = "WPS") +
  # WIK
  annotate("point", fill = NA, color = "black", x = 98.98633, y = 18.78980) +
  annotate("text", color = "black", x = 98.98633, y = 18.78953, size = unit(5, "pt"), label = "WIK") +
  # TKM
  annotate("point", fill = NA, color = "black", x = 98.98693, y = 18.79000) +
  annotate("text", color = "black", x = 98.98693, y = 18.78973, size = unit(5, "pt"), label = "TKM") +
  # label
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "\u00a9 OpenStreetMap contributors",
    color = "Speed (km/s)"
  ) +
  # adjust axes label span
  scale_x_continuous(breaks = seq(98.980, 98.990, by = 0.004)) +
  scale_y_continuous(breaks = seq(18.785, 18.790, by = 0.002)) +
  # split by mode
  facet_wrap(~ mode, ncol = 1) +
  theme_classic()　+
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(2, "mm")
  )
# Plot
print(location_map)

ggsave(
  "location_map.pdf",
  plot = location_map,
  width = 150,
  height = 150,
  units =  "mm"
)
# 
# ----- estimated.parameters.beta -----
# (Extra)
# NOTE
# Table 1 is enough to express the effect
# Interpretation
# "At time points where the 95% credible interval of exp(x[t]) includes 1 (approximately between 0.3 and 2.7), we cannot confidently attribute an increase or decrease in latent speed trends."
# 3355 out of 4835 of x include zero in their 95% CI, suggesting the latent trend x[t] affects 
# 
# beta_car_by_mode (wheelchair)
results_car_wheelchair <- 
  whole_model$draws("beta_car_by_mode[1]") |>
  bayesplot::mcmc_dens_overlay() +
  viridis::scale_color_viridis(discrete = TRUE, option = "plasma") +
  labs(
    x = latex2exp::TeX("$\\beta_{car \\times wheelchair}$"),
    y = "Frequency"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
# save
ggsave(
  "results_car_wheelchair.pdf",
  plot = results_car_wheelchair,
  width = 100,
  height = 100,
  units = "mm"
)








