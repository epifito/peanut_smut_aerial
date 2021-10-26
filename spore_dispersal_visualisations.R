library("tidyverse")
library("classInt")
library("clifro")
library("ggpubr")

load("data/dat.Rda")
wind <- read_csv("data/wind.csv")

wind <- mutate(wind, FIELD = paste0("field_", FIELD))

dat <-
  dat %>%
  left_join(wind, by = c("field" = "FIELD")) %>%
  mutate(
    field = factor(field),
    trap_coord_deg = case_when(
      trap_coord == "N" ~ 0,
      trap_coord == "NbE" ~ 11.25,
      trap_coord == "NNE" ~ 22.5,
      trap_coord == "NEbN" ~ 33.75,
      trap_coord == "NE" ~ 45,
      trap_coord == "NEbE" ~ 56.25,
      trap_coord == "ENE" ~ 67.5,
      trap_coord == "EbN" ~ 73.5,
      trap_coord == "E" ~ 90,
      trap_coord == "EbS" ~ 101.2,
      trap_coord == "ESE" ~ 112.5,
      trap_coord == "SEbE" ~ 123.8,
      trap_coord == "SE" ~ 135.1,
      trap_coord == "SEbS" ~ 146.3,
      trap_coord == "SSE" ~ 157.6,
      trap_coord == "SbE" ~ 168.8,
      trap_coord == "S" ~ 180,
      trap_coord == "SbW" ~ 191.2,
      trap_coord == "SSW" ~ 202.5,
      trap_coord == "SWbS" ~ 213.8,
      trap_coord == "SW" ~ 225,
      trap_coord == "SWbW" ~ 236.2,
      trap_coord == "WSW" ~ 247.5,
      trap_coord == "WbS" ~ 258.8,
      trap_coord == "W" ~ 270,
      trap_coord == "WbN" ~ 281.2,
      trap_coord == "WNW" ~ 292.5,
      trap_coord == "NWbW" ~ 303.8,
      trap_coord == "NW" ~ 315,
      trap_coord == "NWbN" ~ 326.2,
      trap_coord == "NNW" ~ 337.5,
      trap_coord == "NbW" ~ 348.8
    ),
    W_DIR = case_when(
      W_DIR == "N" ~ 0,
      W_DIR == "NbE" ~ 11.25,
      W_DIR == "NNE" ~ 22.5,
      W_DIR == "NEbN" ~ 33.75,
      W_DIR == "NE" ~ 45,
      W_DIR == "NEbE" ~ 56.25,
      W_DIR == "ENE" ~ 67.5,
      W_DIR == "EbN" ~ 73.5,
      W_DIR == "E" ~ 90,
      W_DIR == "EbS" ~ 101.2,
      W_DIR == "ESE" ~ 112.5,
      W_DIR == "SEbE" ~ 123.8,
      W_DIR == "SE" ~ 135.1,
      W_DIR == "SEbS" ~ 146.3,
      W_DIR == "SSE" ~ 157.6,
      W_DIR == "SbE" ~ 168.8,
      W_DIR == "S" ~ 180,
      W_DIR == "SbW" ~ 191.2,
      W_DIR == "SSW" ~ 202.5,
      W_DIR == "SWbS" ~ 213.8,
      W_DIR == "SW" ~ 225,
      W_DIR == "SWbW" ~ 236.2,
      W_DIR == "WSW" ~ 247.5,
      W_DIR == "WbS" ~ 258.8,
      W_DIR == "W" ~ 270,
      W_DIR == "WbN" ~ 281.2,
      W_DIR == "WNW" ~ 292.5,
      W_DIR == "NWbW" ~ 303.8,
      W_DIR == "NW" ~ 315,
      W_DIR == "NWbN" ~ 326.2,
      W_DIR == "NNW" ~ 337.5,
      W_DIR == "NbW" ~ 348.8
    )
  )

wr <-
  with(
    dat,
    windrose(
      W,
      W_DIR,
      field,
      n_col = 3,
      legend_title = "Wind speed (m/s)"
    )
  )
wr +
  scale_fill_viridis_d(name = "Wind Speed (m/s)", direction = -1, option = "E") +
  xlab("") +
  theme_pubclean()

# Spore counts density plot ---------
ggplot(dat, aes(x = spores)) +
  geom_density() +
  xlab("Spores (n)") +
  theme_pubclean()

# Dispersal distance, no directions --------

# All fields
ggplot(dat, aes(x = distance_m,
                y = spores)) +
  geom_count() +
  scale_size(breaks = c(1, 2, 4, 8)) +
  stat_summary(fun.y = "median",
               geom = "line",
               na.rm = TRUE) +
  stat_summary(
    fun.y = "median",
    colour = "red",
    size = 2,
    geom = "point"
  ) +
  scale_x_continuous(breaks = c(0, 10, 25, 50, 75)) +
  ylim(c(-0.5, 9)) +
  ylab("Spores (n)") +
  xlab("Distance (m)") +
  theme_pubclean()

# Facet by field
ggplot(dat, aes(x = distance_m,
                y = spores)) +
  geom_count() +
  scale_size(breaks = c(1, 2, 4, 8)) +
  stat_summary(fun.y = "median",
               geom = "line",
               na.rm = TRUE) +
  stat_summary(
    fun.y = "median",
    colour = "red",
    size = 2,
    geom = "point"
  ) +
  scale_x_continuous(breaks = c(0, 10, 25, 50, 75)) +
  ylim(c(-0.5, 9)) +
  ylab("Spores (n)") +
  xlab("Distance (m)") +
  facet_wrap(. ~ field, ncol = 3) +
  theme_pubclean()

# Dispersal on a compass rose ------
heat_dat <-
  dat %>%
  group_by(field, trap_coord_deg)

# generate breaks in data for colour and size values
spore_cuts <-
  round(classIntervals(heat_dat$spores, style = "fisher", n = 3)$brks, 0)

ggplot(data = heat_dat,
       aes(
         x = trap_coord_deg,
         y = distance_m,
         colour = spores,
         size = spores
       )) +
  geom_count() +
  scale_colour_viridis_c(
    name = "n",
    guide = "legend",
    option = "E",
    breaks = spore_cuts
  ) +
  coord_polar(theta = "x",
              start = 0,
              direction = 1) +
  scale_size(
    range = c(0, 8),
    name = "n",
    breaks = spore_cuts
  ) +
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    expand = c(0, 0),
    limits = c(0, 360),
    labels = c("N", "E", "S", "W")
  ) +
  scale_y_continuous(
    breaks = c(100, 175, 250, 325, 400),
    limits = c(0, 400)) +
  ylab("Distance (m)") +
  xlab("Transect") +
  facet_wrap(. ~ field, ncol = 3) +
  theme_pubclean()
