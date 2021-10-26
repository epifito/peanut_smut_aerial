library("tidyverse")
library("mgcv")
library("classInt")
library("gratia")
library("janitor")

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

m1 <- gam(
  spores ~
    s(trap_coord_deg, k = 3) +
    s(field, bs = "re") +
    s(time_min, bs = "re"),
  family = tw(),
  data = dat,
  method = "REML"
)
summary(m1)
appraise(m1)


m2 <- gam(
  spores ~
    s(distance_m, k = 3) +
    s(field, bs = "re") +
    s(time_min, bs = "re"),
  family = tw(),
  data = dat,
  method = "REML"
)
summary(m2)
appraise(m2)

m3 <- gam(
  spores ~
    s(trap_coord_deg, k = 3) +
    s(distance_m, k = 3) +
    s(field, bs = "re") +
    s(time_min, bs = "re"),
  family = tw(),
  data = dat,
  method = "REML"
)
summary(m3)
appraise(m3)

m4 <- gam(
  spores ~
    s(trap_coord_deg, k = 3) +
    s(distance_m, k = 3) +
    s(W, k = 50) +
    s(field, bs = "re") +
    s(time_min, bs = "re"),
  family = tw(),
  data = dat,
  method = "REML"
)
summary(m4)
appraise(m4)
