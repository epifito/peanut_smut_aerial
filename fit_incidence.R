library(tidyverse)
library(mgcv)
library(here)

# import data ---
sdp_dat <- read_csv(here("data/sdp_data.csv"),
                    show_col_types = FALSE) %>% 
  arrange(field) %>% 
  mutate(field = as_factor(field))

load(here("data/mod_dat.Rdata"))

# summarise field incidence ----

ps_inc <- 
  sdp_dat |> 
  group_by(field) |> 
  summarise(inc = mean(inc))

# prep data for model ----

mod_dat2 <- left_join(mod_dat, ps_inc, by = c("field"))

mod_dat2$xy <- as.factor(paste(mod_dat2$x, mod_dat2$y))

# create predictor degree diff as per Ben Bolker for circular predictors,
# https://fediscience.org/@bbolker/110856805100981742, see Appendix for screenshot
mod_dat2$degree_dif_sin <- sin(2 * pi * mod_dat2$degree_dif / 360)


# model 1 -----

m1 <- gam(
  spore_cm2 ~ s(time_slice, k = 3) +
    s(degree_dif_sin, k = 72) +
    s(distance_m, k = 4) +
    s(field, xy, bs = "re"),
  data = mod_dat2,
  select = TRUE,
  method = "REML",
  family = "tw"
)

# model 2 -----

m2 <- gam(
  spore_cm2 ~ s(time_slice, k = 3) +
    s(degree_dif_sin, k = 72) +
    s(distance_m, k = 4) +
    s(inc, k = 5) +
    s(field, xy, bs = "re"),
  data = mod_dat2,
  select = TRUE,
  method = "REML",
  family = "tw"
)

# check models' fitness

gam.check(m1)

gam.check(m2)

summary(m1)

summary(m2)

AIC(m1, m2) # m1, the more parsimonious model is the better model w/o incidence
