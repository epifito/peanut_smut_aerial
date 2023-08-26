
# This script details model fitting to find the best-fitting model, specifically
# which predictors should be included in the final best-fitting version

library("tidyverse")
library("mgcv")
library("here")

# import data ---
sdp_dat <- read_csv(here("data/sdp_data.csv"),
                    show_col_types = FALSE) %>%
  arrange(field) %>%
  mutate(field = as_factor(field))

load(here("data/mod_dat.Rdata"))

# summarise SDP index by field and create a binomial variable w/ cutoff @ 5 ----

ps_inc <-
  sdp_dat |>
  group_by(field) |>
  summarise(inc = mean(inc),
            SDP = mean(SDP),
            DSI = mean(DSI)) |> 
  mutate(sdp_binom = as.factor(if_else(SDP <= 5, 0, 1)))

# prep data for model ----

mod_dat2 <- left_join(mod_dat, ps_inc, by = c("field"))

mod_dat2$xy <- as.factor(paste(mod_dat2$x, mod_dat2$y))

# create predictor degree diff as per Ben Bolker for circular predictors,
# https://fediscience.org/@bbolker/110856805100981742, see Appendix for screenshot
mod_dat2$degree_dif_sin <- sin(2 * pi * mod_dat2$degree_dif / 360)

# model 1 w/ field incidence -----

m1 <- gam(
  spore_cm2 ~ s(time_slice, k = 3) +
    s(degree_dif_sin, k = 72) +
    s(distance_m, k = 4) +
    s(inc, k = 5) +
    s(field, bs = "re") +
    s(field, xy, bs = "re"),
  data = mod_dat2,
  select = TRUE,
  method = "REML",
  family = "tw"
)

gam.check(m1)
summary(m1)

# model 2 w/ SDP index binomial ----

m2 <- gam(
  spore_cm2 ~ s(time_slice, k = 3) +
    s(degree_dif_sin, k = 72) +
    s(distance_m, k = 4) +
    sdp_binom +
    s(field, bs = "re") +
    s(field, xy, bs = "re"),
  data = mod_dat2,
  select = TRUE,
  method = "REML",
  family = "tw"
)

gam.check(m2)
summary(m2)

# model 3 w/ SDP ----

m3 <- gam(
    spore_cm2 ~ s(time_slice, k = 3) +
      s(degree_dif_sin, k = 72) +
      s(distance_m, k = 4) +
      s(SDP, k = 6) +
      s(field, xy, bs = "re"),
    data = mod_dat2,
    select = TRUE,
    method = "REML",
    family = "tw"
)

gam.check(m3)
summary(m3)

# model 4 w/ DSI ----

m4 <- gam(
  spore_cm2 ~ s(time_slice, k = 3) +
    s(degree_dif_sin, k = 72) +
    s(distance_m, k = 4) +
    s(DSI, k = 6) +
    s(field, xy, bs = "re"),
  data = mod_dat2,
  select = TRUE,
  method = "REML",
  family = "tw"
)

gam.check(m4)
summary(m4)

# org model ----
m_org <- gam(
  spore_cm2 ~ s(time_slice, k = 3) +
    s(degree_dif_sin, k = 72) +
    s(distance_m, k = 4) +
    s(field, xy, bs = "re"),
  data = mod_dat2,
  select = TRUE,
  method = "REML",
  family = "tw"
)

AIC(m1, m2, m3, m4, m_org)

# m_org is the better model by AIC
