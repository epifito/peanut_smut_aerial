library(brms)
library(bayesplot)
library(bayestestR)
library(here)
library(parallel)

options(mc.cores = detectCores())

load(here("data/dat.Rdata"))
load(here("data/mod_dat.Rdata"))

priors <- prior_string("cauchy(0, 10)")

bm1 <-
  brm(
    formula = spore_cm2 ~ downwind + (1 | field) ,
    data = mod_dat,
    seed = 27,
    prior = priors,
    family = "hurdle_gamma",
    control = list(adapt_delta = 0.999,
                   max_treedepth = 15),
    iter = 1000,
    chains = 4
  )

pp_check(bm1)

plot(pd(bm1))


bm2 <-
  brm(
    formula = spore_cm2 ~ distance_m + (1 | field) ,
    data = mod_dat,
    seed = 27,
    prior = priors,
    family = "hurdle_gamma",
    chains = 4
  )

pp_check(bm2)

plot(pd(bm2))

bm3 <-
  brm(
    formula = spore_cm2 ~ time_min + (1 | field),
    data = mod_dat,
    seed = 27,
    prior = priors,
    family = "hurdle_gamma",
    control = list(adapt_delta = 0.999,
                   max_treedepth = 15),
    iter = 2000,
    chains = 4
  )

pp_check(bm3)

plot(pd(bm3))

