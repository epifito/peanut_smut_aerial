library(brms)
library(bayesplot)
library(bayestestR)

load(here("data/dat.Rdata"))
load(here("data/mod_dat.Rdata"))

bm1 <-
  brm(
    formula = spore_cm2 ~  downwind + (1 | field) ,
    data = mod_dat,
    seed = 27,
    prior = priors,
    family = "hurdle_gamma",
    iter = 3000,
    chains = 4
  )

pp_chec(bm1)

plot(pd(m1))
