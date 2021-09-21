pacman::p_load(tidyverse, googlesheets4)

gs4_auth()
url <- "https://docs.google.com/spreadsheets/d/1fBHWS1qCVNoj3yn0VcK9-598CS-Q95esJx8SXvPd5Uo/edit?usp=sharing"
smut <- gs4_get(url)
gs4_browse(smut)

dat <- data_frame(field = sheet_names(smut) %>% str_subset(pattern = "^field")) %>% 
  mutate(data = map(field, ~read_sheet(smut, sheet = .x))) %>% 
  tidyr::unnest(cols = c(data))  %>% 
  mutate_if(is.character, as.factor)

save(dat, file = "data/dat.Rda")

