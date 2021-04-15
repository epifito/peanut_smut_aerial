library(tidyverse)
# pacman::p_load(tidyverse, googlesheets4)
# gs4_find("meteorological_data")
# meteo <- gs4_get("https://docs.google.com/spreadsheets/d/15yGsFy4Bkh5WvQ1xdiPJWIH3R7S9cjf5TS2YyjljPss/edit?usp=sharing")
# 
# meteo_data <- meteo %>% 
#    sheet_names() %>%
#    map_df(~ read_sheet(meteo, sheet = .,  range = "A:F")) %>%
#    bind_rows()
# # 
# write_csv(meteo_data, file= "data/wind.csv")


meteo_dat <- read_csv("data/wind.csv")

meteo_dat %>% filter(is.na(W_DIR))

meteo_dat <-
  meteo_dat %>% 
  mutate(W_DIR= factor(meteo_dat$W_DIR, levels = c(levels(meteo_dat$W_DIR), 
                                                   c("N","NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))
                       )
         )
table(meteo_dat$W_DIR)

meteo_dat %>% 
  group_by(FIELD) %>% 
  count(W_DIR)

meteo_dat %>% 
  ggplot(aes(x=W_DIR, fill=W_DIR))+ 
  geom_bar(stat="count",width=1,colour="black",size=0.1, alpha= 0.5)+ 
  coord_polar(theta = "x", start=6.0729, direction = 1)+
  scale_color_discrete ()+ 
  labs(x="", y="")+ 
  scale_fill_discrete(drop=FALSE) + scale_x_discrete(drop=FALSE)+
  guides(fill="none") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap("FIELD")

ggsave(last_plot(), file = "plots/wind_rose.jpg", width = 15, height = 10, units = "cm",  dpi = 600)
