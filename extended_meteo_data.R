```{r bin-time-slices-rh, eval=FALSE}
rh_90 <-
  m %>%
  filter(time_slice == 90) %>%
  group_by(field) %>%
  summarise(rh = mean(rel_hum))

rh_180 <-
  m %>%
  filter(time_slice == 90 | time_slice == 180) %>%
  group_by(field) %>%
  summarise(rh = mean(rel_hum))

rh_270 <-
  m %>%
  group_by(field) %>%
  summarise(rh = mean(rel_hum))

rh <-
  bind_rows(list(
    "90" = rh_90,
    "180" = rh_180,
    "270" = rh_270
  ), .id = "time_slice")
```


```{r bin-time-slices-temp, eval=FALSE}
temp_90 <-
  m %>%
  filter(time_slice == 90) %>%
  group_by(field) %>%
  summarise(temp = mean(temp))

temp_180 <-
  m %>%
  filter(time_slice == 90 | time_slice == 180) %>%
  group_by(field) %>%
  summarise(temp = mean(temp))

temp_270 <-
  m %>%
  group_by(field) %>%
  summarise(temp = mean(temp))

temp <-
  bind_rows(list(
    "90" = temp_90,
    "180" = temp_180,
    "270" = temp_270
  ), .id = "time_slice")
```


```{r bin-time-slices-wind, eval=FALSE}
wind_speed_90 <-
  m %>%
  filter(time_slice == 90) %>%
  group_by(field) %>%
  summarise(wind_speed = mean(wind_speed))

wind_speed_180 <-
  m %>%
  filter(time_slice == 90 | time_slice == 180) %>%
  group_by(field) %>%
  summarise(wind_speed = mean(wind_speed))

wind_speed_270 <-
  m %>%
  group_by(field) %>%
  summarise(wind_speed = mean(wind_speed))

wind_speed <-
  bind_rows(list(
    "90" = wind_speed_90,
    "180" = wind_speed_180,
    "270" = wind_speed_270
  ), .id = "time_slice")
```


```{r bin-time-slices-wind_degrees, eval=FALSE}
wind_degrees_90 <-
  m %>%
  filter(time_slice == 90) %>%
  group_by(field) %>%
  summarise(wind_degrees = circular.averaging(wind_degrees))

wind_degrees_180 <-
  m %>%
  filter(time_slice == 90 | time_slice == 180) %>%
  group_by(field) %>%
  summarise(wind_degrees = circular.averaging(wind_degrees))

wind_degrees_270 <-
  m %>%
  group_by(field) %>%
  summarise(wind_degrees = circular.averaging(wind_degrees))

wind_degrees <-
  bind_rows(list(
    "90" = wind_degrees_90,
    "180" = wind_degrees_180,
    "270" = wind_degrees_270
  ), .id = "time_slice")

meteo_data <-
  rh %>% 
  left_join(temp, by = c("field", "time_slice")) %>% 
  left_join(wind_speed, by = c("field", "time_slice")) %>% 
  left_join(wind_degrees, by = c("field", "time_slice")) %>% 
  mutate(time_slice = as.numeric(time_slice)) %>% 
  arrange(field, time_slice)
```
