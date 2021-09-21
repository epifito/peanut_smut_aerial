```{r}
df <- structure(list(States = structure(c(1L, 3L, 2L, 4L), 
                                        .Label = c("AP", "Gujarat", "Punjab", "Rajasthan"), 
                                        class = "factor"), 
                     a = c(20, 45, -15, 10), 
                     b = c(14, -67, 45, -5)), 
                class = "data.frame", 
                row.names = c(NA, -4L))

ggplot(df, aes(a, b), scale="globalminmax") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  theme_minimal()
 

pacman::p_load(rattle.data)

aus_temp <- weather %>%
  select(Date, Temp9am, Temp3pm) %>%
  pivot_longer(cols = Temp9am:Temp3pm, names_to="temp", values_to="value") %>% 
  ggplot(aes(x=Date)) +
  geom_line(aes(y=value, color=temp)) +
  labs(x="",y="Temperature (Celsius)")

aus_temp + coord_polar()
```

