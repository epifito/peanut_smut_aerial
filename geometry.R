data.frame(trap_degree = c(0,0,90,90), 
           distance_m = c(100, 200, 100, 200))%>% 
  mutate(data.frame(pol2cart(theta = trap_degree * 0.01745329,
                             r = distance_m))) %>% 
  ggplot()+
  aes(x=y, y=x)+
  geom_point() +
  geom_text(aes(label=paste0(trap_degree,",", distance_m), vjust=1))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)


expand_grid(trap_degrees = c(0,90,180,270), 
            wind_degrees = c(0,90,180,270)) %>% 
  mutate(degree_dif0 = (wind_degrees - trap_degrees)) %>% 
  mutate(degree_dif=if_else(abs(degree_dif0)>180, abs(degree_dif0)-180, abs(degree_dif0)))