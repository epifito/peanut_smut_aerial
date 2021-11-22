# update_drive <- function(x){
#   list.files(path = here::here("plots_", x), 
#              pattern = ".png",
#              all.files = TRUE, full.names = TRUE) #%>% 
#     # map(~ drive_upload(.,
#     #                    path = as_dribble("juanchi_guille/tandilia_fina_2020/rinde"),
#     #                    overwrite = TRUE)
#     # )
# }
# update_drive("rinde")
# here::here("some", "path", "below", "your", "project", "root.txt")

theme_aapre0 <- theme_bw(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1), 
    legend.position = "top", legend.justification = "right",
    plot.title = element_text(family = 'Helvetica', 
                              face = 'bold', 
                              hjust = 0, 
                              vjust = -5),
    plot.caption = element_text(hjust = 0,
                                color = "gray30", face = "italic"))

theme_aapre <- theme_bw(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype =  "dotted"),
    panel.grid.major.x = element_line(linetype =  "dashed"),
    axis.text.x = element_text(angle = 60, hjust = 1), 
    legend.position = "top", legend.justification = "right",
    plot.title = element_text(family = 'Helvetica', 
                              face = 'bold', 
                              hjust = 0, 
                              vjust = 0),
    plot.caption = element_text(hjust = 0,
                                color = "gray30", face = "italic"))

theme_dens <- theme_bw(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(linetype =  "dotted"),
    panel.grid.major.x = element_line(linetype =  "dashed"),
    legend.position = "top", legend.justification = "right",
    plot.title = element_text(family = 'Helvetica', 
                              face = 'bold', 
                              hjust = 0, 
                              vjust = -7),
    axis.text.x = element_text(
      hjust = -0.2,
      vjust = 6
      ),
    # text = element_text(size = 15),
    axis.ticks.length.x = unit(0.5, "cm"))
# axis.text.x = element_text(, hjust = 1), 


#https://scottishsnow.wordpress.com/2020/04/24/lubridate-ggplot-date-helpers/  

theme_dens1_legend <- theme_bw(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    panel.grid.major = element_line(linetype =  "dotted"),
    legend.position = "top", legend.justification = "right",
    plot.title = element_text(family = 'Helvetica', 
                              face = 'bold', 
                              hjust = 0, 
                              vjust = -7),
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.caption = element_text(hjust = 0,
                                color = "gray30", face = "italic"))

theme_dens1 <- theme_bw(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    panel.grid.major = element_line(linetype =  "dotted"),
    legend.position = "top", legend.justification = "right",
    plot.title = element_text(family = 'Helvetica', 
                              face = 'bold' 
                              # hjust = 0, vjust = -7
                              ),
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.caption = element_text(hjust = 0,
                                color = "gray30", face = "italic"))

theme_dens2 <- theme_bw(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    panel.grid.major = element_line(linetype =  "dotted"),
    legend.position = "top", legend.justification = "right",
    plot.title = element_text(family = 'Helvetica', 
                              face = 'bold', 
                              hjust = 0, 
                              vjust = -10),
    plot.caption = element_text(hjust = 0,
                                color = "gray30", face = "italic"),
    plot.margin=grid::unit(c(-1,1,0,1), "mm"))

theme_bw2 <- theme_bw()+ 
            theme(panel.grid.major= element_line(color = gray(0.5), 
                                                 linetype = "dashed", size = 0.05),
                  panel.grid.minor= element_blank(), 
                  plot.title = element_text(family = 'Helvetica', 
                                            face = 'bold', 
                                            hjust = 0, 
                                            vjust = 0),
            )

theme_juan <- function (base_size = base_size, legen_pos = legen_pos) {
  theme_bw(base_size = base_size) %+replace% 
    theme(
      plot.title = element_text(family = 'Helvetica', 
                                face = 'bold', 
                                hjust = 0, 
                                vjust = 0),
      
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black", size=rel(1)),
      axis.title.y = element_text(colour = "black", angle=90),
      
      strip.background = element_blank(), 
      strip.text = element_text(size = rel(1.1)),#,face = "bold"),
      
      # panel.border = element_blank(),
      axis.line    = element_line(color='grey'),
      
      panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      panel.grid.major = element_line(linetype =  "dotted"),
      
      # panel.grid.major.y = element_line(linetype =  "dotted"),
      # panel.grid.major.x = element_line(linetype =  "dotted") ,
      # legend
      legend.position=legen_pos,
      panel.spacing = unit(1,"lines")
    )  
}

#---- Theme maps ----
theme_map <- theme_bw()+
  theme(
    panel.grid.major = 
      element_line(color = gray(0.5), 
                   linetype = "dashed", size = 0.05), 
    panel.background = element_rect(fill = "aliceblue"),
    axis.text.x =  element_text(size = 6),
    axis.text.y = element_text(size = 6))

