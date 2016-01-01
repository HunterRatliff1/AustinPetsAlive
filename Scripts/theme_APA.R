theme_APA <- function() {
  require(ggplot2)
  require(ggthemes)
  
  theme(
    text                 = element_text(family = "Georgia", color="#dd5928"),
    title                = element_text(face = "bold"),
    axis.text            = element_text(family = "Verdana", color="#4D4D4D"),
    axis.line            = element_line(color="#4D4D4D"),
    legend.box           = "horizontal",
    legend.direction     = "horizontal",
    panel.border         = element_rect(color="#A7C539", fill = NA),
    panel.background     = element_rect(fill="white"),
    panel.grid.major.x   = element_line(linetype = "dotted", color="#4d4d4d", size=0.50),
    panel.grid.minor.x   = element_line(linetype = "dotted", color="#4d4d4d", size=0.15),
    # panel.grid.major.y   = element_line(linetype = "dashed", color="#3A7299", size=0.50),
    # panel.grid.minor.y   = element_line(linetype = "dashed", color="#3A7299"),
    strip.background     = element_rect(fill="#4d4d4d"),
    strip.text           = element_text(family = "Verdana", face = "italic", color="#A7C539"),
    plot.background      = element_rect(fill="#CFCFCF"),
    plot.title           = element_text(size=18, hjust = 0),
    
    legend.position      = "bottom",
    legend.background    = element_rect(fill="white", colour = "#3B7D00")
  )
}