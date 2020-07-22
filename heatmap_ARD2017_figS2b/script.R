
library(tidyverse)
ds = readRDS(file="~/Resource/SciVis/heatmap_ARD_figS2b/ds.rds")

theme_Publication <- function(base_size=14, base_family="Helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

jet.colors <- colorRampPalette(c("#00007F", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red"))(7)
ds%>%
  ggplot(.,aes(key,cell))+
  geom_tile(aes(fill=ks),colour = "white")+
  geom_text(aes(label=stars), color="black",vjust = 0.8)+
  labs(x = "IRIS cells",y = "Transcript merkers") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  theme_Publication()+
  scale_fill_gradientn(colours = jet.colors,
                       guide = guide_legend(
                         title="Kolmogorov-Smirnov statistic",
                         direction = "horizontal",
                         title.position = "top",
                         label.position = "bottom",
                         label.hjust = 0.5,
                         label.vjust = 1,
                         keyheight = 0.3))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  coord_flip() -> p

ggsave(plot = p, filename = "~/Resource/SciVis/heatmap_ARD_figS2b/plot.pdf",width = 6,height = 7)
