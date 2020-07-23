

library(cowplot)
library(scales)
library(tidyverse)


fill_directional <- function(p_barlot_name){
  jet.colors <- colorRampPalette(c("#0099dc",
                                   "#e7f0fa",
                                   "#ce472e"))
  col_pos = jet.colors(3)
  return(scale_fill_gradientn(colours = col_pos,
                              guide = guide_legend(
                                title=plot_name,
                                direction = "horizontal",
                                title.position = "top",
                                label.position = "bottom",
                                label.hjust = 0.5,
                                label.vjust = 1,
                                keyheight = 0.3)))
}
fill_non_directional <- function(plot_name){
  return(scale_fill_gradientn(colours = c("#e7f0fa","#c9c2d8",
                                          "#ce472e"),
                              guide = guide_legend(
                                title=plot_name,
                                direction = "horizontal",
                                title.position = "top",
                                label.position = "bottom",
                                label.hjust = 0.5,
                                label.vjust = 1,
                                keyheight = 0.3)))
}
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

ds = readRDS(file="~/Resource/SciVis/heatmap_ARD2017_fig2/ds.rds")

# Bar plot on the top ####
plot_name = "# Sig. Cat." 
ds[[plot_name]]%>%
  ggplot(.,aes(uniq_genset_key,n,fill=Cat))+
  geom_bar(stat = "identity")+
  facet_grid(PN ~ .,
             scales = "free",
             space = "free",switch = "y",)+
  labs(x = "",y = "") +
  scale_x_discrete(drop=FALSE,expand = c(0,0))+
  scale_y_continuous(expand = c(0, 0))+
  theme_Publication()+
  discrete_scale("fill","Publication",
                 manual_pal(values = c("#386cb0","#fdb462","#7fc97f",
                                       "#ef3b2c","#662506","#a6cee3",
                                       "#fb9a99","#984ea3","#ffff33")))+
  theme(axis.line=element_blank(),
        strip.placement="outside",
        axis.ticks =element_blank(),
        axis.text.x = element_blank(),
        legend.position="right")+
  guides(fill=guide_legend(
    title=paste0(plot_name,"\nCategory"),
    direction = "horizontal",
    title.position = "top",
    label.position = "bottom",
    label.hjust = 0.5,
    label.vjust = 1,
    keyheight = 0.3))-> p_bar

# Heatmap on the 2nd row ####
plot_name = "DAI"
ds[[plot_name]]%>%
  ggplot(.,aes(uniq_genset_key,uniq_analysis_key))+
  geom_tile(aes(fill=t),colour = "white")+
  geom_text(aes(label=stars), color="black",vjust = 0.8)+
  facet_grid(Cat ~ ., scales = "free", space = "free",switch = "y")+
  labs(x = "",y = "") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  theme_Publication()+
  fill_directional(paste0(plot_name,"\nT-stat"))+
  theme(axis.ticks =element_blank(),
        axis.text.x = element_blank(),
        legend.position="right",
        axis.line=element_blank(),
        strip.placement="outside")  -> p_dai

# Heatmap on the 3rd row ####
plot_name = "DEG"
ds[[plot_name]]%>%
  ggplot(.,aes(uniq_genset_key,uniq_analysis_key))+
  geom_tile(aes(fill=abs(Ratio)),colour = "white")+
  geom_text(aes(label=stars), color="black",vjust = 0.8)+
  facet_grid(Cat ~ ., scales = "free", space = "free",switch = "y")+
  labs(x = "",y = "") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  theme_Publication()+
  fill_non_directional(paste0(plot_name,"\nRatio"))+
  theme(axis.ticks =element_blank(),
        axis.text.x = element_blank(),
        legend.position="right",
        axis.line=element_blank(),
        strip.placement="outside") -> p_deg

# Heatmap on the 4th row ####
plot_name = "DCOR"
ds[[plot_name]]%>%
  ggplot(.,aes(uniq_genset_key,uniq_analysis_key))+
  geom_tile(aes(fill=Fold_enrich),colour = "white")+
  geom_text(aes(label=stars), color="black",vjust = 0.8)+
  facet_grid(Cat ~ ., scales = "free", space = "free",switch = "y")+
  labs(x = "",y = "") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  theme_Publication()+
  fill_non_directional(paste0(plot_name,"\nFold enrichment"))+
  theme(axis.ticks =element_blank(),
        axis.text.x = element_blank(),
        legend.position="right",
        axis.line=element_blank(),
        strip.placement="outside") -> p_dcor

# Heatmap on the 5th row ####
plot_name = "DMR"
ds[[plot_name]]%>%
  ggplot(.,aes(uniq_genset_key,uniq_analysis_key))+
  geom_tile(aes(fill=abs(fold.enrich)),colour = "white")+
  geom_text(aes(label=stars), color="black",vjust = 0.8)+
  facet_grid(Cat ~ ., scales = "free", space = "free",switch = "y")+
  labs(x = "Transcript module",y = "") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  theme_Publication()+
  fill_non_directional(paste0(plot_name,"\nFold enrichment"))+
  theme(axis.ticks =element_blank(),
        axis.text.x = element_blank(),
        legend.position="right",
        axis.line=element_blank(),
        strip.placement="outside")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) -> p_dmr

# combine all plots ####

p_list=list(p_bar,p_dai,p_deg,p_dcor,p_dmr)

# keep legends
lapply(p_list, function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} ) -> p_legends 

# remove spaces
for (i in 1:(length(p_list)-1)){
  p_list[[i]]=p_list[[i]]+theme(plot.margin = unit(c(0,0.5,-0.5,0), "cm"))
}
p_list[[1]]=p_list[[1]]+theme(plot.margin = unit(c(0.5,0.5,-0.5,0), "cm"))
p_list[[i+1]] = p_list[[i+1]]+theme(plot.margin = unit(c(0,0.5,0,0), "cm"))

# remove legend
for (i in 1:(length(p_list))){
  p_list[[i]]=p_list[[i]]+theme(legend.position="none")
}

# plot all together
p_list=p_list%>%
  lapply(., ggplotGrob)
p_list[[4]]$heights = p_list[[3]]$heights
do.call(what=rbind,
        args=c(p_list,size="first"))-> p 
p$widths = lapply(p_list, function(x){x$widths})%>%do.call(unit.pmax,.)

plot_heat_tr = plot_grid(p,do.call(plot_grid,
                                   args=c(p_legends,labels = NULL, ncol = 1, align = 'v')),
                         ncol=2,rel_widths = c(1,0.2))


save_plot("~/Resource/SciVis/heatmap_ARD2017_fig2/plot.png",
          plot_heat_tr,base_height = 10,base_width = 10)
