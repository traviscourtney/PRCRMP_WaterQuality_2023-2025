#####load required libraries####
library(ggmap)
library(viridis)
library(patchwork)
library(naniar)

####import Report 1 table and replace all -999 with NA to calculate summary stats and make plots####
Report_1_data=read.csv("Report_1_Data.csv") %>% 
  replace_with_na_all(condition = ~.x == -999)

#####summary of data#####
summary(Report_1_data) #this produces summary stats for all of the data we collected (doesn't work for things that aren't numeric)

#####register google key####
register_google(key = "AIzaSyD8X7CdMZBOCTufTLqtF7aB2pjJnIuVLg8") #my personal key linked to my credit card, please do not share

#####generate map of puerto rico####
pr <- get_map('puerto rico', zoom = 8, maptype = 'satellite')

#####plot sampled values at their respective site####
Temperature_plot=
  ggmap(pr)+
  geom_point(Report_1_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_Temp_C), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(a) Bottom Temperature (°C)")+
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black",face="italic"),
        axis.title.y=element_text(colour = "black"),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

Salinity_plot=
  ggmap(pr)+
  geom_point(Report_1_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_Sal_psu), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(b) Bottom Salinity")+
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black",face="italic"),
        axis.title.y=element_text(colour = "black"),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

DO_plot=
  ggmap(pr)+
  geom_point(Report_1_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_DO_mg_L), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(c) Bottom DO (mg/L)")+
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black",face="italic"),
        axis.title.y=element_text(colour = "black"),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

pH_plot=
  ggmap(pr)+
  geom_point(Report_1_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_pH), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(d) Bottom pH")+
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black",face="italic"),
        axis.title.y=element_text(colour = "black"),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

#combine into a multi-panel figure where + makes things side by side and / makes things top to bottom
HydroLab_plot=(Temperature_plot+Salinity_plot)/(DO_plot+pH_plot)
#shows the plot
HydroLab_plot
#saves the plot as a pdf
ggsave("HydroLab_plot.pdf",HydroLab_plot,height=6.5,width=14)
