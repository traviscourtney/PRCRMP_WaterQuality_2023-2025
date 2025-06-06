#####load required libraries####
library(tidyverse)
library(ggmap)
library(viridis)
library(patchwork)
library(naniar)
library(readxl)
library(ggplot2)
library(psych)
library(hablar)

####import Report 1 table and replace all -999 with NA to calculate summary stats and make plots####

Report_2_data=read_excel("Report_2_Data_v5.xlsx")

#####convert all data to numeric to create and export summary table####
Report_2_data$Surface_Temp_C=as.numeric(Report_2_data$Surface_Temp_C)
Report_2_data$Surface_Sal_psu=as.numeric(Report_2_data$Surface_Sal_psu)
Report_2_data$Surface_DO_mg_L=as.numeric(Report_2_data$Surface_DO_mg_L)
Report_2_data$Surface_pH=as.numeric(Report_2_data$Surface_pH)
Report_2_data$Bottom_Temp_C=as.numeric(Report_2_data$Bottom_Temp_C)
Report_2_data$Bottom_Sal_psu=as.numeric(Report_2_data$Bottom_Sal_psu)
Report_2_data$Bottom_DO_mg_L=as.numeric(Report_2_data$Bottom_DO_mg_L)
Report_2_data$Bottom_pH=as.numeric(Report_2_data$Bottom_pH)
Report_2_data$`Chl-a (µg/L)`=as.numeric(Report_2_data$`Chl-a (µg/L)`)
Report_2_data$`Turbidity (NTU)`=as.numeric(Report_2_data$`Turbidity (NTU)`)
Report_2_data$`Surface DIC (µM)`=as.numeric(Report_2_data$`Surface DIC (µM)`)
Report_2_data$`Bottom DIC (µM)`=as.numeric(Report_2_data$`Bottom DIC (µM)`)


Report_2_summary_table=Report_2_data %>% 
  select(c("Surface_Temp_C",
           "Surface_Sal_psu",
           "Surface_DO_mg_L",
           "Surface_pH",
           "Bottom_Temp_C",
           "Bottom_Sal_psu",
           "Bottom_DO_mg_L",
           "Bottom_pH",
           "Secchi_Avg_Depth","TSS (mg/L)",
           "Enterococcus (MPN)","SS (ml/L)","Chl-a (µg/L)",
           "Turbidity (NTU)","TKN (mg/L)","NOx (mg/L)","PO4 (mg/L)","Surface BOD (mg/L)",
           "Bottom BOD (mg/L)","Surface TA (µM)",
           "Bottom TA (µM)","Surface DIC (µM)","Bottom DIC (µM)")) %>% 
  mutate_if(is.character, as.numeric) %>% 
  describe(fast=TRUE) %>% 
  select(!vars)

write.csv(Report_2_summary_table,"Report_2_summary_table.csv") 

#####register google key####
register_google(key = "AIzaSyD8X7CdMZBOCTufTLqtF7aB2pjJnIuVLg8") #my personal key linked to my credit card, please do not share

#####generate map of puerto rico####
pr <- get_map('puerto rico', zoom = 8, maptype = 'satellite')

#####plot sampled values at their respective site####
#Bottom HydroLab data#
Bottom_Temperature_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_Temp_C), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(b) Bottom Temperature (°C)")+
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

Bottom_Salinity_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_Sal_psu), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(d) Bottom Salinity (psu)")+
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

Bottom_DO_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_DO_mg_L), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(f) Bottom DO (mg/L)")+
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

Bottom_pH_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_pH), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(h) Bottom pH")+
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
Bottom_HydroLab_plot=(Bottom_Temperature_plot+Bottom_Salinity_plot)/(Bottom_DO_plot+Bottom_pH_plot)
#shows the plot
Bottom_HydroLab_plot
#saves the plot as a pdf
ggsave("Bottom_HydroLab_plot.pdf",Bottom_HydroLab_plot,height=6.5,width=14)

#Surface HydroLab plots

#####plot sampled values at their respective site####
Surface_Temperature_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_Temp_C), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(a) Surface Temperature (°C)")+
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

Surface_Salinity_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_Sal_psu), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(c) Surface Salinity (psu)")+
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

Surface_DO_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_DO_mg_L), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(e) Surface DO (mg/L)")+
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

Surface_pH_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_pH), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(g) Surface pH")+
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
Surface_HydroLab_plot=(Surface_Temperature_plot+Surface_Salinity_plot)/(Surface_DO_plot+Surface_pH_plot)
#shows the plot
Surface_HydroLab_plot
#saves the plot as a pdf
ggsave("Surface_HydroLab_plot.pdf",Surface_HydroLab_plot,height=6.5,width=14)

#Create combined Surface+Bottom figure
HydroLab_plot=(Surface_Temperature_plot+Bottom_Temperature_plot)/(Surface_Salinity_plot+Bottom_Salinity_plot)/(Surface_DO_plot+Bottom_DO_plot)/(Surface_pH_plot+Bottom_pH_plot)
#Shows the plot
HydroLab_plot
#saves the plot as a pdf
ggsave("HydroLab_plot.pdf",HydroLab_plot,height=13,width=14)


#Bottom Chemistry plots

TSS_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`TSS (mg/L)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(a) Total Suspended Solids (mg/L)")+
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

Ent_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`Enterococcus (MPN)`), size= 4)+
  scale_color_viridis(discrete=TRUE)+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(b) Enterococcus (MPN)")+
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

SS_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`SS (ml/L)`), size= 4)+
  scale_color_viridis(discrete=TRUE)+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(c) Suspended Solids (ml/L)")+
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

Chl_a_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`Chl-a (µg/L)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(d) Chlorophyll-a (µg/L) [LOD = 0.01 µg/L]")+
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

Turbidity_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`Turbidity (NTU)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(e) Turbidity (NTU)")+
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

PO4_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`PO4 (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(f) Phosphate (mg/L) [LOD = 0.001 mg/L]")+
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

NOx_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`NOx (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(g) NOx (mg/L) [LOD = 0.001 mg/L]")+
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

TKN_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`TKN (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(h) TKN (mg/L) [LOD = 0.2 mg/L]")+
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

#Create combined Surface+Bottom figure
Chem_plot=(TSS_plot+Ent_plot)/(SS_plot+Chl_a_plot)/(Turbidity_plot+PO4_plot)/(NOx_plot+TKN_plot)
#Shows the plot
Chem_plot
#saves the plot as a pdf
ggsave("Chem_plot.pdf",Chem_plot,height=13,width=14)

#BOD, TA and DIC: Surface + Bottom

Surface_BOD_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Surface BOD (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(a) Surface BOD (mg/L) [LOD = 0.2 mg/L]")+
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

Bottom_BOD_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Bottom BOD (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(b) Bottom BOD (mg/L) [LOD = 0.2 mg/L]")+
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

Surface_TA_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Surface TA (µM)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(c) Surface Total Alkalinity (µM)")+
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

Bottom_TA_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Bottom TA (µM)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(d) Bottom Total Alkalinity (µM)")+
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

Surface_DIC_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`Surface DIC (µM)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(e) Surface DIC (µM)")+
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

Bottom_DIC_plot=
  ggmap(pr)+
  geom_point(Report_2_data, mapping=aes(x= Longitude, y = Latitude, color=`Bottom DIC (µM)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(f) Bottom DIC (µM)")+
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


#Create combined Surface+Bottom figure
SB_Chem_plot=(Surface_BOD_plot+Bottom_BOD_plot)/(Surface_TA_plot+Bottom_TA_plot)/(Surface_DIC_plot+Bottom_DIC_plot)
#Shows the plot
SB_Chem_plot
#saves the plot as a pdf
ggsave("Surface_and_Bottom_Chem_plot.pdf",SB_Chem_plot,height=8,width=12)
