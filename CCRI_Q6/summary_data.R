#####load required libraries####
library(tidyverse)
library(tigris)
library(viridis)
library(patchwork)
library(naniar)
library(readxl)
library(ggplot2)
library(psych)
library(hablar)
library(ggthemes)
library(ggrepel)

####import Report 3 data####

Report_6_data=read_csv("Quarter_6_Data.csv")

#####convert all data to numeric to create and export summary table####
#Report_6_data$Surface_Temp_C=as.numeric(Report_6_data$Surface_Temp_C)
#Report_6_data$Surface_Sal_psu=as.numeric(Report_6_data$Surface_Sal_psu)
#Report_6_data$Surface_DO_mg_L=as.numeric(Report_6_data$Surface_DO_mg_L)
#Report_6_data$Surface_pH=as.numeric(Report_6_data$Surface_pH)
#Report_6_data$Bottom_Temp_C=as.numeric(Report_6_data$Bottom_Temp_C)
#Report_6_data$Bottom_Sal_psu=as.numeric(Report_6_data$Bottom_Sal_psu)
#Report_6_data$Bottom_DO_mg_L=as.numeric(Report_6_data$Bottom_DO_mg_L)
#Report_6_data$Bottom_pH=as.numeric(Report_6_data$Bottom_pH)
#Report_6_data$`Chl-a (µg/L)`=as.numeric(Report_6_data$`Chl-a (µg/L)`)
#Report_6_data$`Turbidity (NTU)`=as.numeric(Report_6_data$`Turbidity (NTU)`)
#Report_6_data$`Surface DIC (µmol/kg)`=as.numeric(Report_6_data$`Surface DIC (µmol/kg)`)
#Report_6_data$`Bottom DIC (µmol/kg)`=as.numeric(Report_6_data$`Bottom DIC (µmol/kg)`)


Report_6_summary_table=Report_6_data %>% 
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
           "Bottom BOD (mg/L)","Surface TA (µmol/kg)",
           "Bottom TA (µmol/kg)","Surface DIC (µmol/kg)","Bottom DIC (µmol/kg)")) %>% 
  mutate_if(is.character, as.numeric) %>% 
  describe(fast=TRUE) %>% 
  select(!vars)

write.csv(Report_6_summary_table,"Report_6_summary_table.csv") 

#####generate map of puerto rico####
options(tigris_class = "sf")
PRmap=counties(state = "PR", cb = TRUE)

PRCRMP_map=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude), size= 4,alpha=0.9)+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("PRCRMP Monitoring Stations")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

site_code_map=as.data.frame(cbind(Longitude=Report_6_data$Longitude,Latitude=Report_6_data$Latitude,Site_Code=substr(Report_6_data$Site_Code,1,4)))%>% 
  group_by(Site_Code) %>% 
  summarize(Longitude=mean(as.numeric(Longitude)),Latitude=mean(as.numeric(Latitude)))

PRCRMP_map=
ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude), size= 4,alpha=0.9)+
  geom_text_repel(site_code_map, mapping=aes(x= Longitude, y = Latitude,label=Site_Code))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("PRCRMP Monitoring Stations")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(0, "lines"))

ggsave('Figure_1.jpg', PRCRMP_map, width = 16, height = 6, dpi = 100)

#####plot sampled values at their respective site####
#Bottom Eureka data#
Bottom_Temperature_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_Temp_C), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_Temp_C,Report_6_data$Surface_Temp_C),max(Report_6_data$Bottom_Temp_C,Report_6_data$Surface_Temp_C)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(b) Bottom Temperature (°C)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Bottom_Salinity_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_Sal_psu), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_Sal_psu,Report_6_data$Surface_Sal_psu),max(Report_6_data$Bottom_Sal_psu,Report_6_data$Surface_Sal_psu)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(d) Bottom Salinity (psu)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Bottom_DO_plot=
  ggplot()+   
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_DO_mg_L), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_DO_mg_L,Report_6_data$Surface_DO_mg_L),max(Report_6_data$Bottom_DO_mg_L,Report_6_data$Surface_DO_mg_L)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(f) Bottom DO (mg/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Bottom_pH_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Bottom_pH), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_pH,Report_6_data$Surface_pH),max(Report_6_data$Bottom_pH,Report_6_data$Surface_pH)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(h) Bottom pH")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

#combine into a multi-panel figure where + makes things side by side and / makes things top to bottom
Bottom_Eureka_plot=(Bottom_Temperature_plot+Bottom_Salinity_plot)/(Bottom_DO_plot+Bottom_pH_plot)
#shows the plot
Bottom_Eureka_plot
#saves the plot as a pdf
#ggsave("Bottom_Eureka_plot.jpg",Bottom_Eureka_plot,height=6.5,width=14)

#Surface Eureka plots

#####plot sampled values at their respective site####
Surface_Temperature_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_Temp_C), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_Temp_C,Report_6_data$Surface_Temp_C),max(Report_6_data$Bottom_Temp_C,Report_6_data$Surface_Temp_C)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(a) Surface Temperature (°C)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Surface_Salinity_plot=
  ggplot()+ 
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_Sal_psu), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_Sal_psu,Report_6_data$Surface_Sal_psu),max(Report_6_data$Bottom_Sal_psu,Report_6_data$Surface_Sal_psu)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(c) Surface Salinity (psu)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Surface_DO_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_DO_mg_L), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_DO_mg_L,Report_6_data$Surface_DO_mg_L),max(Report_6_data$Bottom_DO_mg_L,Report_6_data$Surface_DO_mg_L)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(e) Surface DO (mg/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Surface_pH_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=Surface_pH), size= 4)+
  scale_color_viridis(limits=c(min(Report_6_data$Bottom_pH,Report_6_data$Surface_pH),max(Report_6_data$Bottom_pH,Report_6_data$Surface_pH)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(g) Surface pH")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

#combine into a multi-panel figure where + makes things side by side and / makes things top to bottom
Surface_Eureka_plot=(Surface_Temperature_plot+Surface_Salinity_plot)/(Surface_DO_plot+Surface_pH_plot)
#shows the plot
Surface_Eureka_plot
#saves the plot as a pdf
#ggsave("Surface_Eureka_plot.jpg",Surface_Eureka_plot,height=6.5,width=14)

#Create combined Surface+Bottom figure
Eureka_plot=(Surface_Temperature_plot+Bottom_Temperature_plot)/(Surface_Salinity_plot+Bottom_Salinity_plot)/(Surface_DO_plot+Bottom_DO_plot)/(Surface_pH_plot+Bottom_pH_plot)
#Shows the plot
Eureka_plot
#saves the plot as a pdf
ggsave("Figure_2.jpg",Eureka_plot,height=8,width=10, dpi = 100)


#Bottom Chemistry plots

TSS_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=`TSS (mg/L)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(a) Total Suspended Solids (mg/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Ent_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Enterococcus (MPN)`)), size= 4)+
  scale_color_viridis()+
  #scale_color_viridis(discrete=TRUE)+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(b) Enterococcus (MPN) [Gray = Below LOD]")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

SS_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=`SS (ml/L)`), size= 4)+
  scale_color_viridis(discrete=TRUE)+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(c) Settleable Solids (ml/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Chl_a_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=`Chl-a (µg/L)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(d) Chl-a (µg/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Turbidity_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=`Turbidity (NTU)`), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(e) Turbidity (NTU)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

PO4_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`PO4 LOD (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(f) Phosphate (mg/L) [Gray = Below LOD]")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

NOx_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`NOx LOD (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(g) NOx (mg/L) [Gray = Below LOD]")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

TKN_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`TKN LOD (mg/L)`)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(h) TKN (mg/L) [Gray = Below LOD]")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

#Create combined Surface+Bottom figure
Chem_plot=(TSS_plot+Ent_plot)/(SS_plot+Chl_a_plot)/(Turbidity_plot+PO4_plot)/(NOx_plot+TKN_plot)
#Shows the plot
Chem_plot
#saves the plot as a pdf
ggsave("Figure_3.jpg",Chem_plot,height=8,width=10, dpi = 100)

#BOD, TA and DIC: Surface + Bottom

Surface_BOD_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Surface BOD (mg/L)`)), size= 4)+
  scale_color_viridis(limits=c(min(as.numeric(Report_6_data$`Surface BOD (mg/L)`),as.numeric(Report_6_data$`Bottom BOD (mg/L)`)),max(as.numeric(Report_6_data$`Surface BOD (mg/L)`),as.numeric(Report_6_data$`Bottom BOD (mg/L)`))))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(a) Surface BOD (mg/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Bottom_BOD_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Bottom BOD (mg/L)`)), size= 4)+
  scale_color_viridis(limits=c(min(as.numeric(Report_6_data$`Surface BOD (mg/L)`),as.numeric(Report_6_data$`Bottom BOD (mg/L)`)),max(as.numeric(Report_6_data$`Surface BOD (mg/L)`),as.numeric(Report_6_data$`Bottom BOD (mg/L)`))))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(b) Bottom BOD (mg/L)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Surface_TA_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Surface TA (µmol/kg)`)), size= 4)+
  scale_color_viridis(limits=c(min(as.numeric(Report_6_data$`Surface TA (µmol/kg)`),as.numeric(Report_6_data$`Bottom TA (µmol/kg)`),na.rm=TRUE),max(as.numeric(Report_6_data$`Surface TA (µmol/kg)`),as.numeric(Report_6_data$`Bottom TA (µmol/kg)`),na.rm=TRUE)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(c) Surface TA (µmol/kg)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Bottom_TA_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(`Bottom TA (µmol/kg)`)), size= 4)+
  scale_color_viridis(limits=c(min(as.numeric(Report_6_data$`Surface TA (µmol/kg)`),as.numeric(Report_6_data$`Bottom TA (µmol/kg)`),na.rm=TRUE),max(as.numeric(Report_6_data$`Surface TA (µmol/kg)`),as.numeric(Report_6_data$`Bottom TA (µmol/kg)`),na.rm=TRUE)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(d) Bottom TA (µmol/kg)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Surface_DIC_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=`Surface DIC (µmol/kg)`), size= 4)+
  scale_color_viridis(limits=c(min(as.numeric(Report_6_data$`Surface DIC (µmol/kg)`),as.numeric(Report_6_data$`Bottom DIC (µmol/kg)`),na.rm=TRUE),max(as.numeric(Report_6_data$`Surface DIC (µmol/kg)`),as.numeric(Report_6_data$`Bottom DIC (µmol/kg)`),na.rm=TRUE)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(e) Surface DIC (µmol/kg)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

Bottom_DIC_plot=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(Report_6_data, mapping=aes(x= Longitude, y = Latitude, color=`Bottom DIC (µmol/kg)`), size= 4)+
  scale_color_viridis(limits=c(min(as.numeric(Report_6_data$`Surface DIC (µmol/kg)`),as.numeric(Report_6_data$`Bottom DIC (µmol/kg)`),na.rm=TRUE),max(as.numeric(Report_6_data$`Surface DIC (µmol/kg)`),as.numeric(Report_6_data$`Bottom DIC (µmol/kg)`),na.rm=TRUE)))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  ggtitle("(f) Bottom DIC (µmol/kg)")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=14,family="sans"),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

#Create combined Surface+Bottom figure
SB_Chem_plot=(Surface_BOD_plot+Bottom_BOD_plot)/(Surface_TA_plot+Bottom_TA_plot)/(Surface_DIC_plot+Bottom_DIC_plot)
#Shows the plot
SB_Chem_plot
#saves the plot as a pdf
ggsave("Figure_4.jpg",SB_Chem_plot,height=6,width=10, dpi = 100)
