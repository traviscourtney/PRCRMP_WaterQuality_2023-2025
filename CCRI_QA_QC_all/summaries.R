#####load required libraries####
library(tidyverse)
library(readxl)
library(tigris)
library(ggthemes)
library(ggspatial)
library(psych)

####import data####

data=read_xlsx("CCRI_PRCRMP_Chemistry_Data_all_5-29-2025.xlsx")

#####convert all data to numeric to create and export summary table####
summary_table=data %>% 
  select(c("Surface_Temp_C",
           "Surface_Sal_psu",
           "Surface_DO_mg_L",
           "Surface_pH",
           "Bottom_Temp_C",
           "Bottom_Sal_psu",
           "Bottom_DO_mg_L",
           "Bottom_pH",
           "Secchi_Depth_m",
           "Surface_DIC_umol_kg",
           "Bottom_DIC_umol_kg",
           "Surface_TA_umol_kg",   
           "Bottom_TA_umol_kg",
           "Surface_BOD_mg_L",
           "Bottom_BOD_mg_L", 
           "Bottom_SS_ml_L",  
           "Bottom_TSS_mg_L", 
           "Bottom_Turbidity_NTU",
           "Bottom_Chl-a_ug_L",                
           "Bottom_PO4_mg_L", 
           "Bottom_NOx_mg_L",              
           "Bottom_TKN_mg_L",           
           "Bottom_Enterococcus_MPN_100mL")) %>% 
  mutate_if(is.character, as.numeric) %>% 
  describe(fast=TRUE) %>% 
  select(!c(vars,median,range,skew,kurtosis,se)) %>% 
  mutate(across(2:5, round, 2))

write.csv(summary_table,"summary_table.csv") 

#####generate map of puerto rico####
options(tigris_class = "sf")
PRmap=counties(state = "PR", cb = TRUE)

sitemapdata=data %>% 
  filter(Sampling_Cycle==8) %>% 
  filter(!str_detect(Site_Code,"-R"))

PRCRMP_map=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  annotation_scale(location="br")+
  geom_point(sitemapdata, mapping=aes(x= Longitude, y = Latitude,fill=as.factor(Depth),shape=as.factor(Depth)),alpha=1,size= 3)+
  scale_fill_manual(name = 'Site Depth (m)',
                    values = c('5' = '#ffffcc', '10' = '#a1dab4', '15' = '#41b6c4','20' = '#2c7fb8', '30' = '#253494'))+ 
  scale_shape_manual(name = 'Site Depth (m)',
                     values = c('5' = 21, '10' = 22, '15' = 23,'20' = 24, '30' = 25)) + 
  scale_y_continuous(limits = c(17.85,18.55), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=12,family="sans"),
        legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank())

ggsave('Figure_1.jpg', PRCRMP_map, width = 7, height = 3, dpi = 300)