#####load required libraries####
library(tidyverse)
library(readxl)
library(tigris)
library(ggthemes)

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

PRCRMP_map=
  ggplot()+
  geom_sf(data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data, mapping=aes(x= Longitude, y = Latitude), colour='#9ecae1', size= 3, alpha=0.2)+
  scale_y_continuous(limits = c(17.85,18.55), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-67.6,-65), expand = c(0, 0)) +
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

ggsave('Figure_1.jpg', PRCRMP_map, width = 7, height = 3, dpi = 300)