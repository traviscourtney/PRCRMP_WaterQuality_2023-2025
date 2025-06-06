#####load required libraries####
library(tidyverse)
library(readxl)

options(scipen=999)

data=read_xlsx("CCRI_PRCRMP_Chemistry_Data_all_5-27-2025.xlsx")

field_replicate_RPD= data %>% 
  group_by(Latitude,Longitude,Date) %>% 
  mutate(Surface_Temp_RPD=(((as.numeric(Surface_Temp_C[1])-as.numeric(Surface_Temp_C[2]))/((as.numeric(Surface_Temp_C[1])+as.numeric(Surface_Temp_C[2])))))*100) %>% 
  mutate(Surface_Sal_psu_RPD=(((as.numeric(Surface_Sal_psu[1])-as.numeric(Surface_Sal_psu[2]))/((as.numeric(Surface_Sal_psu[1])+as.numeric(Surface_Sal_psu[2])))))*100) %>% 
  mutate(Surface_DO_mg_L_RPD=(((as.numeric(Surface_DO_mg_L[1])-as.numeric(Surface_DO_mg_L[2]))/((as.numeric(Surface_DO_mg_L[1])+as.numeric(Surface_DO_mg_L[2])))))*100) %>% 
  mutate(Surface_pH_RPD=(((as.numeric(Surface_pH[1])-as.numeric(Surface_pH[2]))/((as.numeric(Surface_pH[1])+as.numeric(Surface_pH[2])))))*100) %>% 
  mutate(Bottom_Temp_RPD=(((as.numeric(Bottom_Temp_C[1])-as.numeric(Bottom_Temp_C[2]))/((as.numeric(Bottom_Temp_C[1])+as.numeric(Bottom_Temp_C[2])))))*100) %>% 
  mutate(Bottom_Sal_psu_RPD=(((as.numeric(Bottom_Sal_psu[1])-as.numeric(Bottom_Sal_psu[2]))/((as.numeric(Bottom_Sal_psu[1])+as.numeric(Bottom_Sal_psu[2])))))*100) %>% 
  mutate(Bottom_DO_mg_L_RPD=(((as.numeric(Bottom_DO_mg_L[1])-as.numeric(Bottom_DO_mg_L[2]))/((as.numeric(Bottom_DO_mg_L[1])+as.numeric(Bottom_DO_mg_L[2])))))*100) %>%  
  mutate(Bottom_pH_RPD=(((as.numeric(Bottom_pH[1])-as.numeric(Bottom_pH[2]))/((as.numeric(Bottom_pH[1])+as.numeric(Bottom_pH[2])))))*100) %>% 
  mutate(Secchi_Depth_m_RPD=(((as.numeric(Secchi_Depth_m[1])-as.numeric(Secchi_Depth_m[2]))/((as.numeric(Secchi_Depth_m[1])+as.numeric(Secchi_Depth_m[2])))))*100) %>% 
  mutate(Bottom_TSS_mg_L_RPD=(((as.numeric(Bottom_TSS_mg_L[1])-as.numeric(Bottom_TSS_mg_L[2]))/((as.numeric(Bottom_TSS_mg_L[1])+as.numeric(Bottom_TSS_mg_L[2])))))*100) %>% 
  mutate(Bottom_Enterococcus_LOD_MPN_100mL_RPD=(((as.numeric(Bottom_Enterococcus_LOD_MPN_100mL[1])-as.numeric(Bottom_Enterococcus_LOD_MPN_100mL[2]))/((as.numeric(Bottom_Enterococcus_LOD_MPN_100mL[1])+as.numeric(Bottom_Enterococcus_MPN_100mL[2])))))*100) %>% 
  mutate(Bottom_SS_ml_L_RPD=(((as.numeric(Bottom_SS_ml_L[1])-as.numeric(Bottom_SS_ml_L[2]))/((as.numeric(Bottom_SS_ml_L[1])+as.numeric(Bottom_SS_ml_L[2])))))*100) %>% 
  mutate(Surface_BOD_mg_L_RPD=(((as.numeric(Surface_BOD_mg_L[1])-as.numeric(Surface_BOD_mg_L[2]))/((as.numeric(Surface_BOD_mg_L[1])+as.numeric(Surface_BOD_mg_L[2])))))*100) %>% 
  mutate(Bottom_BOD_mg_L_RPD=(((as.numeric(Bottom_BOD_mg_L[1])-as.numeric(Bottom_BOD_mg_L[2]))/((as.numeric(Bottom_BOD_mg_L[1])+as.numeric(Bottom_BOD_mg_L[2])))))*100) %>% 
  mutate(`Bottom_Chl-a_ug_L_RPD`=(((as.numeric(`Bottom_Chl-a_ug_L`[1])-as.numeric(`Bottom_Chl-a_ug_L`[2]))/((as.numeric(`Bottom_Chl-a_ug_L`[1])+as.numeric(`Bottom_Chl-a_ug_L`[2])))))*100) %>% 
  mutate(Bottom_Turbidity_NTU_RPD=(((as.numeric(Bottom_Turbidity_NTU[1])-as.numeric(Bottom_Turbidity_NTU[2]))/((as.numeric(Bottom_Turbidity_NTU[1])+as.numeric(Bottom_Turbidity_NTU[2])))))*100) %>% 
  mutate(Surface_DIC_umol_kg_RPD=(((as.numeric(Surface_DIC_umol_kg[1])-as.numeric(Surface_DIC_umol_kg[2]))/((as.numeric(Surface_DIC_umol_kg[1])+as.numeric(Surface_DIC_umol_kg[2])))))*100) %>% 
  mutate(Bottom_DIC_umol_kg_RPD=(((as.numeric(Bottom_DIC_umol_kg[1])-as.numeric(Bottom_DIC_umol_kg[2]))/((as.numeric(Bottom_DIC_umol_kg[1])+as.numeric(Bottom_DIC_umol_kg[2])))))*100) %>% 
  mutate(Surface_TA_umol_kg_RPD=(((as.numeric(Surface_TA_umol_kg[1])-as.numeric(Surface_TA_umol_kg[2]))/((as.numeric(Surface_TA_umol_kg[1])+as.numeric(Surface_TA_umol_kg[2])))))*100) %>% 
  mutate(Bottom_TA_umol_kg_RPD=(((as.numeric(Bottom_TA_umol_kg[1])-as.numeric(Bottom_TA_umol_kg[2]))/((as.numeric(Bottom_TA_umol_kg[1])+as.numeric(Bottom_TA_umol_kg[2])))))*100) %>%  
  mutate(Bottom_NOx_LOD_mg_L_RPD=(((as.numeric(Bottom_NOx_LOD_mg_L[1])-as.numeric(Bottom_NOx_LOD_mg_L[2]))/((as.numeric(Bottom_NOx_LOD_mg_L[1])+as.numeric(Bottom_NOx_LOD_mg_L[2])))))*100) %>% 
  mutate(Bottom_PO4_LOD_mg_L_RPD=(((as.numeric(Bottom_PO4_LOD_mg_L[1])-as.numeric(Bottom_PO4_LOD_mg_L[2]))/((as.numeric(Bottom_PO4_LOD_mg_L[1])+as.numeric(Bottom_PO4_LOD_mg_L[2])))))*100) %>% 
  mutate(Bottom_TKN_LOD_mg_L_RPD=(((as.numeric(Bottom_TKN_LOD_mg_L[1])-as.numeric(Bottom_TKN_LOD_mg_L[2]))/((as.numeric(Bottom_TKN_LOD_mg_L[1])+as.numeric(Bottom_TKN_LOD_mg_L[2])))))*100) %>% 
  filter(str_detect(Site_Code,"-R")) %>% 
  ungroup() %>% 
  summarise(Temp_RPD_mean=mean(cbind(Surface_Temp_RPD,Bottom_Temp_RPD),na.rm=TRUE),
            Sal_psu_RPD_mean=mean(cbind(Surface_Sal_psu_RPD,Bottom_Sal_psu_RPD),na.rm=TRUE),
            DO_mg_L_RPD_mean=mean(cbind(Surface_DO_mg_L_RPD,Bottom_DO_mg_L_RPD),na.rm=TRUE),
            pH_RPD_mean=mean(cbind(Surface_pH_RPD,Bottom_pH_RPD),na.rm=TRUE),
            Secchi_Depth_m_RPD_mean=mean(Secchi_Depth_m_RPD,na.rm=TRUE),
            Bottom_TSS_mg_L_RPD_mean=mean(Bottom_TSS_mg_L_RPD,na.rm=TRUE),
            Bottom_Enterococcus_LOD_MPN_100mL_RPD_mean=mean(Bottom_Enterococcus_LOD_MPN_100mL_RPD,na.rm=TRUE),
            Bottom_SS_ml_L_RPD_mean=mean(Bottom_SS_ml_L_RPD,na.rm=TRUE),
            BOD_mg_L_RPD_mean=mean(cbind(Surface_BOD_mg_L_RPD,Bottom_BOD_mg_L_RPD),na.rm=TRUE),
            `Bottom_Chl-a_ug_L_RPD_mean`=mean(`Bottom_Chl-a_ug_L_RPD`,na.rm=TRUE),
            Bottom_Turbidity_NTU_RPD_mean=mean(Bottom_Turbidity_NTU_RPD,na.rm=TRUE),
            DIC_umol_kg_RPD_mean=mean(cbind(Surface_DIC_umol_kg_RPD,Bottom_DIC_umol_kg_RPD),na.rm=TRUE),
            TA_umol_kg_RPD_mean=mean(cbind(Surface_TA_umol_kg_RPD,Bottom_TA_umol_kg_RPD),na.rm=TRUE),
            Bottom_NOx_LOD_mg_L_RPD_mean=mean(Bottom_NOx_LOD_mg_L_RPD,na.rm=TRUE),
            Bottom_PO4_LOD_mg_L_RPD_mean=mean(Bottom_PO4_LOD_mg_L_RPD,na.rm=TRUE),
            Bottom_TKN_LOD_mg_L_RPD_mean=mean(Bottom_TKN_LOD_mg_L_RPD,na.rm=TRUE))%>% 
  pivot_longer(cols = everything(), names_to = 'Parameter', values_to = 'Mean RPD') %>% 
  mutate(across(where(is.numeric), round, 2))

View(field_replicate_RPD)
