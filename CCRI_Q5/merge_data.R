##### Load required libraries #####
library(plyr)
library(tidyverse)
library(readxl)
library(hablar)

#Turn off scientific notation
options(scipen=999)

##### Import Quarter_5_log and convert bottle ID to numbers#####
field_log <- read_excel("Field Data Log Quarter 5 .xlsx")
field_log$TSS=as.numeric(field_log$Total_Suspended_Solids)
field_log$Enterococcus=as.numeric(field_log$Enterococcus)
field_log$SS=as.numeric(field_log$Settleable_Solids)
field_log$Surface_BOD=as.numeric(field_log$Surface_BOD)
field_log$Bottom_BOD=as.numeric(field_log$Bottom_BOD)
field_log$Chl_a=as.numeric(field_log$Chl_a)
field_log$Turbidity=as.numeric(field_log$Turbidity_NTU)

##### Import Chemistry Lab Data and Merge with Quarter_5_Tables #####
TSS <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("TSS", ignore_case = TRUE))) %>% 
  mutate(TSS=round(as.numeric(str_remove(`Sample ID`,"TSS_"),digits=1))) %>% 
  mutate(`Result (mg/L)`=round(`Result (mg/L)`,digits=2)) %>% 
  select(TSS,`Result (mg/L)`) %>% 
  rename(`TSS (mg/L)`=`Result (mg/L)`)

Quarter_5_TSS=merge(x = field_log, y = TSS, by.x = "Total_Suspended_Solids", by.y = "TSS", all.x = TRUE)

enterococcus <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="Ent") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("_", ignore_case = TRUE))) %>% 
  mutate(enterococcus=str_remove(`Sample ID`,"Ent_"),digits=2) %>% 
  select(enterococcus,`MPN by 10 (Dilution factor)`) %>% 
  rename(`Enterococcus (MPN)`=`MPN by 10 (Dilution factor)`)

Quarter_5_TSS_Ent=merge(x = Quarter_5_TSS, y = enterococcus, by.x = "Enterococcus", by.y = "enterococcus", all.x = TRUE)

SS <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="SS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("SS", ignore_case = TRUE))) %>% 
  mutate(SS=round(as.numeric(str_remove(`Sample ID`,"SS_"),digits=2))) %>% 
  select(SS,`Results ml/L`) %>% 
  rename(`SS (ml/L)`=`Results ml/L`)

Quarter_5_TSS_Ent_SS=merge(x = Quarter_5_TSS_Ent, y = SS, by.x = "Settleable_Solids", by.y = "SS", all.x = TRUE)

BOD_Surface <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="BOD") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(!str_detect(`Sample ID`,  regex("GGA", ignore_case = TRUE))) %>% 
  filter(str_detect(`Sample ID`,  regex("surface", ignore_case = TRUE))) %>% 
  mutate(BOD=str_remove_all(str_remove_all(str_remove_all(str_remove_all(str_remove_all(`Sample ID`,"BOD_"),"surface"),"_"),"Surface")," ")) %>%
  select(BOD,`BOD in mg/L`) %>% 
  rename(`Surface BOD (mg/L)`=`BOD in mg/L`)

Quarter_5_TSS_Ent_SS_BODs=merge(x = Quarter_5_TSS_Ent_SS, y = BOD_Surface, by.x = "Surface_BOD", by.y = "BOD", all.x = TRUE)

BOD_Bottom <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="BOD") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(!str_detect(`Sample ID`,  regex("GGA", ignore_case = TRUE))) %>% 
  filter(str_detect(`Sample ID`,  regex("bottom", ignore_case = TRUE))) %>% 
  mutate(BOD=str_remove_all(str_remove_all(str_remove_all(str_remove_all(`Sample ID`,"BOD_"),"bottom"),"_")," ")) %>%
  select(BOD,`BOD in mg/L`) %>% 
  rename(`Bottom BOD (mg/L)`=`BOD in mg/L`)

Quarter_5_TSS_Ent_SS_BOD=merge(x = Quarter_5_TSS_Ent_SS_BODs, y = BOD_Bottom, by.x = "Bottom_BOD", by.y = "BOD", all.x = TRUE)

Chla <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(str_detect(`Sample ID`,  regex("Chl_a", ignore_case = TRUE))) %>% 
  mutate(Chla=round(as.numeric(str_remove(`Sample ID`,"Chl_a_"),digits=2))) %>% 
  mutate(`Chl-a (µg/L)`=round(`Chl-a (µg/L)`,digits=2)) %>% 
  select(Chla,`Chl-a (µg/L)`)

Quarter_5_TSS_Ent_SS_BOD_Chla=merge(x = Quarter_5_TSS_Ent_SS_BOD, y = Chla, by.x = "Chl_a", by.y = "Chla", all.x = TRUE)

Turbidity <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="Turbidity") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Turb_", ignore_case = TRUE))) %>% 
  mutate(Turb=round(as.numeric(str_remove(`Sample ID`,"Turb_"),digits=2))) %>% 
  mutate(NTU_round=round_any(`NTU`,0.05)) %>% 
  select(Turb,NTU_round) %>% 
  rename(`Turbidity (NTU)`=`NTU_round`)

Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity=merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla, y = Turbidity, by.x = "Turbidity_NTU", by.y = "Turb", all.x = TRUE)

sample_salinity=read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample Name`,`Salinity`) %>% 
  rename(Bottle=`Sample Name`) %>% 
  distinct()

DIC_temperature=read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="DIC Temperatures") %>% 
  filter(`QA Flag`=="A") %>%
  select(`Sample`,`Temperature (C)`) %>% 
  rename(Bottle=`Sample`)

DIC_temperature_salinity=merge(DIC_temperature,sample_salinity,by="Bottle")

DIC_temperature_salinity$Bottle=round(as.numeric(DIC_temperature_salinity$Bottle))
DIC_temperature_salinity$`Temperature (C)`=as.numeric(DIC_temperature_salinity$`Temperature (C)`)
DIC_temperature_salinity$Salinity=as.numeric(DIC_temperature_salinity$Salinity)

DIC_temperature_salinity$`Lab Density (kg/L)`=(999.842594+0.06793952*DIC_temperature_salinity$`Temperature (C)`-0.00909529*DIC_temperature_salinity$`Temperature (C)`^2+0.0001001685*DIC_temperature_salinity$`Temperature (C)`^3-0.000001120083*DIC_temperature_salinity$`Temperature (C)`^4+0.000000006536332*DIC_temperature_salinity$`Temperature (C)`^5+(0.824493-0.0040899*DIC_temperature_salinity$`Temperature (C)`+0.000076438*DIC_temperature_salinity$`Temperature (C)`^2-0.00000082467*DIC_temperature_salinity$`Temperature (C)`^3+0.0000000053875*DIC_temperature_salinity$`Temperature (C)`^4)*DIC_temperature_salinity$Salinity+ (-0.00572466 + 0.00010227*DIC_temperature_salinity$`Temperature (C)` - 0.0000016546*DIC_temperature_salinity$`Temperature (C)`^2)*DIC_temperature_salinity$Salinity^1.5 +
                0.00048314*DIC_temperature_salinity$Salinity^2)/1000

DIC_s <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA`=="A") %>% 
  select(`Sample Name`,`DIC (uM)`) %>% 
  rename(Bottle=`Sample Name`) %>% 
  rename(`Surface DIC (µM)`=`DIC (uM)`) %>% 
  distinct()

DIC_s$Bottle=round(as.numeric(DIC_s$Bottle))
DIC_s$`Surface DIC (µM)`=round(as.numeric(DIC_s$`Surface DIC (µM)`))
DIC_s=DIC_s[complete.cases(DIC_s),]

DIC_s_density=merge(DIC_s,DIC_temperature_salinity,by="Bottle")

DIC_s_density$`Surface DIC (µmol/kg)`=round(DIC_s_density$`Surface DIC (µM)`/DIC_s_density$`Lab Density (kg/L)`,0)

DIC_s_umolkg=DIC_s_density %>% 
  select(`Bottle`,`Surface DIC (µmol/kg)`)

Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DICs=merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity, y = DIC_s_umolkg, by.x = "Surface_TA_DIC", by.y = "Bottle", all.x = TRUE)

DIC_b <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA`=="A") %>% 
  select(`Sample Name`,`DIC (uM)`) %>% 
  rename(Bottle=`Sample Name`) %>% 
  rename(`Bottom DIC (µM)`=`DIC (uM)`) %>% 
  distinct()

DIC_b$Bottle=round(as.numeric(DIC_b$Bottle))
DIC_b$`Bottom DIC (µM)`=round(as.numeric(DIC_b$`Bottom DIC (µM)`))
DIC_b=DIC_b[complete.cases(DIC_b),]

DIC_b_density=merge(DIC_b,DIC_temperature_salinity,by="Bottle")

DIC_b_density$`Bottom DIC (µmol/kg)`=round(DIC_b_density$`Bottom DIC (µM)`/DIC_b_density$`Lab Density (kg/L)`,0)

DIC_b_umolkg=DIC_b_density %>% 
  select(`Bottle`,`Bottom DIC (µmol/kg)`)

Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC=merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DICs, y = DIC_b_umolkg, by.x = "Bottom_TA_DIC", by.y = "Bottle", all.x = TRUE)

TA_s <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample Name`,`TA_umolkg_corr`) %>% 
  rename(Bottle=`Sample Name`) %>% 
  rename(`Surface TA (µmol/kg)`=`TA_umolkg_corr`)

TA_s$Bottle=round(as.numeric(TA_s$Bottle))
TA_s$`Surface TA (µmol/kg)`=round(as.numeric(TA_s$`Surface TA (µmol/kg)`))

Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TAs=merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC, y = TA_s, by.x = "Surface_TA_DIC", by.y = "Bottle", all.x = TRUE)

TA_b <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample Name`,`TA_umolkg_corr`) %>% 
  rename(Bottle=`Sample Name`) %>% 
  rename(`Bottom TA (µmol/kg)`=`TA_umolkg_corr`)

TA_b$Bottle=round(as.numeric(TA_b$Bottle))
TA_b$`Bottom TA (µmol/kg)`=round(as.numeric(TA_b$`Bottom TA (µmol/kg)`))

Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA=merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TAs, y = TA_b, by.x = "Bottom_TA_DIC", by.y = "Bottle", all.x = TRUE)

NOx_PO4 <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="NOx_PO4") %>% 
  filter(`QA Flag`=="A") %>% 
  select("Sample ID",`NOx_mg/L`,`NOx_LOD`,`PO4_mg/L`,`PO4_LOD`) %>% 
  rename(`NOx (mg/L)`=`NOx_mg/L`) %>% 
  rename(`NOx LOD (mg/L)`=`NOx_LOD`) %>% 
  rename(`PO4 (mg/L)`=`PO4_mg/L`) %>% 
  rename(`PO4 LOD (mg/L)`=`PO4_LOD`)

NOx_PO4$`Sample ID`=round(as.numeric(NOx_PO4$`Sample ID`))

Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA_NOx_PO4=merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA, y = NOx_PO4, by.x = "POx_NOx", by.y = "Sample ID", all.x = TRUE)

TKN <- read_excel("5th_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(`QA Flag`=="A") %>% 
  select("Sample ID",`TKN_mg/L`,`TKN_LOD`) %>% 
  rename(`TKN (mg/L)`=`TKN_mg/L`) %>% 
  rename(`TKN LOD (mg/L)`=`TKN_LOD`)

TKN$`Sample ID`=round(as.numeric(TKN$`Sample ID`))

Quarter_5_data=
  merge(x = Quarter_5_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA_NOx_PO4, y = TKN, by.x = "TKN", by.y = "Sample ID", all.x = TRUE) %>% 
  relocate(TKN:Total_Suspended_Solids, .after = Bottom_Van_Dorn_Depth_m) %>% 
  relocate(TSS:Turbidity,.after = Bottom_Van_Dorn_Depth_m)

#Export Report_3 data to a csv
write.csv(Quarter_5_data,"Quarter_5_Data_8-29-24.csv",row.names=FALSE)
