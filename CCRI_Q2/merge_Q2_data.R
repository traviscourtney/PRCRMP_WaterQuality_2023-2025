##### Load required libraries #####
library(tidyverse)
library(readxl)
library(hablar)

#Turn off scientific notation
options(scipen=999)

##### Import Report_2_log and convert bottle ID to numbers#####
field_log <- read_excel("Report_2_log.xlsx",sheet="data_tac")
field_log$TSS=as.numeric(field_log$Total_Suspended_Solids)
field_log$Enterococcus=as.numeric(field_log$Enterococcus)
field_log$SS=as.numeric(field_log$Settleable_Solids)
field_log$Surface_BOD=as.numeric(field_log$Surface_BOD)
field_log$Bottom_BOD=as.numeric(field_log$Bottom_BOD)
field_log$Chl_a=as.numeric(field_log$Chl_a)
field_log$Turbidity=as.numeric(field_log$Turbidity_NTU)

##### Import Chemistry Lab Data and Merge with Report_2_Tables #####
TSS <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("TSS", ignore_case = TRUE))) %>% 
  mutate(TSS=round(as.numeric(str_remove(`Sample ID`,"TSS_"),digits=2))) %>% 
  select(TSS,`Result (mg/L)`) %>% 
  rename(`TSS (mg/L)`=`Result (mg/L)`)
field_log$TSS

Report_2_TSS=merge(x = field_log, y = TSS, by.x = "Total_Suspended_Solids", by.y = "TSS", all.x = TRUE)

enterococcus <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="Ent") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("_", ignore_case = TRUE))) %>% 
  mutate(enterococcus=str_remove(`Sample ID`,"Ent_"),digits=2) %>% 
  select(enterococcus,`MPN by 10 (Dilution factor)`) %>% 
  rename(`Enterococcus (MPN)`=`MPN by 10 (Dilution factor)`)

Report_2_TSS_Ent=merge(x = Report_2_TSS, y = enterococcus, by.x = "Enterococcus", by.y = "enterococcus", all.x = TRUE)

SS <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="SS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("SS", ignore_case = TRUE))) %>% 
  mutate(SS=round(as.numeric(str_remove(`Sample ID`,"SS_"),digits=2))) %>% 
  select(SS,`Results ml/L`) %>% 
  rename(`SS (ml/L)`=`Results ml/L`)

Report_2_TSS_Ent_SS=merge(x = Report_2_TSS_Ent, y = SS, by.x = "Settleable_Solids", by.y = "SS", all.x = TRUE)

BOD_Surface <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="BOD") %>% 
  filter(`QA Flag`=="A") %>% 
  mutate(BOD=round(as.numeric(str_remove(`Sample ID`,"BOD_"),digits=2))) %>%
  select(BOD,`BOD in mg/L`) %>% 
  rename(`Surface BOD (mg/L)`=`BOD in mg/L`)

Report_2_TSS_Ent_SS_BODs=merge(x = Report_2_TSS_Ent_SS, y = BOD_Surface, by.x = "Surface_BOD", by.y = "BOD", all.x = TRUE)

BOD_Bottom <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="BOD") %>% 
  filter(`QA Flag`=="A") %>% 
  mutate(BOD=round(as.numeric(str_remove(`Sample ID`,"BOD_"),digits=2))) %>%
  select(BOD,`BOD in mg/L`) %>% 
  rename(`Bottom BOD (mg/L)`=`BOD in mg/L`)

Report_2_TSS_Ent_SS_BOD=merge(x = Report_2_TSS_Ent_SS_BODs, y = BOD_Bottom, by.x = "Bottom_BOD", by.y = "BOD", all.x = TRUE)

Chla <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Chl_a", ignore_case = TRUE))) %>% 
  mutate(Chla=round(as.numeric(str_remove(`Sample ID`,"Chl_a_"),digits=2))) %>% 
  select(Chla,`Chl-a (µg/L)`)

Report_2_TSS_Ent_SS_BOD_Chla=merge(x = Report_2_TSS_Ent_SS_BOD, y = Chla, by.x = "Chl_a", by.y = "Chla", all.x = TRUE)

Turbidity <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="Turbidity") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Turb_", ignore_case = TRUE))) %>% 
  mutate(Turb=round(as.numeric(str_remove(`Sample ID`,"Turb_"),digits=2))) %>% 
  select(Turb,NTU) %>% 
  rename(`Turbidity (NTU)`=`NTU`)

Report_2_TSS_Ent_SS_BOD_Chla_Turbidity=merge(x = Report_2_TSS_Ent_SS_BOD_Chla, y = Turbidity, by.x = "Turbidity", by.y = "Turb", all.x = TRUE)

DIC_s <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample ID`,`DIC (uM)`) %>% 
  rename(Bottle=`Sample ID`) %>% 
  rename(`Surface DIC (µM)`=`DIC (uM)`)

Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DICs=merge(x = Report_2_TSS_Ent_SS_BOD_Chla_Turbidity, y = DIC_s, by.x = "Surface_TA_DIC", by.y = "Bottle", all.x = TRUE)

DIC_b <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample ID`,`DIC (uM)`) %>% 
  rename(Bottle=`Sample ID`) %>% 
  rename(`Bottom DIC (µM)`=`DIC (uM)`)

Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC=merge(x = Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DICs, y = DIC_b, by.x = "Bottom_TA_DIC", by.y = "Bottle", all.x = TRUE)

TA_s <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="TA") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample ID`,`TA (uM)`) %>% 
  rename(Bottle=`Sample ID`) %>% 
  rename(`Surface TA (µM)`=`TA (uM)`)

Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TAs=merge(x = Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC, y = TA_s, by.x = "Surface_TA_DIC", by.y = "Bottle", all.x = TRUE)

TA_b <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="TA") %>% 
  filter(`QA Flag`=="A") %>% 
  select(`Sample ID`,`TA (uM)`) %>% 
  rename(Bottle=`Sample ID`) %>% 
  rename(`Bottom TA (µM)`=`TA (uM)`)

Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA=merge(x = Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TAs, y = TA_b, by.x = "Bottom_TA_DIC", by.y = "Bottle", all.x = TRUE)

NOx_PO4 <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="NOx_PO4") %>% 
  select(Bottle,`NOx (mg/L)`,`PO4 (mg/L)`)

Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA_NOx_PO4=merge(x = Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA, y = NOx_PO4, by.x = "POx_NOx", by.y = "Bottle", all.x = TRUE)

TKN <- read_excel("2nd quarter_ Data field and Lab_parameters.xlsx",sheet="TKN") %>% 
  select(Bottle,`TKN_corr (mg/L)`)%>% 
  rename(`TKN (mg/L)`=`TKN_corr (mg/L)`)

Report_2_data=merge(x = Report_2_TSS_Ent_SS_BOD_Chla_Turbidity_DIC_TA_NOx_PO4, y = TKN, by.x = "TKN", by.y = "Bottle", all.x = TRUE)

##### Report 2 Plots and Summary Tables

#Export Report_2 data to a csv
write.csv(Report_2_data,"Report_2_Data.csv",row.names=FALSE)
