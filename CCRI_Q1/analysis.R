##### Load required libraries #####
library(tidyverse)
library(readxl)
library(hablar)
library(lubridate)

##### Import Report_1_Tables and calculate secchi disk mean and sd#####
Report_1_Tables <- read_excel("Report 1 - Tables.xlsx", sheet="Raw Data") %>% 
  group_by(Site_Code,Date) %>% 
  mutate(Secchi_Depth_mean_m = mean(c(Secchi_Depth_1_m,Secchi_Depth_2_m,Secchi_Depth_3_m))) %>% 
  mutate(Secchi_Depth_sd_m = sd(c(Secchi_Depth_1_m,Secchi_Depth_2_m,Secchi_Depth_3_m)))

##### Convert column names to match code####
colnames(Report_1_Tables)
Report_1_Tables$`Total Suspended Solids`=as.numeric(Report_1_Tables$"Total_Suspended_Solids")
Report_1_Tables$Enterococcus=as.numeric(Report_1_Tables$Enterococcus)
Report_1_Tables$Enterococcus=as.numeric(Report_1_Tables$Enterococcus)
Report_1_Tables$"Settleable Solids"=as.numeric(Report_1_Tables$"Settleable_Solids")
Report_1_Tables$"Surface BOD"=as.numeric(Report_1_Tables$"Surface_BOD")
Report_1_Tables$"Bottom BOD"=as.numeric(Report_1_Tables$"Bottom_BOD")
Report_1_Tables$"Chl-a"=as.numeric(Report_1_Tables$"Chl_a")
Report_1_Tables$"Turbidity"=as.numeric(Report_1_Tables$"Turbidity")
Report_1_Tables$"Surface TA/DIC"=as.numeric(Report_1_Tables$"Surface_TA_DIC")
Report_1_Tables$"Bottom TA/DIC"=as.numeric(Report_1_Tables$"Bottom_TA_DIC")
Report_1_Tables$Time_In <- format(strptime(Report_1_Tables$Time_In, "%Y-%m-%d %H:%M:%OS"), "%H:%M:%OS")
Report_1_Tables$Time_Out <- format(strptime(Report_1_Tables$Time_Out, "%Y-%m-%d %H:%M:%OS"), "%H:%M:%OS")

#### TSS QA/QC
TSS_QA=read_excel("Data field and Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  filter(month(`Date analyzed`) != 3) %>% 
  mutate(standard=as.numeric(`Cellulose weight (g)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=standard-`Result (mg/L)`)
summary(TSS_QA$accuracy)
mean(TSS_QA$accuracy)
sd(TSS_QA$accuracy)
TSS_QA$accuracy
#Mean accuracy for Total Suspended Solids was 1.9 mg/L with a precision of 2.5 mg/L. All accuracy checks were within the stated 5 mg/L except for one excedence on 5/12/23 of 5.5 mg/L. This error was corrected and not repeated for any subsequent Cellulose standard accuracy checks.

#### Chl-a QA/QC
Chla_QA=read_excel("Data field and Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(accuracy=228-`Chl-a (µg/L)`)
summary(Chla_QA$accuracy)
mean(Chla_QA$accuracy)
sd(Chla_QA$accuracy)
Chla_QA$accuracy
#Mean accuracy for Chlorophyll a ranged from -12.3 to 41.1 ug/L with a mean accuracy of 14.4 ug/L and precision of ±26.7 ug/L. There is no listed accuracy in the QAPP although precision should be within ±20%. The least precise analysis of the standard was within 18% of the reported value and falls within the precision tests outlined in the QAPP.
  
##### Import Chemistry Lab Data and Merge with Report_1_Tables #####
TSS <- read_excel("Data field and Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("TSS", ignore_case = TRUE))) %>% 
  mutate(TSS=round(as.numeric(str_remove(`Sample ID`,"TSS_"),digits=2))) %>% 
  select(TSS,`Result (mg/L)`) %>% 
  rename(`TSS (mg/L)`=`Result (mg/L)`)
Report_1_Tables$"Total Suspended Solids"

Report_1_TSS=merge(x = Report_1_Tables, y = TSS, by.x = "Total Suspended Solids", by.y = "TSS", all.x = TRUE)

enterococcus <- read_excel("Data field and Lab_parameters.xlsx",sheet="Ent") %>%
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("_", ignore_case = TRUE))) %>% 
  mutate(enterococcus=str_remove(`Sample ID`,"Ent_"),digits=2) %>% 
  select(enterococcus,`MPN by 10 (Dilution factor)`) %>% 
  rename(`Enterococcus (MPN)`=`MPN by 10 (Dilution factor)`)

Report_1_TSS_Ent=merge(x = Report_1_TSS, y = enterococcus, by.x = "Enterococcus", by.y = "enterococcus", all.x = TRUE)

SS <- read_excel("Data field and Lab_parameters.xlsx",sheet="SS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("SS", ignore_case = TRUE))) %>% 
  mutate(SS=round(as.numeric(str_remove(`Sample ID`,"SS_"),digits=2))) %>% 
  select(SS,`Results ml/L`) %>% 
  rename(`SS (ml/L)`=`Results ml/L`)

Report_1_TSS_Ent_SS=merge(x = Report_1_TSS_Ent, y = SS, by.x = "Settleable Solids", by.y = "SS", all.x = TRUE)

BOD_Surface <- read_excel("Data field and Lab_parameters.xlsx",sheet="BOD") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Surface", ignore_case = TRUE))) %>% 
  mutate(BOD=round(as.numeric(str_remove_all(str_remove_all(str_remove_all(`Sample ID`,"BOD_"),"\\(Botton\\)"),"\\(Surface\\)")),digits=2)) %>%
  select(BOD,`BOD in % or mg/L`) %>% 
  rename(`Surface BOD (mg/L)`=`BOD in % or mg/L`)
BOD_Surface$`Surface BOD (mg/L)`=as.numeric(BOD_Surface$`Surface BOD (mg/L)`)

Report_1_TSS_Ent_SS_BODs=merge(x = Report_1_TSS_Ent_SS, y = BOD_Surface, by.x = "Surface BOD", by.y = "BOD", all.x = TRUE)

BOD_Bottom <- read_excel("Data field and Lab_parameters.xlsx",sheet="BOD") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Botton", ignore_case = TRUE))) %>% 
  mutate(BOD=round(as.numeric(str_remove_all(str_remove_all(str_remove_all(`Sample ID`,"BOD_"),"\\(Botton\\)"),"\\(Surface\\)")),digits=2)) %>%
  select(BOD,`BOD in % or mg/L`) %>% 
  rename(`Bottom BOD (mg/L)`=`BOD in % or mg/L`)
BOD_Bottom$`Bottom BOD (mg/L)`=as.numeric(BOD_Bottom$`Bottom BOD (mg/L)`)

Report_1_TSS_Ent_SS_BOD=merge(x = Report_1_TSS_Ent_SS_BODs, y = BOD_Bottom, by.x = "Bottom BOD", by.y = "BOD", all.x = TRUE)

Chla <- read_excel("Data field and Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Chl_a", ignore_case = TRUE))) %>% 
  mutate(Chla=round(as.numeric(str_remove(`Sample ID`,"Chl_a_"),digits=2))) %>% 
  select(Chla,`Chl-a (µg/L)`)

Report_1_TSS_Ent_SS_BOD_Chla=merge(x = Report_1_TSS_Ent_SS_BOD, y = Chla, by.x = "Chl-a", by.y = "Chla", all.x = TRUE)

Turbidity <- read_excel("Data field and Lab_parameters.xlsx",sheet="Turbidity") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Turb_", ignore_case = TRUE))) %>% 
  mutate(Turb=round(as.numeric(str_remove(`Sample ID`,"Turb_"),digits=2))) %>% 
  select(Turb,NTU) %>% 
  rename(`Turbidity (NTU)`=`NTU`)

Report_1_TSS_Ent_SS_BOD_Chla_Turbidity=merge(x = Report_1_TSS_Ent_SS_BOD_Chla, y = Turbidity, by.x = "Turbidity", by.y = "Turb", all.x = TRUE)

Alkalinity_raw=read_excel("Data field and Lab_parameters.xlsx",sheet="TA") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("Sample ", ignore_case = TRUE))) %>% 
  mutate(Bottle=as.numeric(str_remove(`Sample Name`,regex("Sample ", ignore_case = TRUE))))%>% 
  select(Bottle,`Total Alkalinity (uM)`)

Alkalinity_corr=read_excel("Data field and Lab_parameters.xlsx",sheet="TA") %>% 
  filter(`QA Flag`=="Q") %>%
  filter(str_detect(`Sample Name`,  regex("Sample ", ignore_case = TRUE))) %>% 
  mutate(Bottle=as.numeric(str_remove(`Sample Name`,regex("Sample ", ignore_case = TRUE))))%>% 
  group_by(Bottle) %>% 
  summarize(`Total Alkalinity (uM)`=as.numeric(`Corrected Total Alkalinity (uM)`))

Alkalinity=bind_rows(Alkalinity_raw,Alkalinity_corr)

read_excel("Data field and Lab_parameters.xlsx",sheet="TA") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  filter(`QA Flag`=="A") %>% 
  summarize(TA_accuracy=2202.12-mean(`Total Alkalinity (uM)`),TA_precision=sd(`Total Alkalinity (uM)`))
#TA accuracy was 0.7 uM with a precision of ±2uM

Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf=merge(x = Report_1_TSS_Ent_SS_BOD_Chla_Turbidity, y = Alkalinity, by.x = "Surface TA/DIC", by.y = "Bottle", all.x = TRUE) %>% 
  rename("Surface TA (uM)"="Total Alkalinity (uM)")

Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth=merge(x = Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf, y = Alkalinity, by.x = "Bottom TA/DIC", by.y = "Bottle", all.x = TRUE)%>% 
  rename("Bottom TA (uM)"="Total Alkalinity (uM)")

DIC_raw=read_excel("Data field and Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(`QA Flag`=="A") %>% 
  mutate(Bottle=as.numeric(`Sample Name`)) %>% 
  group_by(Bottle) %>% 
  summarize(`DIC (uM)`=mean(`DIC (uM)`))

DIC_corr=read_excel("Data field and Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(`QA Flag`=="Q") %>% 
  mutate(Bottle=as.numeric(`Sample Name`)) %>% 
  group_by(Bottle) %>% 
  summarize(`DIC (uM)`=mean(as.numeric(`DICcorr (uM)`)))

DIC=bind_rows(DIC_raw,DIC_corr)

read_excel("Data field and Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  filter(`QA Flag`=="A") %>% 
  summarize(DIC_accuracy=2028.23-mean(`DIC (uM)`),DIC_precision=sd(`DIC (uM)`))
#DIC accuracy was 0.6 uM with a precision of +-2.5uM
#however some samples were corrected for instrument drift by up to 8.6uM

Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth_DICsurf=merge(x = Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth, y = DIC, by.x = "Surface TA/DIC", by.y = "Bottle", all.x = TRUE) %>% 
  rename("Surface DIC (uM)"="DIC (uM)")
Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth_DICsurf_DICdepth=merge(x = Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth_DICsurf, y = DIC, by.x = "Bottom TA/DIC", by.y = "Bottle", all.x = TRUE) %>% 
  rename("Bottom DIC (uM)"="DIC (uM)")

View(Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth_DICsurf_DICdepth)
colnames(Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth_DICsurf_DICdepth)

Report_1_data = Report_1_TSS_Ent_SS_BOD_Chla_Turbidity_TAsurf_TAdepth_DICsurf_DICdepth %>% 
  select(-c("Turbidity","Chl-a","Bottom BOD","Surface BOD","Settleable Solids","Total Suspended Solids","Bottom TA/DIC","Surface TA/DIC"))

summary(Report_1_data) #this produces summary stats for all of the data we collected (doesn't work for things that aren't numeric)

write.csv(Report_1_data,"Report_1_Data.csv",row.names=FALSE) #we need to cross reference values to make sure everything is correct and also manually fill in missing values that didn't match because of sample names or replicates or etc.
