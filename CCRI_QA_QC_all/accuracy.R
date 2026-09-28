#####load required libraries####
library(tidyverse)
library(readxl)

#Calculate depth, temperature, salinity, pH, DO sonde Accuracy

Sonde_Accuracy=read_excel("Sonde_Daily_Checks.xlsx") %>% 
  mutate(depth_accuracy=((as.numeric(Depth_meas)-as.numeric(Depth_std))/as.numeric(Depth_std))*100) %>% 
  mutate(Temp_accuracy=((as.numeric(T_meas)-as.numeric(T_std))/as.numeric(T_std))*100)%>% 
  mutate(S_accuracy=((as.numeric(S_meas)-as.numeric(S_std))/as.numeric(S_std))*100)%>% 
  mutate(pH_accuracy=((as.numeric(pH_meas)-as.numeric(pH_std))/as.numeric(pH_std))*100)%>% 
  mutate(DO_accuracy=((as.numeric(DO_meas)-as.numeric(DO_std))/as.numeric(DO_std))*100) %>% 
  summarize(mean_depth_accuracy=mean(depth_accuracy,na.rm=TRUE),
            mean_Temp_accuracy=mean(Temp_accuracy,na.rm=TRUE),
            mean_S_accuracy=mean(S_accuracy,na.rm=TRUE),
            mean_pH_accuracy=mean(pH_accuracy,na.rm=TRUE),
            mean_DO_accuracy=mean(DO_accuracy,na.rm=TRUE))

#Calculate TSS Lab Accuracy
TSS_Accuracy_Q1=read_excel("Data/1st_quarter_Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (g)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q2=read_excel("Data/2nd quarter_ Data field and Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (g)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q3=read_excel("Data/3rd_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (g)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q4=read_excel("Data/4th_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (g)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q5=read_excel("Data/5th_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (mg)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q6=read_excel("Data/6th_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (mg)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q7=read_excel("Data/7th_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (mg)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_Q8=read_excel("Data/8th_quarter_ Lab_parameters.xlsx",sheet="TSS") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("Cellulose", ignore_case = TRUE))) %>% 
  mutate(standard=as.numeric(`Cellulose weight (mg)`)/as.numeric(`Volume sample (L)`)) %>% 
  mutate(accuracy=((`Result (mg/L)`-standard)/standard)*100)

TSS_Accuracy_percent = mean(
  c(TSS_Accuracy_Q1$accuracy,
    TSS_Accuracy_Q2$accuracy,
    TSS_Accuracy_Q3$accuracy,
    TSS_Accuracy_Q4$accuracy,
    TSS_Accuracy_Q5$accuracy,
    TSS_Accuracy_Q6$accuracy,
    TSS_Accuracy_Q7$accuracy,
    TSS_Accuracy_Q8$accuracy))

#Calculate Chl-a accuracy
Chla_Accuracy_Q1=read_excel("Data/1st_quarter_Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((`Chl-a (µg/L)`-standard)/standard)*100)

Chla_Accuracy_Q2=read_excel("Data/2nd quarter_ Data field and Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`)) %>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_Q3=read_excel("Data/3rd_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_Q4=read_excel("Data/4th_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_Q5=read_excel("Data/5th_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_Q6=read_excel("Data/6th_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_Q7=read_excel("Data/7th_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_Q8=read_excel("Data/8th_quarter_ Lab_parameters.xlsx",sheet="Chlor.a") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("standard", ignore_case = TRUE))) %>% 
  mutate(standard=parse_number(`Sample ID`))%>% 
  summarize(accuracy=((as.numeric(`Chl-a (µg/L)`)-standard)/standard)*100)

Chla_Accuracy_percent = mean(
  c(Chla_Accuracy_Q1$accuracy,
    Chla_Accuracy_Q2$accuracy,
    Chla_Accuracy_Q3$accuracy,
    Chla_Accuracy_Q4$accuracy,
    Chla_Accuracy_Q5$accuracy,
    Chla_Accuracy_Q6$accuracy,
    Chla_Accuracy_Q7$accuracy,
    Chla_Accuracy_Q8$accuracy))

#Calculate BOD Lab Accuracy
BOD_Accuracy_percent=read_excel("BOD_Accuracy.xlsx") %>% 
  filter(`QA Flag`=="A") %>% 
  summarize(accuracy=mean(((as.numeric(`GGA Spike Measured`)-2)/2)*100,na.rm=TRUE))

#Calculate TA Lab Accuracy
TA_Q1=read_excel("Data/1st_quarter_Lab_parameters.xlsx",sheet="TA") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  select(`Sample ID`,`Corrected Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Corrected Total Alkalinity (uM)`) %>% 
  rename(`Sample Name`=`Sample ID`)

TA_Q2=read_excel("Data/2nd quarter_ Data field and Lab_parameters.xlsx",sheet="TA") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample ID`,`TA (uM)`) %>% 
  rename(`Sample Name`=`Sample ID`)

TA_Q3=read_excel("Data/3rd_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample Name`,`Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Total Alkalinity (uM)`)

TA_Q4=read_excel("Data/4th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample Name`,`Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Total Alkalinity (uM)`)

TA_Q5=read_excel("Data/5th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample Name`,`Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Total Alkalinity (uM)`)

TA_Q6=read_excel("Data/6th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample Name`,`Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Total Alkalinity (uM)`)

TA_Q7=read_excel("Data/7th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample Name`,`Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Total Alkalinity (uM)`)

TA_Q8=read_excel("Data/8th_quarter_ Lab_parameters.xlsx",sheet="All TA raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE)))%>% 
  select(`Sample Name`,`Total Alkalinity (uM)`) %>% 
  rename(`TA (uM)`=`Total Alkalinity (uM)`)

TA_CRM=rbind(TA_Q1,TA_Q2,TA_Q3,TA_Q4,TA_Q5,TA_Q6,TA_Q7,TA_Q8) %>% 
  mutate(CRM=as.numeric(substr(`Sample Name`,5,7)))

TA_CRM_known=read_excel("CRM_TA_DIC.xlsx") %>% 
  select(`CRM`,`TA (uM)`) %>% 
  rename(`CRM TA (uM)`=`TA (uM)`)

TA_CRM_accuracy_percent=left_join(TA_CRM,TA_CRM_known,by="CRM") %>% 
  summarize(accuracy=mean(((as.numeric(`TA (uM)`)-`CRM TA (uM)`)/`CRM TA (uM)`)*100,na.rm=TRUE))

#Calculate DIC Lab Accuracy

DIC_Q1=read_excel("Data/1st_quarter_Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(`QA Flag`==c("A","Q")) %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(as.numeric(`DIC_final (uM)`))) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_Q2=read_excel("Data/2nd quarter_ Data field and Lab_parameters.xlsx",sheet="DIC") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  ungroup() %>% 
  select(`Sample ID`,`DIC (uM)`) %>% 
  rename(`Sample Name`=`Sample ID`)

DIC_Q3=read_excel("Data/3rd_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(as.numeric(DIC_final_uM))) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_Q4=read_excel("Data/4th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(`DIC (uM)`)) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_Q5=read_excel("Data/5th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(`DIC (uM)`)) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_Q6=read_excel("Data/6th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(`DIC (uM)`)) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_Q7=read_excel("Data/7th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(`DIC_corr (uM)`)) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_Q8=read_excel("Data/8th_quarter_ Lab_parameters.xlsx",sheet="All DIC raw data") %>% 
  filter(`QA Flag`=="A") %>% 
  filter(str_detect(`Sample Name`,  regex("CRM", ignore_case = TRUE))) %>% 
  group_by(`Sample # In Batch`) %>% 
  mutate(`DIC (uM)`=mean(`DIC_corr (uM)`)) %>% 
  filter (! duplicated(`Sample # In Batch`)) %>% 
  ungroup() %>% 
  select(`Sample Name`,`DIC (uM)`)

DIC_CRM=rbind(DIC_Q1,DIC_Q2,DIC_Q3,DIC_Q4,DIC_Q5,DIC_Q6,DIC_Q7,DIC_Q8) %>% 
  mutate(CRM=as.numeric(substr(`Sample Name`,5,7)))

DIC_CRM_known=read_excel("CRM_TA_DIC.xlsx") %>% 
  select(`CRM`,`DIC (uM)`) %>% 
  rename(`CRM DIC (uM)`=`DIC (uM)`)

DIC_CRM_Q1=DIC_Q1 %>% 
  mutate(CRM=as.numeric(substr(`Sample Name`,5,7)))

DIC_CRM=rbind(DIC_Q2,DIC_Q3,DIC_Q4,DIC_Q5,DIC_Q6,DIC_Q7,DIC_Q8) %>% 
  mutate(CRM=as.numeric(substr(`Sample Name`,5,7)))

#CRM in Q1 were analyzed as umol/kg whereas later quarters were analyzed as uM and converted to umol/kg in subsequent calculations so Q1 must be treated differently for evaluation of CRM
DIC_CRM_known_Q1=read_excel("CRM_TA_DIC.xlsx") %>% 
  filter(CRM==204) %>% 
  select(`CRM`,`DIC (umol/kg)`) %>% 
  rename(`CRM DIC`=`DIC (umol/kg)`)

DIC_CRM_known_other=read_excel("CRM_TA_DIC.xlsx") %>% 
  filter(CRM!=204) %>% 
  select(`CRM`,`DIC (uM)`) %>% 
  rename(`CRM DIC`=`DIC (uM)`)

DIC_CRM_accuracy_percent=rbind(
  left_join(DIC_CRM_Q1,DIC_CRM_known_Q1,by="CRM"),
  left_join(DIC_CRM,DIC_CRM_known_other,by="CRM")) %>% 
  summarize(accuracy=mean(((as.numeric(`DIC (uM)`)-`CRM DIC`)/`CRM DIC`)*100,na.rm=TRUE))

#Calculate NOx Lab Accuracy
NOx_Accuracy_percent=read_excel("NOx_PO4_CRM_Accuracy.xlsx") %>% 
  summarize(accuracy=mean(((as.numeric(NOx)-as.numeric(NOx_CRM))/as.numeric(NOx_CRM))*100,na.rm=TRUE))

#Calculate oPO4 Lab Accuracy
oPO4_Accuracy_percent=read_excel("NOx_PO4_CRM_Accuracy.xlsx") %>% 
  summarize(accuracy=mean(((as.numeric(oPO4)-as.numeric(oPO4_CRM))/as.numeric(oPO4_CRM))*100,na.rm=TRUE))

#Calculate TKN Lab Accuracy

TKN_standard = 1.5

#TKN_Q1 CRM was below LOD

TKN_Q2=read_excel("TKN_CRM_Q2.xlsx") %>% 
  summarize(accuracy=((as.numeric(`TKN_corr_mg/L`)-`CRM_TKN_mg/L`)/`CRM_TKN_mg/L`)*100)

TKN_Q3=read_excel("Data/3rd_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  summarize(accuracy=((as.numeric(`TKN_mg/L`)-TKN_standard)/TKN_standard)*100)

TKN_Q4=read_excel("Data/4th_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  summarize(accuracy=((as.numeric(`TKN_mg/L`)-TKN_standard)/TKN_standard)*100)

TKN_Q5=read_excel("Data/5th_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  summarize(accuracy=((as.numeric(`TKN_mg/L`)-TKN_standard)/TKN_standard)*100)

TKN_Q6=read_excel("Data/6th_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  summarize(accuracy=((as.numeric(`TKN_mg/L`)-TKN_standard)/TKN_standard)*100)

TKN_Q7=read_excel("Data/7th_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  summarize(accuracy=((as.numeric(`TKN_mg/L`)-TKN_standard)/TKN_standard)*100)

TKN_Q8=read_excel("Data/8th_quarter_ Lab_parameters.xlsx",sheet="TKN") %>% 
  filter(str_detect(`Sample ID`,  regex("CRM", ignore_case = TRUE))) %>% 
  mutate(accuracy=((as.numeric(`TKN_mg/L`)-TKN_standard)/TKN_standard)*100)

TKN_Accuracy_percent = mean(
  c(TKN_Q2$accuracy,
    TKN_Q3$accuracy,
    TKN_Q4$accuracy,
    TKN_Q5$accuracy,
    TKN_Q6$accuracy,
    TKN_Q7$accuracy,
    TKN_Q8$accuracy))