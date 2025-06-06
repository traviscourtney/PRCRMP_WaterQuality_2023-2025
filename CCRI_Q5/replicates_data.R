#####load required libraries####
library(tidyverse)

####import Report 4 data####
Report_5_data=read_csv("Quarter_5_Data_8-20-24.csv")

#find replicate sites
Replicate_Sites=substr(Report_5_data[str_detect(Report_5_data$Site_Code,"-R"),]$Site_Code,1,6)

Matching_Sites=str_c(Replicate_Sites, collapse = "|")

Replicated_Sites=as_tibble(str_subset(Report_5_data$Site_Code,Matching_Sites)) %>% 
  rename("Site_Code"="value")

Replicate_data=merge(Report_5_data,Replicated_Sites,by="Site_Code") %>% 
  select("Site_Code","Surface_Temp_C","Surface_Sal_psu","Surface_DO_mg_L","Surface_pH",
         "Bottom_Temp_C","Bottom_Sal_psu","Bottom_DO_mg_L","Bottom_pH",
         "Secchi_Avg_Depth",`TSS (mg/L)`,`Enterococcus (MPN)`,`SS (ml/L)`,
         `Surface BOD (mg/L)`,`Bottom BOD (mg/L)`,`Chl-a (µg/L)`,`Turbidity (NTU)`,
         `Surface DIC (µmol/kg)`,`Bottom DIC (µmol/kg)`,`Surface TA (µmol/kg)`,
         `Bottom TA (µmol/kg)`,`NOx LOD (mg/L)`,`PO4 LOD (mg/L)`,`TKN LOD (mg/L)`)

write.csv(Replicate_data,"Quarter_5_Replicate_data.csv",row.names=FALSE)