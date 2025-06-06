##### Load required libraries #####
library(plyr)
library(tidyverse)
library(readxl)
library(hablar)

#Turn off scientific notation
options(scipen=999)

#Import Updated Nutrient Data
Q1_NOx_PO4 <- read_excel("Q1_Q2_Nutrient_Corrections.xlsx",sheet="Q1_NOx_PO4")
Q1_TKN <- read_excel("Q1_Q2_Nutrient_Corrections.xlsx",sheet="Q1_TKN")
Q2_NOx_PO4 <- read_excel("Q1_Q2_Nutrient_Corrections.xlsx",sheet="Q2_NOx_PO4")
Q2_TKN <- read_excel("Q1_Q2_Nutrient_Corrections.xlsx",sheet="Q2_TKN")
site_Code_Order <- read_excel("Q1_Q2_Site_Code_Order.xlsx")

#Import Q1 and Q2 Report Data
R1 <- read_excel("Report_1_Data_Manuel_7-25-23_tac.xlsx")
R2 <- read_excel("Report_2_Data_v5.xlsx")

#Merge data
R1_NOx_PO4=merge(x = R1, y = Q1_NOx_PO4, by.x = "POx_NOx", by.y = "Sample", all.x = TRUE)
R1_NOx_PO4_TKN=merge(x = R1_NOx_PO4, y = Q1_TKN, by.x = "TKN", by.y = "Sample", all.x = TRUE)

#Merge data
R2_NOx_PO4=merge(x = R2, y = Q2_NOx_PO4, by.x = "POx_NOx", by.y = "Sample", all.x = TRUE)
R2_NOx_PO4_TKN=merge(x = R2_NOx_PO4, y = Q2_TKN, by.x = "TKN", by.y = "Sample", all.x = TRUE)

#Export data to a csv
write.csv(R1_NOx_PO4_TKN,"R1_NOx_PO4_TKN.csv",row.names=FALSE)
write.csv(R2_NOx_PO4_TKN,"R2_NOx_PO4_TKN.csv",row.names=FALSE)


