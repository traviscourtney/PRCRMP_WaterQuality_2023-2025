library(tidyverse)
library(readxl)

Report_1_data=read_excel("Report_1_Data.xlsx") %>% 
  replace_with_na_all(condition = ~.x == -999)

colnames(Report_1_data) #check the column names of Report_1_data
summary(Report_1_data) # summary of the Report_1_data
#this summary shows that all of the chemistry data is characters instead of numeric so you need to tell R to make them numeric by writing as.numeric(variable) instead of simply variable for the color argument in the ggplot

TSS_plot=
  ggmap(pr)+
  geom_point(Report_1_data, mapping=aes(x= Longitude, y = Latitude, color=as.numeric(TSS_mg_L)), size= 4)+
  scale_color_viridis()+
  scale_y_continuous(limits = c(17.75, 18.75), breaks = seq(17.75, 18.75, by = 0.5))+
  scale_x_continuous(limits = c(-67.5, -65), breaks = seq(-67.5, -65, by = 0.5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(d) Total Suspended Soligs (mg/L)")+
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black",face="italic"),
        axis.title.y=element_text(colour = "black"),
        legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

#saves the plot as a pdf
ggsave("TSS_plot.pdf",TSS_plot,height=6.5,width=14)