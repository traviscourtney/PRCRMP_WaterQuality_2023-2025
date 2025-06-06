#this creates a summary plot for multiple parameters if we want to compare box plots of similar things like surface vs bottom or to show what all of the data looks like for many of the parameters at the same time

colnames(Report_1_data) #shows us the column names that we can select for producing any summary box + jitter plots

long_data=
  Report_1_data %>% 
  pivot_longer(cols=c(Bottom_Temp_C,Bottom_Sal_psu,Bottom_DO_mg_L,Bottom_pH,Secchi_Depth_1_m,`TSS (mg/L)`,`Chl-a (µg/L)`,`Turbidity (NTU)`),#we can add the names for any params
               names_to='parameter',
               values_to='value') %>% 
  select(parameter,value)

ggplot(long_data)+
  geom_boxplot(mapping=aes(x=parameter,y=value),alpha=0.5)+
  geom_jitter(mapping=aes(x=parameter,y=value),alpha=0.5)+
  xlab("Parameter")+
  ylab("Value")+
  ggtitle("Summary Data")+
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
