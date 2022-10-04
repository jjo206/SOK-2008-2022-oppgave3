library(OECD) 
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(PxWebApiData)
library(dplyr)

dsets<-get_datasets()
search_dataset("wage",dsets)
search_dataset("unemployment",dsets)



#MinWage
minwage <- get_dataset("MIN2AVE",
                       filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                       pre_formatted = TRUE)

minwage2019 <- subset(minwage, Time < 2019 & Time >2007 & SERIES=="MEDIAN")
minwage2007_2019 <- subset(minwage2019, Time>2007)

#UnEmpl
unempl <- get_dataset("MIG_NUP_RATES_GENDER",
                      filter = "USA+CAN+FRA+GBR+DEU+NZL", 
                      pre_formatted = TRUE)


unempl2019 <- subset(unempl, Time<2019 & RATE=="U_RATE" & BIRTH=="NB" & GENDER=="TOT")
unempl2007_2019 <- subset(unempl2019, Time>2007)


minwage_unempl <-left_join(minwage2007_2019, unempl2007_2019, by=c("COUNTRY","Time"))


complete_minwage_unempl <- na.omit(minwage_unempl)


complete_minwage_unempl$MinWage_0 <-as.numeric(complete_minwage_unempl$ObsValue.x) 

complete_minwage_unempl$UnEmpl <-as.numeric(complete_minwage_unempl$ObsValue.y)

complete_minwage_unempl$MinWage <- complete_minwage_unempl$MinWage_0 * 100


minwage_plot <- ggplot(data=complete_minwage_unempl,aes(x=UnEmpl,y=MinWage, group=COUNTRY, color=COUNTRY)) +
  geom_line(aes(group=COUNTRY), size=1) +
  geom_point(size=2.5)+
  labs(x = "Unemployment" , y ="Min Wage")  + 
  theme(legend.position="none")+
  geom_label_repel(
    data=complete_minwage_unempl %>% group_by(COUNTRY) %>%
      filter(UnEmpl==min(UnEmpl)), 
    aes(UnEmpl, MinWage, fill = factor(COUNTRY), label = sprintf('%s', COUNTRY)),
    color = "black",
    fill = "white")
minwage_plot