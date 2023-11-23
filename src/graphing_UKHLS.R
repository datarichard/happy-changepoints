###SETUP and load in changepoint model data as well as full processed UK data ###

library(tidyverse)
library(gridExtra)

changepoints <- read_rds("../results/models_uk.rds")
processed_uk <- read_rds("../results/processed_uk.rds")
uk_happiness_cp <- changepoints %>% filter(changepoint == "happiness")
uk_satisfaction_cp <- changepoints %>% filter(changepoint == "life_satisfaction")

###Generate graph for changepoint for both happiness and satisfaction###
uk_happiness_cp_gg <- ggplot(uk_happiness_cp, aes(x=year, y=psi)) + 
  geom_line(size=2, alpha=0.5, color="red")+ 
  geom_smooth(method = 'lm', formula = y~x) + 
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+
  ggtitle("Change Point in Happiness", subtitle = "(British Pounds)")+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=10), plot.subtitle = element_text(size=8))+
  coord_cartesian(ylim = c(25000, 70000)) 

uk_satisfaction_cp_gg <- ggplot(uk_satisfaction_cp, aes(x=year, y=psi)) + 
  geom_line(size=2, alpha=0.5, color="green")+
  geom_smooth(method = 'lm', formula = y~x) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+
  ggtitle("Change Point in Life Satisfaction", subtitle = "(British Pounds)")+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())+
  coord_cartesian(ylim = c(25000, 70000)) 

ggsave("../graphs/uk_happiness_cp_gg.png")
ggsave("../graphs/uk_satisfaction_cp_gg.png")


###Join full processed data and change point data for happiness and then mutate data to determine proportion about change point graph###
working <- full_join(processed_uk, uk_happiness_cp, by="year")
working <- working %>% mutate(income_above_happiness_cp = income_ann - psi)

working %>%
  group_by(year)%>%
  nest()-> working_nested 

working_nested%>%
  mutate(proportion_above_psi = map(data,~{100*sum(.x$income_above_happiness_cp>0)/(sum(.x$income_above_happiness_cp<0)+ sum(.x$income_above_happiness_cp>=0))}))%>%
  unnest() -> working

###Generate graph for proportion above change point ###
uk_happiness_acp<- ggplot(working, aes(x=year, y=proportion_above_psi)) + 
  geom_line(size=2, alpha=0.5, color="blue") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+
  ggtitle("Population above Change Point", subtitle = "(%)")+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=10), plot.subtitle = element_text(size=8))+
  coord_cartesian(ylim = c(0, 40)) 


###Creat graph for median income over time as comparative point to change points###
avg_income <- working_nested %>% 
  mutate(avg_income = map_dbl(data,~{mean(.x$income_ann)}),median_income= map_dbl(data,~{quantile(.x$income_ann, c(.50))}) )

uk_income <- ggplot(avg_income, aes(x=year, y=median_income)) + 
  geom_line(size= 2, alpha=0.5, color="darkorange") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+
  ggtitle("Median Real Household Income", subtitle = "(British Pounds)") + 
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=10), plot.subtitle = element_text(size=8))+
  coord_cartesian(ylim = c(25000, 70000))

ggsave("../graphs/uk_income.png")


###Combine 3 graphs into one overall graph and export###
combined_uk <- arrangeGrob(uk_income, uk_happiness_cp_gg, uk_happiness_acp, nrow=1)
ggsave(file="../graphs/combined_uk.png", combined_uk)


###Produce graphs about average life satisfaction and average happiness over time ###
###HAVING A FEW PROBLEMS WITH THIS PORTION OF CODE AND NEED TO REVIEW AND FIX ###
working <- working_nested%>%
   mutate(avg_mcs = map_dbl(data,~{mean(.x$mcs)}),median_mcs= map_dbl(data,~{quantile(.x$mcs, c(.50))}),avg_lifesat = map_dbl(data,~{mean(.x$life_sat)}),median_lifesat= map_dbl(data,~{quantile(.x$life_sat, c(.5))}) )


colors <- c("Average MCS" = "Blue", "Median MCS" = "Red", "Average Life Sat" = "Green", "Median Life Sat"= "Pink")
uk_happiness <- ggplot(working, aes(x=Year, y=median_mcs)) + 
 # geom_line(size=3, alpha=0.5, aes(y=avg_mcs, color = "Mean MCS"))+
  geom_line(size=2, alpha=0.5, color="green") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  ggtitle("Median Happiness", subtitle = "(1-100)") +
  #scale_color_manual(values = colors)+ 
  coord_cartesian(ylim = c(49, 53))+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())
  
uk_ls <- ggplot(working, aes(x=Year, y=avg_lifesat))+
  geom_line(size=2, alpha=0.5, color="darkorchid")+
  #geom_line(size=3, alpha=0.5,aes(y=median_lifesat, color = "Median Life Sat")) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+ 
  ggtitle("Mean Life Satisfaction", subtitle = "(0-10)") + 
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(4.5, 5.5)) +
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())

uk_median_ls <- ggplot(working, aes(x=Year, y=median_lifesat))+
  geom_line(size=2, alpha=0.5, color="pink")+
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+ 
  ggtitle("Median Life Satisfaction UK", subtitle = "(0-10)") + 
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(4, 7)) +
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())


combined_happiness_ls_uk <- arrangeGrob(uk_happiness, uk_ls, nrow = 1)
ggsave(file="../graphs/combined_happiness_ls_uk.png", combined_happiness_ls_uk)
