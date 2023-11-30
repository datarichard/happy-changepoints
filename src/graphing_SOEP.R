###SETUP###
#Load in changepoint model data as well as full processed SOEP data#
library(tidyverse)
library(gridExtra)

changepoints_soep <- read_rds("../results/models_soep.rds")
processed_soep <- read_rds("../results/processed_soep.rds")
soep_happiness_cp <- changepoints_soep %>% filter(changepoint == "happiness")
soep_satisfaction_cp <- changepoints_soep %>% filter(changepoint == "life_satisfaction")

###Produce graphs of change point in happiness and life satisfaction###
soep_happiness_cp_gg <- ggplot(soep_happiness_cp, aes(x=year, y=psi)) + 
  geom_line(size=2, alpha=0.5, color="red")+ 
  geom_smooth(method = 'lm', formula = y~x) + 
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  ggtitle("Change Point in Happiness", subtitle = "(Euros)")+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=10), plot.subtitle = element_text(size=8))+
  coord_cartesian(ylim = c(10000, 70000)) 
ggsave("../graphs/soep_happiness_cp_gg.png")

soep_satisfaction_cp_gg <- ggplot(soep_satisfaction_cp, aes(x=year, y=psi)) + 
  geom_line(size=2, alpha=0.5, color="green")+
  geom_smooth(method = 'lm', formula = y~x) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  ggtitle("Change Point in Life Satisfaction", subtitle = "(Euros)")+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())+
  coord_cartesian(ylim = c(10000, 70000)) 
ggsave("../graphs/soep_satisfaction_cp_gg.png")


###Join full processed data and change point data for happiness and then mutate data to determine proportion about change point graph###
working <- right_join(processed_soep, soep_happiness_cp, by="year")
working <- working %>% mutate(income_above_happiness_cp = income_ann - psi)

working %>%
  group_by(year)%>%
  nest()-> working_nested 

working_nested%>%
  mutate(proportion_above_psi = map(data,~{100*sum(.x$income_above_happiness_cp>0)/(sum(.x$income_above_happiness_cp<0)+ sum(.x$income_above_happiness_cp>=0))}))%>%
  unnest() -> working

###Produce graphs for proportion of population above change point###
soep_happiness_acp<- ggplot(working, aes(x=year, y=proportion_above_psi)) + 
  geom_line(size=2, alpha=0.5, color="blue") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  ggtitle("Population above Change Point", subtitle = "(%)")+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=10), plot.subtitle = element_text(size=8))+
  coord_cartesian(ylim = c(0, 80)) 


###Create graph for median income over time as comparative point to change points###
avg_income <- working_nested %>% 
  mutate(avg_income = map_dbl(data,~{mean(.x$income_ann)}),median_income= map_dbl(data,~{quantile(.x$income_ann, c(.50))}) )

soep_income <- ggplot(avg_income, aes(x=year, y=median_income)) + 
  geom_line(size= 2, alpha=0.5, color="darkorange") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  ggtitle("Median Real Household Income", subtitle = "(Euros)") + 
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), plot.title = element_text(size=10), plot.subtitle = element_text(size=8))+
  coord_cartesian(ylim = c(10000, 70000))

ggsave("../graphs/soep_income.png")


###Combine 3 graphs into one overall graph and export###
combined_soep <- arrangeGrob(soep_income, soep_happiness_cp_gg, soep_happiness_acp, nrow=1)
ggsave(file="../graphs/combined_soep.png", combined_soep)

###Produce graphs of median happiness and average and median life satisfaction. Note that average life satisfaction is a better graph###
working <- working_nested%>%
   mutate(avg_mcs = map_dbl(data,~{mean(.x$mcs)}),median_mcs= map_dbl(data,~{quantile(.x$mcs, c(.50))}),avg_lifesat = map_dbl(data,~{mean(.x$life_sat)}),median_lifesat= map_dbl(data,~{quantile(.x$life_sat, c(.5))}) )


soep_happiness <- ggplot(working, aes(x=year, y=median_mcs)) + 
  geom_line(size=2, alpha=0.5, color="green") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  ggtitle("Median Happiness Germany", subtitle = "(1-100)") +
  #scale_color_manual(values = colors)+ 
  coord_cartesian(ylim = c(49, 55))+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())
  
soep_ls <- ggplot(working, aes(x=year, y=avg_lifesat))+
  geom_line(size=2, alpha=0.5, color="darkorchid")+
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+ 
  ggtitle("Mean Life Satisfaction Germany", subtitle = "(0-10)") + 
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(5, 8)) +
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())

soep_median_ls <- ggplot(working, aes(x=year, y=median_lifesat))+
  geom_line(size=2, alpha=0.5, color="pink")+
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+ 
  ggtitle("Median Life Satisfaction Germany", subtitle = "(0-10)") + 
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(4, 7)) +
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())


combined_happiness_ls_soep <- arrangeGrob(soep_happiness, soep_ls, nrow = 1)
ggsave(file="../graphs/combined_happiness_ls_soep.png", combined_happiness_ls_soep)

