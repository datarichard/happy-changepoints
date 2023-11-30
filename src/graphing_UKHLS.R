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
working <- working_nested%>%
   mutate(avg_mcs = map_dbl(data,~{mean(.x$mcs)}),median_mcs= map_dbl(data,~{quantile(.x$mcs, c(.50))}),avg_lifesat = map_dbl(data,~{mean(.x$life_sat)}),median_lifesat= map_dbl(data,~{quantile(.x$life_sat, c(.5))}) )


colors <- c("Average MCS" = "Blue", "Median MCS" = "Red", "Average Life Sat" = "Green", "Median Life Sat"= "Pink")
uk_happiness <- ggplot(working, aes(x=year, y=median_mcs)) + 
  geom_line(size=2, alpha=0.5, color="green") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  ggtitle("Median Happiness", subtitle = "(1-100)") +
  #scale_color_manual(values = colors)+ 
  coord_cartesian(ylim = c(49, 53))+
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())
  
uk_ls <- ggplot(working, aes(x=year, y=avg_lifesat))+
  geom_line(size=2, alpha=0.5, color="darkorchid")+
  scale_x_continuous(breaks = seq(2010, 2020, by = 2))+ 
  ggtitle("Mean Life Satisfaction", subtitle = "(0-10)") + 
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(4.5, 5.5)) +
  ylab("")+
  theme_minimal()+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank())

uk_median_ls <- ggplot(working, aes(x=year, y=median_lifesat))+
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


### Produce Individual Plots for each year###
filtered_uk <- filter(processed_uk, year %in% c("2010", "2012", "2014", "2016", "2018", "2019", "2020") )
filtered_uk%>% 
  mutate(
    income_bin = ntile(income_ann,20)
  ) -> ntile_uk


ntile_uk %>% group_by(year, income_bin) %>% 
  summarise(
    wellbeing = mean(mcs),
    income = mean(income_ann, na.rm=T)/1000
  ) -> summary_table


filtered_uk %>% mutate(
  income = income_ann/1000, 
  wellbeing = mcs
) -> df.plot 
df.plot

  ggplot(df.plot, aes(x = income, y = wellbeing)) +
    geom_smooth(aes(fill = "manual1", color = "manual2"),
      method = "lm", 
      size = 0.5
      ) +
    coord_cartesian(xlim = c(0, 80),
                  ylim = c(46, 52)) +
    facet_wrap(~year, nrow = 1) +
    theme_test() +
     labs(subtitle = "Linear Regression of Happiness on Income",
         x = "Income (£000s)",
         y = "Wellbeing") +
    theme(
      legend.position = "none") -> p1
  
###Save ggplot###  
linear_uk <- p1 + geom_point(data = summary_table)
ggsave(file="../graphs/linear_uk.png", linear_uk)


###Produce Logarithmic linear model and then plot this for each year###
  
filtered_uk %>%
    group_by(year) %>% mutate(log_income = log(income_ann)) %>%
    mutate(
      income_bin = ntile(log_income, 20)
      ) %>%
    group_by(year, income_bin) %>%
    summarise(
      wellbeing = mean(mcs, na.rm=T),
      income = mean(log_income, na.rm=T)/1000
    ) -> log.df.deciles

ggplot(log.df.deciles, aes(x = income, y = wellbeing)) +
    geom_smooth(aes(fill = "manual1", color = "manual2"),
                method = "lm", formula = y ~ x,
                size = 0.5
    ) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = "Logarithmic Regression of Happiness on Income",
         x = "Log of Income (£000s)",
         y = "Wellbeing") +
    theme_test() +
    theme(
      legend.position = "none",
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) -> uk_p2

###Save ggplot###  
log_uk <- uk_p2 + geom_point(data = log.df.deciles) 
ggsave(file="../graphs/log_uk.png", log_uk)


###Produce piecewise-log model and then plot this for each year###
filtered_uk %>%
    group_by(year) %>%
    summarise(n = n()) %>%
    mutate(note = paste("n =", format(n, big.mark = ","))) -> df.sum

filtered_uk %>%
    mutate(
      wealth = income_ann/1000,
      wellbeing = mcs
    ) -> df.plot

filtered_uk %>%
    group_by(year) %>%
    mutate(
      dollar_bin = ntile(income_ann, 20)
      ) %>%
    group_by(year, dollar_bin) %>%
    summarize(
      wellbeing = mean(mcs, na.rm=T),
      wealth = mean(income_ann, na.rm=T)/1000,
      n = n()
    ) -> df.deciles

library(segmented)

  df.plot %>%
    group_by(year) %>%
    nest() %>%
    transmute(
      fit = map(data, 
                ~segmented(lm(formula = wellbeing ~ wealth, data = .x),
                                 seg.Z = ~wealth)),
      knot = map_dbl(fit, ~summary(.x)$psi[2])
    ) %>%
      pull(knot, name = year) -> knots
  
  detach("package:segmented", unload = TRUE)
  detach("package:MASS", unload = TRUE)
  
  years = as.numeric(names(knots))

  log_uk <- uk_p2 + geom_point(data = log.df.deciles) 

  
pw_lm_uk <- ggplot(df.deciles, aes(x = wealth, y = wellbeing)) +
    geom_point() +
   # geom_text(data = df.sum, aes(x = Inf, y = -Inf, label = note),
   #           hjust = 1.1, vjust = -.75, colour = 1, size = 3) +
    geom_smooth(data = filter(df.plot, year == years[1]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[1]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == years[2]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[2]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == years[3]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[3]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == years[4]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[4]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == years[5]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm,
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[5]),
                size = 0.5) +
  geom_smooth(data = filter(df.plot, year == years[6]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm,
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[6]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == years[7]),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm,
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[7]),
                size = 0.5) +
    coord_cartesian(xlim = c(0, max(df.deciles$wealth)),
                    ylim = c(min(df.deciles$wellbeing), max(df.deciles$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = "Linear-Piecewise Regression of Happiness on Income  ",
         x = "Income (£000s)",
         y = "Wellbeing") +
    theme_test() +
    theme(legend.position = "none")  

###Save ggplot###  
ggsave(file="../graphs/pw_lm_uk.png", pw_lm_uk)


