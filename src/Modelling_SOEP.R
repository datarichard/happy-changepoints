### SETUP and Load Processed Data ###
library(segmented) # select!!!
library(tidyverse)
library(haven)

processed_soep <- read_rds("results/processed_soep.rds")
processed <- processed_soep %>% mutate_all(as_factor)

###Group Processed German data by year and then utilise segmented package to determine change points in Affective Wellbeing- Psi###
###Being consistent with UKHLS Data, Focus on 2008 onwards
processed_soep <- processed_soep %>% filter(year > 2007)
processed_soep %>%
  group_by(year)%>%
  nest() -> soep_nested


###Investigate ChangePoints for Affective Wellbeing###

soep_nested %>%
  mutate(
    fit = map(.x=data, .f=~segmented(lm(
      formula = mcs ~ income_ann,
      data=.x),
      seg.Z = ~income_ann))
  ) -> soep_segmented


knot <- soep_segmented %>% 
  mutate(
    psi = map_dbl(.x=fit,
                  .f = ~summary(.x)$psi[, 'Est.'])
  )

knot <- knot %>% select(year, psi) %>% mutate(changepoint = "happiness")

###Investigate Knot Points for Life Satisfaction ###
soep_nested %>%
  mutate(
    fit = map(.x=data, .f=~segmented(lm(
      formula = life_sat ~ income_ann,
      data=.x),
      seg.Z = ~income_ann))
  ) -> soep_segmented_1


knot_1 <- soep_segmented_1 %>%
  mutate(
    psi = map_dbl(.x=fit,
                  .f = ~summary(.x)$psi[, 'Est.'])
  )

knot_1 <- knot_1 %>% select(year, psi) %>% mutate(changepoint = "life_satisfaction")

###Export Models to be ready for graphing###
knots <- list(knot, knot_1) 
changepoints_soep <- bind_rows(knots)

write_rds(changepoints_soep, "results/models_soep.rds")


