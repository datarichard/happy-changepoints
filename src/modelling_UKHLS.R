
library(tidyverse)
setwd("../results")
processed_uk <- read_rds("processed_uk.rds")


###Group Processed UK data by year and then utilise segmented package to determine change points in Affective Wellbeing- Psi###
processed_uk%>%
  group_by(year)%>%
  nest()-> uk_nested

library(segmented)
uk_nested %>%
  mutate(
    fit = map(.x=data, .f=~segmented(lm(
      formula = mcs ~ income_ann,
      data=.x),
      seg.Z = ~income_ann))
  ) -> uk_segmented
detach("package:segmented", unload = TRUE)
detach("package:MASS", unload = TRUE)

knot <- uk_segmented %>%
  mutate(
    psi = map_dbl(.x=fit,
                  .f = ~summary(.x)$psi[, 'Est.'])
  )
knot <- knot%>% select(year, psi) %>% mutate(changepoint = "happiness")


###Determine change in points in Life Satisfaction ###
library(segmented)
uk_nested %>%
  mutate(
    fit = map(.x=data, .f=~segmented(lm(
      formula = life_sat ~ income_ann,
      data=.x),
      seg.Z = ~income_ann))
  ) -> uk_segmented_sat
detach("package:segmented", unload = TRUE)

knot_1 <- uk_segmented_sat %>%
  mutate(
    psi = map_dbl(.x=fit,
                  .f = ~summary(.x)$psi[, 'Est.'])
  )

knot_1 <- knot_1%>% select(year, psi)%>% mutate(changepoint = "life_satisfaction")
knot_1

###Combine data for export of model ###
knots <- list(knot, knot_1) 
changepoints <- bind_rows(knots)

write_rds(changepoints, "../results/models_uk.rds")
