# Import SOEP data from source

#### Setup ####
library(tidyverse)
library(haven)

#### Import files ####
SOEP_files <- list.files("../data/SOEP/SPSS_EN")

# remember to deal with SPSS labels correctly
SOEP_health <- read_spss(SOEP_files[1])
SOEP_pequiv <- read_spss(SOEP_files[1])

                      
#### Select variables I want ####
mcs <- select(SOEP_health, mcs)
hh_income <- select(SOEP_pequiv, income)



#### Join the variables I want into a data frame ####
# dataframe_to_save <- left_join(mcs, hh_income, by = "xwaveid")

#### Save results ####
write_rds(dataframe_to_save, "../results/imported_SOEP.rds")
