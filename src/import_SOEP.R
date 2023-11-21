# Import SOEP data from source

#### Setup ####
library(tidyverse)
library(haven)

#### Import files ####
SOEP_files <- list.files("../data/SOEP/SPSS_EN")

# import files to environment from SOEP_files

#### Preprocessing ####


#### Join data frame ####
# dataframe_to_save <- left_join()

#### Save results ####
write_rds(dataframe_to_save, "../results/preprocessed_SOEP.rds")
