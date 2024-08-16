# Run file created by RMorris 

#### Import & preprocess ####
# Includes removal of missing values
# Adjustment to 2023 prices
source("src/import_SOEP.R")  # write results/processed_soep.rds
source("src/import_UKHLS.R") # write results/processed_uk.rds



#### Model fitting ####
source("src/modelling_SOEP.R") # write results/models_soep.rds
source("src/modelling_UKHLS.R")


#### Plot changepoints ####
source("scr/graphing_SOEP.R")
