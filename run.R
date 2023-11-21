# Run file created by RMorris 

#### Import ####
source("src/import_SOEP.R")



#### Preprocessing ####
# includes removal of missing values (please check!)
source("src/preprocess_SOEP.R")



#### Model fitting ####
source("src/fit_SOEP_MCS.R")


#### Plot changepoints ####
source("scr/plot_SOEP_MCS_CPs.R")
