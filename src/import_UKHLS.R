
#load packages, then select items from individual data for personal and HH identifers, followed by GHQ metric, job identifier (to be used to be removed students later), life satisfaction and MCS metric. 

library(tidyverse)
library(haven)
library(naniar)
library(writexl)
library(readxl)

#adjust working directory as needed to access data#
#Left in W_1_indresp_extra as this is the only file which includes all components of MCS, all others only have sf1 and then sf12mcs#
setwd('/Users/sam/Library/Mobile Documents/com~apple~CloudDocs/CODING/happy-changepoints/Data/UKHLS/ind_resp')
W_1_indresp_extra <- read_dta(file = "a_indresp.dta") %>% select(pidp, a_hidp, a_age_dv, a_scghq1_dv, a_sclfsato, a_sf12mcs_dv, a_sf1, a_sf2a, a_sf2b, a_sf3b, a_sf4a, a_sf4b, a_sf5, a_sf6a, a_sf6b, a_sf6c, a_sf7)
W_1_indresp <- W_1_indresp_extra %>% select(pidp, a_hidp, a_age_dv, a_scghq1_dv, a_sclfsato, a_sf12mcs_dv, a_sf1)
W_2_indresp <- read_dta(file = "b_indresp.dta") %>% select(pidp, b_hidp, b_age_dv, b_scghq1_dv, b_sclfsato, b_sf12mcs_dv, b_sf1)
W_3_indresp <- read_dta(file = "c_indresp.dta") %>% select(pidp, c_hidp, c_age_dv, c_scghq1_dv, c_sclfsato, c_sf12mcs_dv, c_sf1) 
W_4_indresp <- read_dta(file = "d_indresp.dta") %>% select(pidp, d_hidp, d_age_dv, d_scghq1_dv, d_sclfsato, d_sf12mcs_dv, d_sf1) 
W_5_indresp <- read_dta(file = "e_indresp.dta") %>% select(pidp, e_hidp, e_age_dv, e_scghq1_dv, e_sclfsato, e_sf12mcs_dv, e_sf1)
W_6_indresp <- read_dta(file = "f_indresp.dta") %>% select(pidp, f_hidp, f_age_dv, f_scghq1_dv, f_sclfsato, f_sf12mcs_dv, f_sf1)
W_7_indresp <- read_dta(file = "g_indresp.dta") %>% select(pidp, g_hidp, g_age_dv, g_scghq1_dv, g_sclfsato, g_sf12mcs_dv, g_sf1)
W_8_indresp <- read_dta(file = "h_indresp.dta") %>% select(pidp, h_hidp, h_age_dv, h_scghq1_dv, h_sclfsato, h_sf12mcs_dv, h_sf1)
W_9_indresp <- read_dta(file = "i_indresp.dta") %>% select(pidp, i_hidp, i_age_dv, i_scghq1_dv, i_sclfsato, i_sf12mcs_dv, i_sf1)
W_10_indresp <- read_dta(file = "j_indresp.dta") %>% select(pidp, j_hidp, j_age_dv, j_scghq1_dv, j_sclfsato, j_sf12mcs_dv, j_sf1)
W_11_indresp <- read_dta(file = "k_indresp.dta") %>% select(pidp, k_hidp, k_age_dv, k_scghq1_dv, k_sclfsato, k_sf12mcs_dv, k_sf1)
W_12_indresp <- read_dta(file = "l_indresp.dta",  encoding = "latin1") %>% select(pidp, l_hidp, l_age_dv, l_scghq1_dv, l_sclfsato, l_sf12mcs_dv, l_sf1)

setwd('/Users/sam/Library/Mobile Documents/com~apple~CloudDocs/CODING/happy-changepoints/Data/UKHLS//hh_resp')
W_1_hhresp <-read_dta(file = "a_hhresp.dta") %>% select(a_hidp, a_fihhmnnet1_dv, a_hhsize)
W_2_hhresp <-read_dta(file = "b_hhresp.dta") %>% select(b_hidp, b_fihhmnnet1_dv, b_hhsize)
W_3_hhresp <-read_dta(file = "c_hhresp.dta") %>% select(c_hidp, c_fihhmnnet1_dv, c_hhsize)
W_4_hhresp <-read_dta(file = "d_hhresp.dta") %>% select(d_hidp, d_fihhmnnet1_dv, d_hhsize)
W_5_hhresp <-read_dta(file = "e_hhresp.dta") %>% select(e_hidp, e_fihhmnnet1_dv, e_hhsize)
W_6_hhresp <-read_dta(file = "f_hhresp.dta") %>% select(f_hidp, f_fihhmnnet1_dv, f_hhsize)
W_7_hhresp <-read_dta(file = "g_hhresp.dta") %>% select(g_hidp, g_fihhmnnet1_dv, g_hhsize)
W_8_hhresp <-read_dta(file = "h_hhresp.dta") %>% select(h_hidp, h_fihhmnnet1_dv, h_hhsize)
W_9_hhresp <-read_dta(file = "i_hhresp.dta") %>% select(i_hidp, i_fihhmnnet1_dv, i_hhsize)
W_10_hhresp <-read_dta(file = "j_hhresp.dta") %>% select(j_hidp, j_fihhmnnet1_dv, j_hhsize)
W_11_hhresp <-read_dta(file = "k_hhresp.dta") %>% select(k_hidp, k_fihhmnnet1_dv, k_hhsize)
W_12_hhresp <-read_dta(file = "l_hhresp.dta") %>% select(l_hidp, l_fihhmnnet1_dv, l_hhsize)

#Merge individual and household files by household identifier and assign the start year of each wave
W_1_merge_ext <- full_join(W_1_indresp_extra, W_1_hhresp, by = "a_hipd") %>% mutate(Year = 2009)
W_1_merge <- full_join(W_1_indresp, W_1_hhresp, by="a_hidp") %>% mutate(Year = 2009)
W_2_merge <- full_join(W_2_indresp, W_2_hhresp, by="b_hidp") %>% mutate(Year = 2010) 
W_3_merge <- full_join(W_3_indresp, W_3_hhresp, by="c_hidp") %>% mutate(Year = 2011)
W_4_merge <- full_join(W_4_indresp, W_4_hhresp, by="d_hidp") %>% mutate(Year = 2012)
W_5_merge <- full_join(W_5_indresp, W_5_hhresp, by="e_hidp") %>% mutate(Year = 2013)
W_6_merge <- full_join(W_6_indresp, W_6_hhresp, by="f_hidp") %>% mutate(Year = 2014)
W_7_merge <- full_join(W_7_indresp, W_7_hhresp, by="g_hidp") %>% mutate(Year = 2015)
W_8_merge <- full_join(W_8_indresp, W_8_hhresp, by="h_hidp") %>% mutate(Year = 2016)
W_9_merge <- full_join(W_9_indresp, W_9_hhresp, by="i_hidp") %>% mutate(Year = 2017)
W_10_merge <- full_join(W_10_indresp, W_10_hhresp, by="j_hidp") %>% mutate(Year = 2018)
W_11_merge <- full_join(W_11_indresp, W_11_hhresp, by="k_hidp") %>% mutate(Year = 2019)
W_12_merge <- full_join(W_12_indresp, W_12_hhresp, by="l_hidp") %>% mutate(Year = 2020)

colnames(W_1_merge_ext) <- c("pidp", "hidp", "age", "ghq", "life_sat", "mcs", "sf1", "sf2a", "sf2b", "sf3b", "sf4a", "sf4b", "sf5", "sf6a", "sf6b", "sf6c", "sf7", "hh_net_income_month", "hh_size", "year")
colnames(W_1_merge) = colnames(W_2_merge) = colnames(W_3_merge) = colnames(W_4_merge) = colnames(W_5_merge) = colnames(W_6_merge) = colnames(W_7_merge) = colnames(W_8_merge) = colnames(W_9_merge) = colnames(W_10_merge) = colnames(W_11_merge) = colnames(W_12_merge)<- c("pidp", "hidp", "age", "ghq", "life_sat", "mcs", "sf1", "hh_net_income_month", "hh_size", "year")


#Adjust prices to October 2023 Prices using https://www.bankofengland.co.uk/monetary-policy/inflation/inflation-calculator
#Remove values which were not entered (NA values) or negative monthly incomes
W_1_merge <- W_1_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.52)
W_2_merge <- W_2_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.48)
W_3_merge <- W_3_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.41)
W_4_merge <- W_4_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.37)
W_5_merge <- W_5_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.34)
W_6_merge <- W_6_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.32)
W_7_merge <- W_7_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.32)
W_8_merge <- W_8_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.31)
W_9_merge <- W_9_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.28)
W_10_merge <- W_10_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.25)
W_11_merge <- W_11_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.22)
W_12_merge <- W_12_merge %>% filter(!(hh_net_income_month<0), !(is.na(hh_net_income_month))) %>% mutate(hh_net_income_ann = hh_net_income_month*12*1.21)

df_all <- list(W_1_merge, W_2_merge, W_3_merge, W_4_merge, W_5_merge, W_6_merge, W_7_merge, W_8_merge, W_9_merge, W_10_merge, W_11_merge, W_12_merge)
processed_uk <- bind_rows(df_all)

write_rds(processed_uk, "../results/processed_uk.rds")
