###SETUP ####
library(tidyverse)
library(haven)
library(writexl)
library(readxl)

###Select director as required and Import Data###
###Rich to Check - not sure how to deal with SPSS labels###
setwd("../Data/SPSS_EN_2022")
pequiv <- read_spss("pequiv.sav")
health <- read_spss("health.sav")
hbrutt <- read_spss("hbrutt.sav")

####Select Variables####

#Select post government income (i11102) from different file along with the number of persons in a household (d11106) and overall life satisfaction (p11101)
#Remove negative incomes#
pequiv_1 <- pequiv %>% select(cid, hid, pid,syear, i11102, d11106, p11101) %>% filter(!(i11102<0))


#Select MCS value in health. Additionally remove any mcs values which are negative or na#
health_1 <- health %>% select(cid, pid, syear, mcs) %>% filter(!mcs<0)%>% filter(!is.na(mcs))

#Separate out data into individual years in order to merge 
health_2002 <- health_1 %>%filter(syear==2002)
health_2004 <- health_1 %>%filter(syear==2004)
health_2006 <- health_1 %>%filter(syear==2006)
health_2008 <- health_1 %>%filter(syear==2008)
health_2010 <- health_1 %>%filter(syear==2010)
health_2012 <- health_1 %>%filter(syear==2012)
health_2014 <- health_1 %>%filter(syear==2014)
health_2016 <- health_1 %>%filter(syear==2016)
health_2018 <- health_1 %>%filter(syear==2018)
health_2020 <- health_1 %>%filter(syear==2020)

pequiv_2002 <- pequiv_1 %>% filter(syear==2002)
pequiv_2004 <- pequiv_1 %>% filter(syear==2004)
pequiv_2006 <- pequiv_1 %>% filter(syear==2006)
pequiv_2008 <- pequiv_1 %>% filter(syear==2008)
pequiv_2010 <- pequiv_1 %>% filter(syear==2010)
pequiv_2012 <- pequiv_1 %>% filter(syear==2012)
pequiv_2014 <- pequiv_1 %>% filter(syear==2014)
pequiv_2016 <- pequiv_1 %>% filter(syear==2016)
pequiv_2018 <- pequiv_1 %>% filter(syear==2018)
pequiv_2020 <- pequiv_1 %>% filter(syear==2020)

####Join required variables into dataframe and name columns ####

merge_2002 <- full_join(pequiv_2002, health_2002, by="pid") %>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2002) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2004 <- full_join(pequiv_2004, health_2004, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2004) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2006 <- full_join(pequiv_2006, health_2006, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2006) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2008 <- full_join(pequiv_2008, health_2008, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2008) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2010 <- full_join(pequiv_2010, health_2010, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2010) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2012 <- full_join(pequiv_2012, health_2012, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2012) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2014 <- full_join(pequiv_2014, health_2014, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2014) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2016 <- full_join(pequiv_2016, health_2016, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2016) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2018 <- full_join(pequiv_2018, health_2018, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2018) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

merge_2020 <- full_join(pequiv_2020, health_2020, by="pid")%>% select(cid.x, pid, syear.x, i11102, d11106, mcs,p11101)
colnames(merge_2020) <- c("hh_id", "person_id", "year", "income", "hh_size", "mcs","life_sat")

####adjust for inflation and put all prices in 2023 prices using the following website - CHECK THIS IF SHOULD BE DONE DIFFERENTLY OR AUTOMATICALLY###
#https://www.inflationtool.com/euro-germany?amount=1&year1=2016&year2=2018

merge_2002 <- merge_2002 %>% mutate(income_ann = income*1.51/sqrt(hh_size))
merge_2004 <- merge_2004 %>% mutate(income_ann = income*1.47/sqrt(hh_size))
merge_2006 <- merge_2006 %>% mutate(income_ann = income*1.42/sqrt(hh_size))
merge_2008 <- merge_2008 %>% mutate(income_ann = income*1.35/sqrt(hh_size))
merge_2010 <- merge_2010 %>% mutate(income_ann = income*1.33/sqrt(hh_size))
merge_2012 <- merge_2012 %>% mutate(income_ann = income*1.28/sqrt(hh_size))
merge_2014 <- merge_2014 %>% mutate(income_ann = income*1.25/sqrt(hh_size))
merge_2016 <- merge_2016 %>% mutate(income_ann = income*1.24/sqrt(hh_size))
merge_2018 <- merge_2018 %>% mutate(income_ann = income*1.2/sqrt(hh_size))
merge_2020 <- merge_2020 %>% mutate(income_ann = income*1.18/sqrt(hh_size))

###SAVE RESULTS####
df_all <- list(merge_2002, merge_2004, merge_2006, merge_2008, merge_2010, merge_2012, merge_2014, merge_2016, merge_2018, merge_2020)
processed_soep <- bind_rows(df_all)
write_rds(processed_soep, "../results/processed_soep.rds")
