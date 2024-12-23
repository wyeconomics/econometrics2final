#configure file for main studies
message('Preparing datasets')
packageStartupMessage('initializing...', appendLF = F)
# Preambles----
library(tidyverse)
library(synthdid)
library(did)
library(DIDmultiplegt)
library(DIDmultiplegtDYN)
library(Synth)
library(TwoWayFEWeights)
library(staggered)
library(fixest)
library(texreg)

mutate_random_numeric <- function(data, char_column, new_column) {
  set.seed(42)  # For reproducibility; you can remove or change this if desired
  unique_chars <- unique(data[[char_column]])
  char_to_num <- setNames(sample(1:length(unique_chars)), unique_chars)
  data[[new_column]] <- char_to_num[data[[char_column]]]
  return(data)
}

# Data Loading----
df <- read_rds('data/fcensus_emdpanel.rds')                     #depends on emd_panel.R
df_commerce <- read_rds('data/fcensus_emdpanel_commerce.rds')   #depends on emd_panel.R 
data1 <- read_rds('data/treatment(emd).rds')                    #depends on treatment_status.R
data2 <- read_rds('data/treatment(emd)-2.rds')                  #depends on treatment_status.R



# Creating df1(baseline study) ----
df1 <- df %>% left_join(data1, by = c('emd' = 'locationD'))
df1

df1 <- df1 %>% mutate(
  log_emp_total = log(emp_total),
  log_emp_reg = log(emp_reg),
  log_emp_temp = log(emp_temp),
  log_n = log(n)
)

df1 <- df1 %>% mutate_random_numeric('emd', 'emd')

df1 <- df1 %>% mutate(time_notyet = if_else(!is.na(time), time, 9999))

df1_upper <- df1 %>% filter(time_notyet == 9999 | (time_notyet != 9999 & level %in% c(1,2)))
df1_lower <- df1 %>% filter(time_notyet == 9999 | (time_notyet != 9999 & level %in% c(0,2)))


# Creatubg df1_commerce(baseline study) ----

df1_commerce <- df_commerce %>% left_join(data1, by = c('emd' = 'locationD')) 
df1_commerce

df1_commerce <- df1_commerce %>% mutate(
  log_emp_total = log(emp_total),
  log_emp_reg = log(emp_reg),
  log_emp_temp = log(emp_temp),
  log_n = log(n),
  log_emp_total_commerce = log(emp_total_commerce),
  log_emp_total_retail = log(emp_total_retail),
  log_emp_total_rest_and_hotel = log(emp_total_rest_and_hotel),
  log_emp_total_rest = log(emp_total_rest),
  log_n_commerce = log(n_commerce),
  log_n_retail = log(n_retail),
  log_n_rest_and_hotel = log(n_rest_and_hotel),
  log_n_rest = log(n_rest)
)

df1_commerce <- df1_commerce %>% mutate_random_numeric('emd', 'emd')

df1_commerce <- df1_commerce %>% mutate(time_notyet = if_else(!is.na(time), time, 9999))

df1_commerce_u <- df1_commerce %>% filter(time_notyet == 9999 | (time_notyet != 9999 & level %in% c(1,2)))
df1_commerce_l <- df1_commerce %>% filter(time_notyet == 9999 | (time_notyet != 9999 & level %in% c(0,2)))


# Creating df2(study with emds treated only once) ----

df2 <- df %>% left_join(data2, by = c('emd' = 'locationD')) %>% 
  rename(n_firm = n.x, n_station = n.y)

df2 <- df2 %>% filter(exclude==0)

df2 <- df2 %>% mutate(
  log_emp_total = log(emp_total),
  log_emp_reg = log(emp_reg),
  log_emp_temp = log(emp_temp),
  log_n = log(n_firm)
)

df2 <- df2 %>% mutate_random_numeric('emd', 'emd')

df2 <- df2 %>% mutate(time_notyet = if_else(!is.na(time), time, 9999))


df2_upper <- df2 %>% filter(time_notyet == 9999 | (time_notyet != 9999 & level == 1))
df2_lower <- df2 %>% filter(time_notyet == 9999 | (time_notyet != 9999 & level == 0))

rm(df, df_commerce)
Sys.sleep(1)
message(' done
        data1: treatment status(emd)
        data2: treatment status(emd, strict)
        df1, df1_lower, df1_upper: data1 + emd panel data
        df2, df2_lower, df2_upper: data2 + emd panel data
        df1_commerce, df1_commerce_l, df1_commerce_u: data1 + emd panel data(commerce)
        ')
