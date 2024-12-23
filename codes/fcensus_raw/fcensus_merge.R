# data cleaning for fcensus

# Preambles----
library(tidyverse)
library(haven)


# Merging all years to one----
# All data are filtered into capital region through the do files.

data <- tibble()

for(i in 1994:2021){
  if(i<=2012){
    df <- read_delim(paste0('data/fcensus/data', i,'.txt')) %>% 
      rename(sd = v1, sgg = v2, emd = v3, industry = v4, industryD = v5, 
             emp_reg = v6, emp_temp = v7, emp_total = v8) %>% 
      mutate(year = i) %>% 
      select(year, sd, sgg, emd, industry, industryD, emp_reg, emp_temp, emp_total)
    data <- rbind(data, df)
  }
  else{
    df <- read_delim(paste0('data/fcensus/data', i,'.txt')) %>% 
      rename(year = v1, sd = v2, sgg = v3, emd = v4, industry = v5, industryD = v6, 
             emp_reg = v7, emp_temp = v8, emp_total = v9) %>% 
      select(year, sd, sgg, emd, industry, industryD, emp_reg, emp_temp, emp_total)
    data <- rbind(data, df)
  }
}

write_rds(data, 'data/fcensus(1994-2022).rds')
