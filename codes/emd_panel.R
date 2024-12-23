library(tidyverse)


# fcensus to emd level variables----
data <- read_rds('data/fcensus(1994-2022).rds')


df <- data %>% mutate(
  sgg = str_split_i(sgg, ' ', -1), 
  emd = str_split_i(emd, ' ', -1),
  emd = if_else(str_detect(emd, '^\\d'), NA, emd)) %>% 
  filter(!is.na(emd)) %>% 
  group_by(year, sd, sgg, emd) %>% 
  summarise(
    n = n(),
    emp_reg = sum(emp_reg, na.rm = T),
    emp_temp = sum(emp_temp, na.rm = T),
    emp_total = sum(emp_total, na.rm = T)
  ) %>% ungroup()


write_rds(df, 'data/fcensus_emdpanel.rds')

data <- data %>% mutate(
  retail=if_else(industryD %in% c('도매 및 상품 중개업', '소매업; 자동차 제외', '자동차 판매 및 차량연료 소매업', '자동차 및 부품 판매업'), 1, 0),
  rest_and_hotel = if_else(industryD %in% c('숙박 및 음식점업', '음식점 및 주점업', '숙박업'), 1,0),
  rest = if_else(industryD == '음식점 및 주점업', 1, 0),
  commerce = if_else(retail==1|rest_and_hotel==1, 1, 0))

df_commerce <- data %>% mutate(
  sgg = str_split_i(sgg, ' ', -1), 
  emd = str_split_i(emd, ' ', -1),
  emd = if_else(str_detect(emd, '^\\d'), NA, emd)) %>% 
  filter(!is.na(emd)) %>% 
  group_by(year, sd, sgg, emd) %>% 
  summarise(
    n = n(),
    emp_reg = sum(emp_reg, na.rm = T),
    emp_temp = sum(emp_temp, na.rm = T),
    emp_total = sum(emp_total, na.rm = T),
    n_commerce = sum(commerce, na.rm = T),
    emp_total_commerce = sum(emp_total*commerce, emp_total, na.rm=T),
    n_retail = sum(retail, na.rm = T),
    emp_total_retail = sum(emp_total*retail, emp_total, na.rm=T),
    n_rest_and_hotel = sum(rest_and_hotel, na.rm = T),
    emp_total_rest_and_hotel = sum(emp_total*rest_and_hotel, emp_total, na.rm=T),
    n_rest = sum(rest, na.rm = T),
    emp_total_rest = sum(emp_total*rest, emp_total, na.rm=T)
) %>% ungroup()

write_rds(df_commerce, 'data/fcensus_emdpanel_commerce.rds')
