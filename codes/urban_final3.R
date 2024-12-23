#-----------------------------------
# (1) preamble
#-----------------------------------

library(sf)
library(ggmap)
library(fixest)
library(kableExtra)
library(broom)
library(stargazer)
library(rempsyc)
library(rebus)
library(texreg)
library(knitr)
library(tidyverse)
library(rebus)

#-----------------------------------
# (5) 기업 단위 매출
#-----------------------------------

data5 <- read_delim('data/mdis_dong.txt', delim='\t')
data5 <- data5 %>% select(-sgg.x) %>% mutate(onland = if_else(n_onland>0, 1,0),
                                             station = if_else(n_total>0, 1,0),
                                             line1 = if_else(line1_number>0, 1,0)) %>% 
  rename(sgg = sgg.y)

##full employment
#baseline OLS
all_sector_n_OLS <- feols(workers ~ n_total + n_onland:n_total | sgg + sector, data = data5)
all_sector_n_IV <- feols(workers ~ n_total| sgg + sector |  n_onland:n_total ~ line1_number, data = data5)

#all_sector_OLS <- feols(workers ~ station + onland:station |sgg + sector, data = data5)
#all_sector_IV <- feols(workers ~ station + station:onland | sgg + sector | onland ~ line1, data = data5)

##restaurant employment
data5_rest <- data5 %>% filter(sector=='I')
rest_OLS <- feols(workers ~ n_total + n_onland:n_total | sgg, data = data5_rest)
rest_IV <- feols(workers ~ n_total | sgg | n_onland:n_total ~ line1_number, data = data5_rest)

#rest_IV <- feols(workers ~ station | sgg | station*onland ~ line1, data = data5_rest)
screenreg(list(all_sector_n_OLS, all_sector_n_IV, rest_OLS, rest_IV),
          custom.header = list('All Sector'=1:2, 'Restaurantts' =3:4),
          custom.model.names = c('OLS', 'IV', 'OLS', 'IV'),
          caption = 'Regression on all firms')

"
texreg(l=list(all_sector_OLS, rest_OLS, all_sector_IV, rest_IV),
       'temp/firm_reg.tex',
       custom.header = list('OLS'=1:2, 'IV' =3:4),
       caption = 'Regression on all firms')
       "