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
register_google('AIzaSyCF9wnpxXfS31aA76QczHlaaupxtBTx__E')
ggmap::register_stadiamaps("e9e7b9a7-7f8a-47e3-b581-084e98290ce8", write=TRUE)

#-----------------------------------
# (2) data loading
#-----------------------------------

#데이터 전처리
data <- read_csv('data/subway_1fin.csv')         #지하철역 데이터
data <- data %>% mutate(name = paste0(name,'역'))


#regional gdp data
seoul_gdp <- read_csv('data/seoul_gdp.csv')
capital_gdp <- read_csv('data/gyeonggi_gdp.csv')
incheon_gdp <- read_csv('data/incheon_gdp.csv')
gdp <- rbind(seoul_gdp, capital_gdp, incheon_gdp) %>%
  mutate(gdp = as.numeric(gdp), gdp_adjusted = as.numeric(gdp_adjusted))


LF <- read_csv('data/employment_rate.csv')
LF <- LF %>% mutate(district = str_remove(district, '서울 '))

#regional population data
pop <- read_csv('data/sdg.csv')

#micro level analysis
micro <- read_csv('data/micro_seoul.csv')
dong <- read_csv('data/dong_seoul.csv') %>% mutate(station = paste0(station, '역'), 
                                                   dong = str_remove_all(dong, DGT)) %>% 
  mutate(dong = if_else(str_detect(dong, '동$'), dong, paste0(dong,'동')))

#전국사업체조사

source('codes/mdis2019.R', local=T, encoding = 'EUC-KR')
mdis19 <- mdis %>% tibble()

#colnames(mdis21) = c("year", "sido", "sgg", "dong", "사업체구분코드", "sector", "workers")
colnames(mdis19) = c("sido", "sgg", "dong", "sector", "workers")
mdis19 <- mdis19 %>% filter(sido=='서울특별시') %>% 
  mutate(dong = str_remove_all(dong, DGT) %>% str_remove_all('\\.')) %>% 
  mutate(dong = str_extract(dong, one_or_more(ALPHA) %R% END), year = 2019) %>% tibble()

#-----------------------------------
# (3) map loading
#-----------------------------------

#spatial distribution of the stations
seoul <- get_map('Seoul', zoom = 9, source='stadia',
                 maptype='stamen_toner_lite')
seoul2 <- get_map('Seoul', zoom = 11, source='stadia',
                  maptype='stamen_toner_lite')

sig <- st_read('data/sig.shp', options='ENCODING=CP949') %>%
  filter(str_detect(SIG_CD, "^11")|str_detect(SIG_CD, "^28")|str_detect(SIG_CD, "^41")) %>% 
  st_set_crs(5179) %>% st_transform(crs=4326) 
#spatial <- data %>% mutate_geocode(name)
#write_csv(spatial, file = ('data/지하철역.csv'))
spatial <- read_csv('data/지하철역.csv')

#-----------------------------------
# (4) 동 단위 가게 매출
#-----------------------------------
data4 <- dong %>% select(sgg_code, sgg, dong_code, dong, station_code, station) %>% 
  left_join(data %>% select(name, line, level, area, year_built), by=c('station' = 'name')) %>% 
  distinct(station, .keep_all = TRUE)


mdis4 <- mdis19 %>% mutate(rest = if_else(sector =='I', 1, 0)) %>% 
  group_by(sector, dong) %>% 
  summarise(n_workers = mean(workers, na.rm=T), n_rest_workers = mean(workers*rest, na.rm=T)) %>% 
  ungroup()
mdis4

data4_sum <- data4 %>% mutate(line1 = if_else(line %in% c('1', '경의선'), 1,0)) %>% 
  group_by(sgg, dong) %>% 
  summarise(ratio = mean(level, na.rm=T), 
            avg_area = mean(area, na.rm=T), 
            avg_yr = mean(year_built, na.rm=T), 
            line1_ratio = mean(line1, na.rm=T),
            line1_number = sum(line1, na.rm = T),
            n_onland = sum(level, na.rm=T),
            n_under = n() - n_onland,
            n_total = n(),
            onland_major = if_else(n_under < n_onland, 1,0)) %>% ungroup()
data4_sum

data4_analysis <- data4_sum %>% full_join(mdis4, by=c('dong'))
data4_rest <- data4_sum %>% full_join(mdis4, by=c('dong')) %>% filter(sector =='I')

##full employment
#baseline OLS
#all_sector_reg_1 <- feols(log(n_workers) ~ ratio | sgg + sector, data = data4_analysis)
all_sector_reg_2 <- feols(log(n_workers) ~ n_onland:n_total + n_total | sgg + sector, se = "cluster", data = data4_analysis)

#covariate OLS
#all_sector_reg_3 <- feols(log(n_workers) ~ ratio + avg_area + avg_yr| sgg + sector, data = data4_analysis)
all_sector_reg_4 <- feols(log(n_workers) ~ n_onland:n_total + n_total + avg_area + avg_yr| sgg + sector, cluster = c('sgg', 'sector'), se = "cluster", data = data4_analysis)

#baseline IV
#all_sector_reg_5 <- feols(log(n_workers) ~ n_total  | sgg + sector | ratio ~ line1_ratio, data = data4_analysis)
all_sector_reg_6 <- feols(log(n_workers) ~ n_total | sgg + sector | n_onland:n_total ~ line1_number,cluster = c('sgg', 'sector'), se = "cluster", data = data4_analysis)

#covariate IV
#all_sector_reg_7 <- feols(log(n_workers) ~ avg_area + avg_yr | sgg + sector | ratio ~ line1_ratio, data = data4_analysis)
all_sector_reg_8 <- feols(log(n_workers) ~ n_total + avg_area + avg_yr| sgg + sector | n_onland:n_total ~ line1_number, cluster = c('sgg', 'sector'), se = "cluster", data = data4_analysis)

screenreg(list( all_sector_reg_2,all_sector_reg_4,
                all_sector_reg_6, all_sector_reg_8))

texreg(l=list( all_sector_reg_2,all_sector_reg_4,
               all_sector_reg_6, all_sector_reg_8),
       'temp/all_sector_reg.tex',
       custom.header = list("OLS" = 1:2, "IV" = 3:4),
       caption = 'Regression on all sectors')

#rest
#baseline OLS
#rest_reg_1 <- feols(log(n_workers) ~ ratio | sgg + sector, data = data4_rest)
rest_reg_2 <- feols(log(n_workers) ~ n_onland:n_total + n_total | sgg, cluster='sgg',se = "cluster", data = data4_rest)

#covariate OLS
#rest_reg_3 <- feols(log(n_workers) ~ ratio + avg_area + avg_yr| sgg + sector, data = data4_rest)
rest_reg_4 <- feols(log(n_workers) ~ n_onland:n_total + n_total + avg_area + avg_yr| sgg ,cluster='sgg', se = "cluster", data = data4_rest)

#baseline IV
#rest_reg_5 <- feols(log(n_workers) ~ n_total  | sgg + sector | ratio ~ line1_ratio, data = data4_rest)
rest_reg_6 <- feols(log(n_workers) ~ n_total  | sgg | n_onland:n_total ~ line1_number, cluster='sgg', se = "cluster", data = data4_rest)

#covariate IV
#rest_reg_7 <- feols(log(n_workers) ~ avg_area + avg_yr | sgg + sector | ratio ~ line1_ratio, data = data4_rest)
rest_reg_8 <- feols(log(n_workers) ~ n_total + avg_area + avg_yr| sgg | n_onland:n_total ~ line1_number, cluster='sgg', se = "cluster", data = data4_rest)

screenreg(list( rest_reg_2,rest_reg_4,
                rest_reg_6, rest_reg_8))

texreg(l=list( rest_reg_2,rest_reg_4,
               rest_reg_6, rest_reg_8),
       'temp/rest_reg.tex',
       custom.header = list("OLS" = 1:2, "IV" = 3:4),
       caption = 'Regression on restaurants and accomodation')


ggplot(barplot, aes(n_onland, avg_rest_workers/avg_workers)) +
  geom_col() +
  scale_y_continuous(labels=scales::label_comma(scale=100, suffix='%')) +
  labs(title = '지역 전체 고용 대비 식당,여가,문화 시설 고용 비율',
       x='Onland Station Count', y='ratio') +
  theme_bw(base_family = "AppleGothic")


#-----------------------------------
# (5) 시군구 단위 기업 매출
#-----------------------------------
data_sgg <- data4 %>% filter(!is.na(level), !is.na(line)) %>% 
  mutate(line1 = if_else(line %in% c('1', '경의선'), 1,0)) %>% 
  group_by(sgg) %>% 
  summarise(n_total = n(), n_onland = sum(level, na.rm=T), n_line_1 = sum(line1, na.rm=T))

mdis_sgg <- mdis19  %>% 
  filter(!is.na(sector), !is.na(workers)) %>% 
  mutate(rest = if_else(sector =='I', 1, 0), sgg = str_remove(sgg, '서울특별시 '))

seoul_gdp_5 <- seoul_gdp %>% filter(sector =='숙박 및 음식점업'|sector=='지역내총생산(시장가격)') %>% 
  group_by(district) %>% mutate(rest_gdp=lead(gdp)) %>% filter(!is.na(rest_gdp)) %>% select(-gdp_adjusted) %>% 
  mutate(gdp=as.numeric(gdp), rest_gdp=as.numeric(rest_gdp))

data_sgg <- left_join(mdis_sgg, data_sgg, by=c('sgg')) %>% left_join(pop, by=c('sgg' = 'name')) %>% 
  left_join(seoul_gdp_5, by=c('sgg' = 'district')) %>% 
  left_join(LF %>% select(district, LF_ratio), by=c('sgg'='district')) %>% 
  rename(sector=sector.x) %>% select(-sector.y)
data_sgg_rest <- data_sgg %>% filter(rest==1)

all_sector_n_OLS <- feols(log(workers) ~ n_total + pop + LF_ratio + n_onland:n_total| sector, data = data_sgg)
all_sector_n_IV <- feols(log(workers) ~ n_total+ pop + LF_ratio  | sector |  n_onland:n_total ~ n_line_1, data = data_sgg)

rest_n_OLS <- feols(log(workers) ~ n_total  + pop + LF_ratio + n_onland:n_total, data = data_sgg_rest)
rest_n_IV <- feols(log(workers) ~ n_total + pop + LF_ratio |  n_onland:n_total ~ n_line_1, data = data_sgg_rest)


#-----------------------------------
# (6) 1호선으로 제한
#-----------------------------------
data_sgg_1 <- data4 %>% filter(!is.na(level), !is.na(line)) %>% 
  mutate(line1 = if_else(line == '1', 1,0)) %>% 
  group_by(sgg) %>% 
  summarise(n_total = n(), n_onland = sum(level, na.rm=T), n_line_1 = sum(line1, na.rm=T)) %>% 
  filter(n_line_1 > 0)
data_sgg_1 <- inner_join(mdis_sgg, data_sgg_1, by=c('sgg')) %>% left_join(pop, by=c('sgg' = 'name')) %>% 
  left_join(seoul_gdp_5, by=c('sgg' = 'district')) %>% 
  left_join(LF %>% select(district, LF_ratio), by=c('sgg'='district')) %>% 
  rename(sector=sector.x) %>% select(-sector.y)
data_sgg_rest_1 <- data_sgg_1 %>% filter(rest==1)

all_sector_n_OLS_1 <- feols(log(workers) ~ n_total + pop + LF_ratio + n_onland:n_total| sector, cluster = 'sgg', data = data_sgg_1)
rest_n_OLS_1 <- feols(log(workers) ~ n_total  + pop + LF_ratio + n_onland:n_total, cluster = 'sgg', data = data_sgg_rest_1)

screenreg(list(all_sector_n_OLS, all_sector_n_IV, rest_n_OLS, rest_n_IV),
          custom.header = list('OLS'=1:2, 'IV' =3:4),
          custom.model.names = c('All Sector', 'Restaurants','All Sector', 'Restaurants'),
          omit.coef = 'pop',
          custom.coef.map = list('n_total' = 'n_stations',
                                 'LF_ratio' = 'Labor Force Participation', 'n_total:n_onland' = 'n_stations*n_onland stations',
                                 'fit_n_onland:n_total' = 'fitted n_stations*n_onland stations'),
          caption = 'Baseline Model',
          digits = 4,
          include.adjrs= F)

texreg(l=list(all_sector_n_OLS, rest_n_OLS, all_sector_n_IV, rest_n_IV),
       'temp/mdis_sgg_reg.tex',
       custom.header = list('OLS'=1:2, 'IV' =3:4),
       custom.model.names = c('All Sector', 'Restaurants','All Sector', 'Restaurants'),
       omit.coef = 'pop',
       custom.coef.map = list('n_total' = 'n_stations',
                              'LF_ratio' = 'Labor Force Participation', 'n_total:n_onland' = 'n_stations*n_onland stations',
                              'fit_n_onland:n_total' = 'fitted n_stations*n_onland stations'),
       caption = 'Baseline Model',
       digits = 4,
       include.adjrs= F)




#extra
all_sector_reg_1 <- feols(log(n_workers) ~ onland_major | sgg + sector, se = "cluster", data = data4_analysis)

all_sector_reg_2 <- feols(log(n_workers) ~ onland_major + n_total | sgg + sector, se = "cluster", data = data4_analysis)

rest_reg_1 <- feols(log(n_workers) ~ onland_major | sgg, se = "cluster", data = data4_rest)

rest_reg_2 <- feols(log(n_workers) ~ onland_major + n_total | sgg, se = "cluster", data = data4_rest)



line1_all_sector_reg <- feols(log(n_workers) ~ onland_major | sgg + sector, se = "cluster", data = data4_analysis %>% filter(line1_number>0))
line1_all_sector_reg2 <- feols(log(n_workers) ~ onland_major + n_total | sgg + sector, se = "cluster", data = data4_analysis%>% filter(line1_number>0))
line1_rest_reg <- feols(log(n_workers) ~ onland_major | sgg, se = "cluster", data = data4_rest %>% filter(line1_number>0))
line1_rest_reg2 <-feols(log(n_workers) ~ onland_major + n_total | sgg, se = "cluster", data = data4_rest %>% filter(line1_number>0))



screenreg(list( all_sector_reg_1,all_sector_reg_2,
                rest_reg_1, rest_reg_2), digits = 2)



screenreg(list( line1_all_sector_reg, line1_all_sector_reg2,
                line1_rest_reg, line1_rest_reg2), digits =3)

texreg(l=list( all_sector_reg_1,all_sector_reg_2,
               rest_reg_1, rest_reg_2),
       'temp/mdis_sgg_reg_final.tex',
       custom.header = list('All Sectors'=1:2, 'Restaurants and Accomodation' =3:4),
       custom.coef.map = list('onland_major' ='onland_major', 'n_total' = 'n_stations'),
       caption = 'Baseline Model',
       digits = 2,
       include.adjrs= F)
