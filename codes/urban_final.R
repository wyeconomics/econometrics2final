#-----------------------------------
# (1) preamble
#-----------------------------------

library(tidyverse)
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
register_google('AIzaSyCF9wnpxXfS31aA76QczHlaaupxtBTx__E')
ggmap::register_stadiamaps("e9e7b9a7-7f8a-47e3-b581-084e98290ce8", write=TRUE)

#-----------------------------------
# (2) data loading
#-----------------------------------

#데이터 전처리
data <- read_csv('data/subway_1fin.csv')         #지하철역 데이터
data <- data %>% mutate(name = paste0(name,'역'))

data2 <- data %>% mutate(line_1 = if_else(line =='1' | line=='경의선', 1,0)) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  group_by(location) %>% summarise(ratio = mean(level), line_1_ratio = mean(line_1), n_station = n()) %>% 
  mutate(seoul = if_else(str_detect(location, '구$'), 1,0))

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

#전국사업체조사
#별도의 R 파일

colnames(mdis21) = c("year", "sido", "sgg", "dong", "사업체구분코드", "sector", "n_workers")
colnames(mdis19) = c("sido", "sgg", "dong", "sector", "n_workers")

#-----------------------------------
# (3) map loading
#-----------------------------------

#spatial distribution of the stations
seoul <- get_map('Seoul', zoom = 9, source='stadia',
                 maptype='stamen_toner_lite')
sseoul2 <- get_map('Seoul', zoom = 11, source='stadia',
                 maptype='stamen_terrain')

sig <- st_read('data/sig.shp', options='ENCODING=CP949') %>%
  filter(str_detect(SIG_CD, "^11")|str_detect(SIG_CD, "^28")|str_detect(SIG_CD, "^41")) %>% 
  st_set_crs(5179) %>% st_transform(crs=4326) 
#spatial <- data %>% mutate_geocode(name)
#write_csv(spatial, file = ('data/지하철역.csv'))
spatial <- read_csv('data/지하철역.csv')

#-----------------------------------
# (4) main analysis - part 1
#-----------------------------------

ggmap(seoul, zoom=9, base_layer = ggplot(data=spatial)) +
  geom_sf(data=sig, color='darkblue', alpha = 0, inherit.aes = FALSE) +
  geom_point(aes(x=lon, y=lat, color=factor(level)), alpha = 0.4) +
  scale_color_discrete(name = NULL, labels=c('underground stations(409)', 'upper gound stations(248)')) +
  labs(title = 'Spatial Distribution of Subway Stations in Seoul') +
  theme_void() 
ggsave('temp/spatial_distribution.png')

#고용 분석
regional_labor <- data2 %>% left_join(LF, by = c('location' = 'district'))

ggplot(regional_labor, aes(ratio, employment_rate)) +
  geom_point() +
  geom_smooth(method = 'lm')

#소득 분석
regional_gdp <- gdp %>% filter(district != '소계') %>% left_join(data2, by = c('district' = 'location')) %>% 
 left_join(pop, by=c('district' = 'name'))

ggplot(regional_gdp %>% filter(sector ==	'지역내총생산(시장가격)'), aes(x=ratio, y=log(gdp/pop))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_y_continuous(name = 'log(Gross Regional Product / District Population)', labels = scales::label_comma()) +
  labs(title = 'Total', x = 'ratio of upper ground stations', caption = 'unit: 시군구\ndata source: KOSIS') +
  theme(text = element_text(family = 'AppleGothic'))
#ggsave('temp/total_gdp.png')

feols(fml = log(gdp/pop) ~ ratio + seoul, data = regional_gdp %>% filter(sector== '지역내총생산(시장가격)'))

"  
#소득 분석(2)
regional_gdp2 <- data %>% left_join(gdp %>% filter(district!='소계'), by = c('location' = 'district')) %>% 
  left_join(pop, by = c('location' = 'name')) %>% 
  mutate(seoul = str_detect(location, '구$'))

ggplot(regional_gdp2 %>% filter(sector ==	'지역내총생산(시장가격)'), aes(x=level, y=log(gdp/pop))) +
  geom_col()

#feols(fml = log(gdp/pop) ~ level + seoul, data = regional_gdp2 %>% filter(sector =='지역내총생산(시장가격)' ))
feols(fml = log(gdp/pop) ~ level + seoul, data = regional_gdp2 %>% filter(sector  %in%	c('숙박 및 음식점업', 	'문화 및 기타 서비스업')))
#feols(fml = log(gdp/pop) ~ seoul|level ~ if_else(line%in%c('1', '경의선'), 1,0) , data = regional_gdp2 %>% filter(sector =='지역내총생산(시장가격)'))
feols(fml = log(gdp/pop) ~ seoul|level ~ if_else(line%in%c('1', '경의선'), 1,0) , data = regional_gdp2 %>% filter(sector  %in%	c('숙박 및 음식점업', 	'문화 및 기타 서비스업')))
"

#숙박 및 음식점업, 	문화 및 기타 서비스업
regional_gdp2 <- regional_gdp %>% filter(sector %in%	c('숙박 및 음식점업', 	'문화 및 기타 서비스업')) %>% 
  group_by(district) %>% mutate(gdp=sum(gdp), sector ='숙박, 음식, 문화 서비스업') %>% distinct(district, .keep_all = T)
ggplot(regional_gdp2, aes(x=ratio, y=log(gdp/pop))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_y_continuous(name = 'log(Gross Regional Product / District Population)', labels = scales::label_comma()) +
  labs(title = 'Accomodation, Restaurant and Culture Businesses', x = 'ratio of upper ground stations', caption = 'unit: 시군구\ndata source: KOSIS') +
  theme(text = element_text(family = 'AppleGothic'))
ggsave('temp/restaurant_and_culture_gdp.png')

L1 <- feols(fml = log(gdp/pop) ~ log(1+ratio) + seoul, data = regional_gdp2)
#texreg(l, file = 'reg.tex')

#-----------------------------------
# (5) IV Estimation
#-----------------------------------

#year built
ybuilt <- spatial %>% filter(!is.na(year_built)) %>%  mutate(year_built2 = cut_interval(year_built, length = 10, dig.lab = 4))
ggmap(seoul2, base_layer = ggplot(data=ybuilt)) +
  geom_sf(data=filter(sig, str_detect(SIG_CD, '^11')), color='darkblue', alpha = 0, inherit.aes = FALSE) +
  geom_point(aes(x=lon, y=lat, color=year_built2), alpha = 0.7) +
  scale_color_brewer(palette = 'Accent') +
  labs(title = 'Spatial Distribution of Subway Stations in Seoul') +
  theme_void(base_family = 'AppleGothic') 

# 1, 경의선
sig2 <- left_join(sig, pop, by=c('SIG_KOR_NM' = 'name')) %>% 
  left_join(gdp %>% filter(sector=='지역내총생산(시장가격)'),by=c('SIG_KOR_NM' = 'district')) %>% 
  mutate(gdp_capita=as.numeric(gdp)/pop, gdp=as.numeric(gdp), group=cut_interval(gdp, n=7))

ggmap(seoul2, base_layer = ggplot(data=spatial %>% filter(line =='1'))) +
  geom_sf(aes(fill=group), data=sig2 %>% filter(str_detect(SIG_CD, '^11')), color='darkblue', inherit.aes = FALSE) +
  geom_point(aes(x=lon, y=lat, color=factor(level))) +
  labs(title = 'Dark Blue Line(Line 1)') +
  scale_fill_brewer(palette = 'YlOrRd') +
  theme_void(base_family = 'AppleGothic')  +
  guides(fill ='none') +
  labs(caption = 'Filled color represents the regional GDP of each district.')
ggsave('temp/line_1(gdp).png')

L2 <- feols(fml = log(gdp/pop) ~  seoul | log(1+ratio) ~ line_1_ratio, data = regional_gdp2)
texreg(list(L1, L2), file='reg.tex', custom.model.names = c('OLS', 'IV'))
screenreg(list(L1, L2), custom.model.names = c('OLS', 'IV'))
stargazer(L1, L2)

#-----------------------------------
# (6) main analysis - part 2
#-----------------------------------
#micro <- data %>% distinct(name, .keep_all = TRUE) %>% group_by(level) %>% sample_n(20)
#write_csv(micro, 'data/micro_analysis.csv')
#micro_seoul <- data %>% distinct(name, .keep_all = TRUE) %>% filter(str_detect(location, '구$')) %>% group_by(level) %>% sample_n(20)
#write_csv(micro_seoul, 'data/micro_seoul.csv')

micro_data <- micro %>% mutate(diff = abs((LEFT_DOWN-RIGHT_UP)), norm_diff = 2*diff/(LEFT_DOWN + RIGHT_UP))
micro_data %>% group_by(level) %>% summarise(diff = mean(diff), avg_count = mean(LEFT_DOWN + RIGHT_UP)) %>% 
  mutate(normalized_diff = diff/avg_count) 

ggplot(micro_data, aes(factor(level, labels = c('underground', 'on-land')), norm_diff)) +
  geom_boxplot() +
  scale_x_discrete(name =NULL) +
  scale_y_continuous(name ='normalized difference') +
  labs(title = 'Asymmetry in the Allocation of Restaurants',
       caption = '*Randomly sampled 20 subway stations from each group') +
  theme_bw() +
  theme(panel.background = element_rect(fill = "linen"))
ggsave('temp/micro_seoul.png')
  

stats.table <- t.test(norm_diff ~ level, data = micro_data)
stats.table <- tidy(stats.table, conf.int = TRUE)
nice_table(stats.table, broom = "t.test")
#texreg(t.test(norm_diff ~ level, data = micro_data), file = 't_test.tex')
