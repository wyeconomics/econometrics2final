library(ggmap)
library(tidyverse)
theme_set(theme_void(base_family='AppleGothic'))
register_google('AIzaSyCF9wnpxXfS31aA76QczHlaaupxtBTx__E')
ggmap::register_stadiamaps("e9e7b9a7-7f8a-47e3-b581-084e98290ce8", write=TRUE)
# dir.create('figures')


data1 <- read_csv('data/subway_1fin.csv')
data2 <- read_csv('data/소상공인_우용_변환.csv') #restaurants
# data3 <- read_csv('data/micro_seoul.csv') # dataset of train station

df1 <- data1 %>% filter(str_detect(location, '구$')) %>% 
  mutate(full_name = paste0(name, '역 ', line,'호선'))
df2 <- data2 %>% 
  filter(상권업종대분류명 != '숙박') %>%     # filter(상권업종대분류명 == '음식')
  select(상호명, 행정동명, 도로명, 경도, 위도)

for(i in 1:nrow(df1)){
  x <- df1$full_name[i]
  map <- get_map(x, zoom=17)
  address <- tibble(address = c(x)) %>% mutate_geocode(address)
  circle <- tibble(lon = address$lon + cos(seq(0, 2*pi, length.out=100))*0.002,
                   lat = address$lat + sin(seq(0, 2*pi, length.out=100))*0.002)
  ggmap(map) +
    geom_polygon(data = circle, aes(x = lon, y = lat), fill = "red", alpha = 0.2) +
    geom_point(aes(lon,lat), data = address, color = 'blue') +
    geom_point(aes(경도, 위도), data = df2, color = 'black', size = 1)
  photo <- paste0('figures/radius(', x, ').png')
  ggsave(photo)
}


# yadang <- get_map('야당역', zoom =17)
# tb <- tibble(address=c('야당역'))
# address <- tb %>% mutate_geocode(address)
#   
# circle <- tibble(lon = address$lon + cos(seq(0, 2*pi, length.out=100))*0.002,
#                      lat = address$lat + sin(seq(0, 2*pi, length.out=100))*0.002)
# 
# ggmap(yadang) +
#   geom_polygon(data = circle, aes(x = lon, y = lat), fill = 'red', alpha = 0.2) +
#   geom_point(aes(lon,lat), data = address, color = 'blue')
