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
library(geosphere)
register_google('AIzaSyCF9wnpxXfS31aA76QczHlaaupxtBTx__E')
ggmap::register_stadiamaps("e9e7b9a7-7f8a-47e3-b581-084e98290ce8", write=TRUE)

data <- read_csv('data/소상공인_우용_변환.csv')
data %>% view()

gondae <- get_map('건대입구역', zoom = 17)
banghak <- get_map('방학역', zoom = 17)
dangsan <- get_map('당산역', zoom = 17)

address <- tibble(address = c('상계역')) %>% mutate_geocode(address)
circle <- tibble(lon = address$lon + cos(seq(0, 2*pi, length.out=100))*0.002,
                 lat = address$lat + sin(seq(0, 2*pi, length.out=100))*0.002)

ggmap(sangye, zoom=17, base_layer = ggplot(data=data)) +
  geom_polygon(data = circle, aes(x = lon, y = lat), fill = "red", alpha = 0.2) +
  geom_point(aes(x=`경도`, y=`위도`), alpha = 0.3, color='darkblue') +
  theme_void()

# Get the bounding box of the map
map_bbox <- attr(gondae, "bb")
map_bbox <- attr(banghak, "bb")
map_bbox <- attr(dangsan, "bb")

# Extract bounding box coordinates
bbox_left <- map_bbox$ll.lon
bbox_right <- map_bbox$ur.lon
bbox_bottom <- map_bbox$ll.lat
bbox_top <- map_bbox$ur.lat

# Filter the data based on bounding box
gondae_data <- data %>%
  filter(경도 >= bbox_left & 경도 <= bbox_right & 위도 >= bbox_bottom & 위도 <= bbox_top)
banghak_data <- data %>%
  filter(경도 >= bbox_left & 경도 <= bbox_right & 위도 >= bbox_bottom & 위도 <= bbox_top)
dangsan_data <- data %>%
  filter(경도 >= bbox_left & 경도 <= bbox_right & 위도 >= bbox_bottom & 위도 <= bbox_top)
gondae_data <- gondae_data %>% mutate(station='건대입구역')
banghak_data <- banghak_data %>% mutate(station='방학역')
dangsan_data <- dangsan_data %>% mutate(station='당산역')
price <- rbind(gondae_data, banghak_data, dangsan_data)
price <- price %>% filter(`상권업종대분류명`=='음식')


write_csv(price, 'data/price.csv')
write_csv(banghak_data, 'data/banghak.csv')
write_csv(dangsan_data, 'data/gondae.csv')
