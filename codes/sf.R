library(sf)
library(ggmap)
library(ggsflabel)

register_google('AIzaSyCF9wnpxXfS31aA76QczHlaaupxtBTx__E')

map <- get_map('Bangladesh', zoom=7, source = 'stadia', maptype = 'stamen_toner_lite')

sig <- st_read('shp/BGD_adm2.shp', options='ENCODING=UTF-8')


ggmap(map, zoom=7) +
  geom_sf(data=sig, color='darkblue', alpha = 0, inherit.aes = FALSE) 