library(ggplot2)
library(sf)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(rnaturalearth)


china <- readShapePoly(
  "D:/master_study/fills/data/geo_data/bou2_4p.shp")
plot(china)
name <- china$NAME

china1 <- read_sf(
  "D:/master_study/fills/data/geo_data/bou2_4p.shp")
china1$NAME <- name


value <- read.csv(
  'D:/master_study/fills/r/domestic/地图/2017年.csv')
value$NAME <- value$省份

china_map <- merge(china1,value,all.x=T)

st_crs(china_map)

china_map <- st_set_crs(china_map,4326)
china_map <- st_transform(china_map, "+init=epsg:4508")

p2 <- ggplot(china_map) +
  geom_sf(aes(fill = 总值))+
  scale_fill_gradient(low='white',high='red')+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks =element_blank(),
    axis.title = element_blank(),
    legend.position = 'right'
  )+labs(title = '2017年')

p2

ggsave('2017.png',p2,width = 6,height = 4,dpi = 300)


