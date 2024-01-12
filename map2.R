library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(devtools)
library(rnaturalearthhires)
library(geobr)
library(ggspatial)
library(elevatr)
library(raster)
library(metR)
library(ggrepel)
library(sf)
library(scales)

dados_coordenadas <- tribble(~Localidade, ~lat, ~lon,
                             'ULC', -19.837, -45.385,
                             'UJA', -20.422, -42.784,
                             'AGR', -19.393, -44.965,
                             'USV', -18.787, -50.244,
                             'BIO', -18.813, -48.601,
                             'UIR', -19.719, -50.229,
                             'UAV', -18.522, -49.135,
                             'UVG', -19.979, -48.145)


est <- geobr::read_state(code_state = "MG")
mun <- geobr::read_municipality(code_muni = "MG")
biom <- geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = T)


elevations <- elevatr::get_elev_raster(
  locations = est$geom, z = 7, clip = 'locations'
)

# Convert elevation data to dataframe
elevations <- as.data.frame(elevations, xy = TRUE)

colnames(elevations)[3] <- 'elevation'

river <- ne_download(scale = "large", type = 'rivers_lake_centerlines', 
                           category = 'physical', returnclass = "sf")

unir_state <- st_union(est)

river |> st_transform(st_crs(est)) |> st_intersection(unir_state) -> out_river

biom |> st_transform(st_crs(est)) |> st_intersection(unir_state) -> out_biom


ggplot() +
  geom_relief(
    data = elevations,
    aes(
      x = x, y = y, z = elevation, light = 'white', dark = 'gray4'
    ),
    raster = FALSE,
    interpolate = TRUE,
    sun.angle = 60
  ) +
  geom_sf(data = est, color = 'white', fill = NA) +
  coord_sf(datum = sf::st_crs(est)) +
  geom_sf(data = out_biom, aes(fill = name_biome), alpha = 0.4) +
  geom_sf(data = out_river, color = 'blue') +
  labs(fill = "Biomes") +
  scale_fill_viridis_d() +
  
  geom_point(
    data = dados_coordenadas,
    aes(x = lon, y = lat),
    color = 'black',
    size = 1.5
  )  +
  geom_text(
    data = dados_coordenadas,
    aes(x = lon, y = lat, label = Localidade),
    hjust = 1.2,
    vjust = 1,
    size = 3.25
  ) +
  
  xlab('Latitude') +
  ylab('Longitude')  +
  theme_minimal() + # deve estar acima do theme, por isso, não mudava o tamanho das letras
  theme(
    axis.text = element_text(size = 10, color = 'black'),
    axis.title = element_text(size = 14, color = 'black', face = 'bold'),
    legend.text = element_text(size = 8, color = 'black'),
    legend.title = element_text(size = 10, color = 'black', face = 'bold')
    
  ) -> mp




ggsave('mp.jpg', mp,
       width = 200,
       height = 135,
       units = 'mm',
       dpi = 600)




est1 <- sf::st_transform(x = est, crs = 3857)
mun1 <- sf::st_transform(x = mun, crs = 3857)


ggplot()+
  geom_sf(data = mun, color = 'black', fill = 'white')+
  coord_sf(datum = sf::st_crs(mun1)) + 
  geom_point(data = dados_coordenadas,
             aes(x = lon, y = lat, color = Localidade), size = 2.5) +
  scale_color_manual(values = c("blue3", "blueviolet", "gold3", "mediumpurple4",
                                "mediumpurple1", "sienna3", "yellow2", "violetred1"))+
  theme_test() +
  labs(x = "Longitude",
       y = 'Latitude',
       color = 'Local',
       title = 'Mapa de Minas Gerais')+
  annotation_north_arrow(style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "br", height = unit(0.15, "cm")) +
  xlim(-51, -42) +
  ylim(-21, -18) -> mapa_tematico


ggmap(mapa_tematico)


