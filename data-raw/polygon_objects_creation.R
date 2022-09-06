# ..... PROVINCIAS Y PARQUES NACIONALES .......
# .............................................


#      .) Si usamos el SIMPLIFY el tamaño varia


all_polygons <- st_read("SHAPES/POLYGONS/all_polygons.shp")%>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)


provincias <- all_polygons %>% dplyr::filter(Tipus == "PROV")
catalunya <- all_polygons %>% dplyr::filter(CCAA == "Cataluña")
ordesa <- all_polygons %>% dplyr::filter(Tipus == "OR")
aiguestortes <- all_polygons %>% dplyr::filter(Tipus == "AT")



