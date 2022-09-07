# ..... PROVINCIAS Y PARQUES NACIONALES .......
# .............................................

#      .) Si usamos el SIMPLIFY el tamaño varia


#      .) all_polygons = "96.1 Kb"
#      .) provincias   = "91.6 Kb"
#      .) catalunya    = "43.9 Kb"
#      .) ordesa       = "49.4 Kb"
#      .) aiguestortes = "34.8 Kb"



all_polygons <- sf::st_read("SHAPES/POLYGONS/all_polygons.shp") %>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)

parques <- sf::st_read("SHAPES/POLYGONS/all_polygons.shp") %>%
  dplyr::filter(Tipus == "OR" | Tipus == "AT") %>%
  rmapshaper::ms_simplify(0.5) %>%
  sf::st_transform(4326)

provincias <- all_polygons %>% dplyr::filter(Tipus == "PROV")  
catalunya <- all_polygons %>% dplyr::filter(CCAA == "Cataluña")

ordesa <- parques %>% dplyr::filter(Tipus == "OR")  
aiguestortes <- parques %>% dplyr::filter(Tipus == "AT")  


# format(object.size(all_polygons), units = "auto")
# format(object.size(provincias), units = "auto")
# format(object.size(catalunya), units = "auto")
# 
# format(object.size(ordesa), units = "auto")
# format(object.size(aiguestortes), units = "auto")
