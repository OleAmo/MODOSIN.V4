# ..... PROVINCIAS Y PARQUES NACIONALES .......
# .............................................

#      .) Si usamos el SIMPLIFY el tamaño varia


#      .) all_polygons = "131 Kb"
#      .) parques      = "178 Kb"



all_polygons <- sf::st_read("SHAPES/POLYGONS/all_polygons.shp") %>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)

all_parques <- sf::st_read("SHAPES/POLYGONS/all_polygons.shp") %>%
  dplyr::filter(Tipus == "OR" | Tipus == "AT" | Tipus == "peri_OR" | Tipus == "peri_AT" ) %>%
  rmapshaper::ms_simplify(0.5) %>%
  sf::st_transform(4326)


#  PARQUES / PROVINCIAS
provincias <- all_polygons %>% dplyr::filter(Tipus == "PROV")  
catalunya <- all_polygons %>% dplyr::filter(Cod_CCAA == "09")

parques <- all_parques %>% dplyr::filter(Tipus == "OR" | Tipus == "AT")
ordesa <- all_parques %>% dplyr::filter(Tipus == "OR")  
aiguestortes <- all_parques %>% dplyr::filter(Tipus == "AT")  



# PERIMETRE
peri_total <- all_parques  %>% dplyr::filter(Tipus %in% c("peri_OR","peri_AT"))
peri_ordesa <- all_parques  %>% dplyr::filter(Tipus == "peri_OR")
peri_aiguestortes <- all_parques  %>% dplyr::filter(Tipus == "peri_AT")


# format(object.size(parques), units = "auto")
# format(object.size(all_polygons), units = "auto")


# format(object.size(all_polygons), units = "auto")
# format(object.size(provincias), units = "auto")
# format(object.size(catalunya), units = "auto")
# 
# format(object.size(ordesa), units = "auto")
# format(object.size(aiguestortes), units = "auto")
