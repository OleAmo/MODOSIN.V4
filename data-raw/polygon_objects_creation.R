# polygons and nfi plots objects


# .......... COMARCAS / PROVINCIAS ...........
# ............................................

#      .) Creamos POLIGONOS estandards
#      .) Creamos POLÍONGOS simplificados
#      .) Usamos el ST_READ para NO CONECTAR con la BBDD

#      .) Calculamos el ÀREA en KM2
#      .) Lo usaremos para visualizarlo en el POPUP



comarcas <- st_read("SHAPES/COMARCAS/comarcas.shp") %>%
  dplyr::mutate(nom = nomcomar, area_km2 =as.numeric(round((st_area(geometry)/1000000),digits = 2)))  

provincias <- st_read("SHAPES/PROVINCIAS/provincias.shp")%>%
  dplyr::mutate(nom = nomprov, area_km2 =as.numeric(round((st_area(geometry)/1000000),digits = 2)))   

nucleos <- st_read("SHAPES/NUCLIS/nuclis.shp")

embass <- st_read("SHAPES/EMBASSAMENTS/embassaments.shp")
