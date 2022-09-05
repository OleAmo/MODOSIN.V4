# ..... PROVINCIAS Y PARQUES NACIONALES .......
# .............................................


#      .) Si usamos el SIMPLIFY el tamaño varia

#                                         (factor 0.01) (factor 0.2)
#             .) aiguestortes_simplfy =        6.6 Kb      12.3 Kb
#             .) aiguestortes         =       36.5 Kb 

#             .) ordesa_simplfy       =        7.4 Kb      28.7 Kb
#             .) ordesa               =      119.5 Kb

#             .) provincias_simplfy =         17.5 Kb      52.5 Kb
#             .) provincias         =        214.8 Kb


# aiguestortes <- st_read("SHAPEs/PARQUES_NACIONALES/aiguestortes.shp")
# ordesa <- st_read("SHAPEs/PARQUES_NACIONALES/ordesa.shp")
# provincias <- st_read("SHAPEs/PROVINCIAS/provincias_modosin_4326.shp")

aiguestortes_simplfy <- st_read("SHAPES/PARQUES_NACIONALES/aiguestortes.shp") %>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)

ordesa_simplfy <- st_read("SHAPES/PARQUES_NACIONALES/ordesa.shp") %>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)

provincias_simplfy <- st_read("SHAPES/PROVINCIAS/provincias_modosin_4326.shp") %>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)

all_divisions_simplfy <- st_read("SHAPES/ALL/prov_ord_at.shp") %>%
  rmapshaper::ms_simplify(0.2) %>%
  sf::st_transform(4326)

