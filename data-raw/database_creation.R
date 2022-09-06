
# ///////////////////////////////  MODOSIN  //////////////////////////////
# ///////////////////////////////////////////////////////////////////////


# %%%%%%%%%%%%%%%%    CREAR BBDD PRINCIPAL POSTGRESQL  %%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#     .) CONECTAR a la BBDD principal (POSGRESQL)
#     .) CREAR DE NUEVO la BBDD (CREAF_v4)
#     .) La nueva BBDD usarà el SUPERUSARIO (postgres)


conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
                 dbname = 'postgres',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = '12345database')

drop_database <- glue::glue_sql("
DROP DATABASE IF EXISTS creaf_v4 ;", .con = con)

create_database <- glue::glue_sql("
CREATE DATABASE creaf_v4 ;", .con = con)


RPostgres::dbExecute(con, drop_database)
RPostgres::dbExecute(con, create_database)
RPostgres::dbDisconnect(con)


# %%%%%%%%%%%%%%%%%%%%%%   GEOMETRÍA PLOTS_ID   %%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#      .) Cada día se crean 5283 archivos RDS del MEDFATE
#      .) Cada archivo RDS es UNA PARCELA
#      .) Cada PARCELA => tiene los 365 días de datos del WHATER BALANCE

#      .) En esta PRUEVA DE APP
#      .) Tengo un FOLDER en el LOCALHOST con 5283 RDS 
#      .) Después en el servidor, el Script, también buscará en una carpeta 
#      .) En esta carpeta habrá descargadas las 5283 y algo parcelas cada día


# .......... GEOMETRÍA PLOTS_ID .........
# .......................................

#     .) La geometría NO la da el MEDFATE 
#     .) La obtenemos de diferentes formas
#              .) PLOTS PERE          (Miquel Shape) 
#              .) PLOTS AIGUESTORTES  (Miquel TXT)
#              .) PLOTS ORDESSA       (Miquel TXT)
#              .) IFN4                (App NFI)

# ....... PLOTS PERE ..
# .....................

pere_sf <- sf::st_read('COORD_PLOTS/coords_topo.shp') %>% 
  dplyr::mutate(plot_id = paste0("S_",Id), geom = geometry) %>%
  dplyr::select(plot_id, geom)  %>% 
  sf::st_transform(crs = 4326)


# ...PLOTS AiguesTortes
# .....................

# CSV (txt transformado a CSV) 
aiguestortes_csv <- read.csv2('COORD_PLOTS/CSV/plots_AiguesTortes.csv') %>%
  dplyr::mutate(longitude = x, latitude = y) %>%
  dplyr::select(plot_id, longitude, latitude)


aiguestortes_sf <- st_as_sf(aiguestortes_csv, coords = c("longitude", "latitude"), 
                            crs = 25831, agr = "constant") %>% sf::st_transform(crs = 4326) %>%
  dplyr::mutate(geom = geometry)

# ...PLOTS ORDESA.....
# .....................

# CSV (txt transformado a CSV) 
ordesa_csv <- read.csv2('COORD_PLOTS/CSV/plots_Ordesa.csv') %>%
  dplyr::mutate(longitude = x, latitude = y) %>%
  dplyr::select(plot_id, longitude, latitude)


ordesa_sf <- st_as_sf(ordesa_csv, coords = c("longitude", "latitude"), 
                      crs = 25830, agr = "constant") %>% sf::st_transform(crs = 4326) %>%
  dplyr::mutate(geom = geometry)


# IFN4 ............... 
# .....................

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = 'catalunya', 
                      host = 'localhost',  
                      port = 5432,  
                      user = 'postgres',
                      password = '12345database')

parcelas_nfi <- sf::st_read(dsn = con, Id(schema="creaf", table = "nif_app"))

RPostgres::dbDisconnect(con)


# ...... CREACIÓN PLOTS GEOM ALL ........
# .......................................

#     .) Los GUARDO los 4 SF en el PC como shapes
#     .) Despues usando QGIS uniré los 3 shapes en 1 shape

# st_write(parcelas_nfi,'SHAPE/PLOTS/SEPARADOS/polts_nfi.shp')
# st_write(aiguestortes_sf,'SHAPE/PLOTS/SEPARADOS/polts_at.shp')
# st_write(pere_sf,'SHAPE/PLOTS/SEPARADOS/polts_p.shp')
# st_write(ordesa_sf,'SHAPE/PLOTS/SEPARADOS/polts_o.shp')



# .......... PLOTS GEOM ALL .............
# .......................................

#     .) Es una tabla con la relación PLOT_ID Geom
#     .) Son los Plots de:
#          .)  NFI
#          .)  AiguesTortes
#          .)  Pere
#     .) Lo he creado en QGis uniendo los 3 shapes creados

plots_geom_all <- st_read('SHAPES/PLOTS/plots_geom_all.shp') %>%
  dplyr::mutate(old_idparcela = old_dpr, old_idclasse_nfi3 = old_d_3, old_idclasse_nfi4 = old_d_4) %>%
  dplyr::select(plot_id, old_idparcela, old_idclasse_nfi3, old_idclasse_nfi4)




# %%%%%%%%%%%%%%%%%%%%%   REAR DATA FRAME FIRE   %%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# ........ OBTENER PLOT_ID Corregidos.........
# ............................................

#     .) Obtengo los nombres de los archivos (P_15081.rds)
#     .) les quito el .RDS
#     .) Y me quedo lo 2do (el plot_id = P_15081)

plots_id <- list.files("RDS", pattern="rds$") %>%
  gsub(".rds", "", ., fixed = TRUE)  


# ........ FUNCION PLOT_ORIGIN ...............
# ............................................
#     .) Obtengo los nombres de los archivos => "y_xxxxx.RDS"
#     .) Separo con SPLIT "_" y me quedo la primera parte
#     .) Después en función de si és P o A retorno IFN o AIGUESTORTES

plot_origin <- function(data){
  res <- data %>%
    str_split_fixed(.,"_", 2) %>%
    .[,1]
  
  if(res == "A"){
    return("aiguestortes")
  } else if (res == "P") {
    return("ifn")
  } else if (res == "S") {
    return("matollar")
  } else if (res == "O") {
    return("ordesa")
  }
  
}

# .......... CREAR DATA DAY FIRE .............
# ............................................

#     .) Necesitamos una LISTA para empezar el proceso
#     .) La LISTA tiene que tener un NAME y un VALUE
#          .) NAME = me da el id corregido
#          .) VALUE = el nombre del archivo

#     .) Usaremos purrr:: IMAT_DFR (hará un LOOP)
#          .) Creará un DATA FRAME
#          .) Usando el VALUE de la LISTA recorrerá cada archivo RDS

#          .) Cada archivo RDS, modificará fila a fila
#          .) Añadir PLOT_ID, DATE
#          .) Y una vez con el PLOT_ID añadir GEOM (via JOIN)

#      .) Por lo tanto, al final de 5283 RDS de 365 filas cada uno
#      .) Tendremos una DATA FRAME final de 1.928.295 filas


data_day_fire <- list.files("RDS", pattern="rds$") %>%
  set_names(plots_id) %>%
  purrr::imap_dfr(.f = function(archivo,id){
    readRDS(paste0("RDS/",archivo)) %>%
      dplyr::mutate(plot_id = id, date = as.Date(row.names(.)), plot_origin = plot_origin(archivo))}) %>%
  dplyr::left_join(
    select(plots_geom_all,plot_id),
    by = c("plot_id" ="plot_id"))




# %%%%%%%%%%%%%%%%%%%%%   INSERTAR SF a BBDD   %%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#     .) Creamos CONEXION
#     .) Creamos la TABLA con ST_WRITE
#     .) VACUUM y ANALIZE

# VACUUM
# recupera el almacenamiento ocupado por tuplas muertas. En el funcionamiento normal de PostgreSQL,
# las tuplas que se borran o quedan obsoletas por una actualización no se eliminan físicamente de su tabla;
# permanecen presentes hasta que se hace un VACUUM. Por lo tanto, es necesario hacer VACUUM periódicamente,
# especialmente en las tablas que se actualizan con frecuencia.


# ANALYZE
# recoge estadísticas sobre el contenido de las tablas de la base de datos y almacena los resultados
# en el catálogo del sistema pg_statistic. Posteriormente, el planificador de consultas utiliza estas
# estadísticas para ayudar a determinar los planes de ejecución más eficientes para las consultas.

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'creaf_v4',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = '12345database')

drop_table <- glue::glue_sql("
DROP TABLE IF EXISTS public.data_day_fire CASCADE;", .con = con)

vacuum_analyze <- glue::glue_sql("
VACUUM ANALYZE public.data_day_fire;", .con = con)


# ........... FUNCIÓN INSERT_DATA ............
# ............................................

#     .) Calculamos el TIEMPO de crear la tabla
#     .) Tiempo Incial - Tiempo Final
#     .) Creamos la TABLA con ST_WRITE

insert_data <- function(tabla){
  # start_time <- Sys.time()
  st_write(obj = tabla,
           dsn = con,
           Id(schema="public", table = "data_day_fire"),
           append=FALSE)
  # end_time <- Sys.time()
  # end_time - start_time
}


RPostgres::dbExecute(con, drop_table)
insert_data(data_day_fire)                        # Time processing data_day_fire (Casas 9.54 min / UAB = 2.49 min)  
RPostgres::dbExecute(con, vacuum_analyze)






# %%%%%%%%%%%%%%%%%%%%%%   CREACIÓN de INDEXS  %%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#     .) Uso el link = https://docs.microsoft.com/es-es/sql/relational-databases/sql-server-index-design-guide?view=sql-server-2017#nonclustered-index-architecture

#     .) Para probar los diferentes índices usaré el DATA.DAY_CREADO
#     .) Usaré la tabla TERBALL.NUCLIS_POBLACIO


drop_index_fire_plot_origin <- glue::glue_sql("
DROP INDEX IF EXISTS public.data_day_fire_plot_origin CASCADE;", .con = con)

drop_index_fire_geom <- glue::glue_sql("
DROP INDEX IF EXISTS public.data_day_fire_gist CASCADE;", .con = con)

drop_index_fire_date <- glue::glue_sql("
DROP INDEX IF EXISTS public.data_day_fire_date CASCADE;", .con = con)




create_index_fire_plot_origin <- glue::glue_sql("
CREATE INDEX data_day_fire_plot_origin 
ON public.data_day_fire (plot_origin);", .con = con)

create_index_fire_geom <- glue::glue_sql("
CREATE INDEX data_day_fire_gist on public.data_day_fire
using gist(geometry);", .con = con)

create_index_fire_date <- glue::glue_sql("
CREATE INDEX data_day_fire_date 
ON public.data_day_fire (date);", .con = con)


RPostgres::dbExecute(con, drop_index_fire_plot_origin) 
RPostgres::dbExecute(con, drop_index_fire_geom)
RPostgres::dbExecute(con, drop_index_fire_date)

RPostgres::dbExecute(con, create_index_fire_plot_origin)
RPostgres::dbExecute(con, create_index_fire_geom )
RPostgres::dbExecute(con, create_index_fire_date)


RPostgres::dbDisconnect(con)




