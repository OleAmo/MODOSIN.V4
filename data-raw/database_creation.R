
# ///////////////////////////////  MODOSIN ////////////////////////////
# ///////////////////////////////////////////////////////////////////////


# ....... CREAR BBDD PRINCIPAL POSTGRESQL ......
# ..............................................

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


# ......... CREAR CONEXION BBDD CREAF_V4 ........
# ...............................................

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'creaf_v4',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = '12345database')


# ........... CORREGIR PLOTIDS ..........
# .......................................

#     .) Creo una función que Corriga los PLots_Id
#     .) Hay plots que empieza con ["8..."]
#     .) Tendría que empezar con ["08..."]
#         1.) Detectamos los errores => STR_DETECTED
#         1.) Añadimos un "0" con    => PASTE0


plotid_corrected <- function(data){
  plot_id <- data
  plot_id[stringr::str_detect(plot_id, "^8")] <-
    base::paste0("0", plot_id[stringr::str_detect(plot_id, "^8")])
  return(plot_id)
}


# .......... GEOMETRÍA PLOTS_ID .........
# .......................................

#     .) La geometría NO la da el MEDFATE
#     .) La geometría está en un GEOPACK (plot_id => geom)
#     .) El GEOPACK lo sacamos de la web LFC (NIF_APP)

#     .) En este caso La tengo guardada en mi POSTGRESQL
#             .) BBDD = CATALUNYA
#             .) SCHEMA/TABLE = CAREAF.NFI_APP


con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'catalunya',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = '12345database')

#     .) CREAR el SF de la tabla NIF_APP
#     .) DESCONECTAR de la BBDD dele NIF
#     .) CONECTAR a la BBDD donde estamos trabajando


parcelas_nfi <- st_read(dsn = con, Id(schema="creaf", table = "nif_app"))

RPostgres::dbDisconnect(con)

# ........ OBTENER PLOT_ID Corregidos.........
# ............................................

#     .) Obtengo los nombres de los archivos
#     .) les quito el .RDS
#     .) Hago SPLIT con "_" y me quedo lo 2do (el plot_id)
#     .) Corrijo el código

plots_id <- list.files("RDS_f", pattern="rds$") %>%
  gsub(".rds", "", ., fixed = TRUE) %>%
  str_split_fixed(.,"_", 2) %>%
  .[,2] %>%
  plotid_corrected()


# ........ FUNCION PLOT_ORIGIN ...............
# ............................................
#     .) Obtengo los nombres de los archivos => "y_xxxxx.RDS"
#     .) Separo con SPLIT "_" y me quedo la primera parte
#     .) Después en función de si és P o A retorno IFN o AIGUESTORTES

plot_origin <- function(data){
  res <- data %>%
    str_split_fixed(.,"_", 2) %>%
    .[,1]

  if(res == "a"){
    return("aiguestortes")
  } else if (res == "p") {
    return("ifn")
  } else {
    return("NA")
  }

}

# ........... FUNCIÓN INSERT_DATA ............
# ............................................

#     .) Calculamos el TIEMPO de crear la tabla
#     .) Tiempo Incial - Tiempo Final
#     .) Creamos la TABLA con ST_WRITE



insert_data <- function(tabla){

  st_write(obj = tabla,
           dsn = con,
           Id(schema="public", table = "data_day"),
           append=FALSE)
}

# ........ CREAR DATA DAY ....................
# ............................................


data_day <- list.files("RDS_f", pattern="rds$") %>%

  set_names(plots_id) %>%

  purrr::imap_dfr(.f = function(archivo,id){
    readRDS(paste0("RDS_f/",archivo)) %>%
      dplyr::mutate(plot_id = id , date = as.Date(row.names(.)), plot_origin = plot_origin(archivo))
  }) %>%

  dplyr::left_join(
    select(parcelas_nfi,old_idparcela),
    by = c("plot_id" ="old_idparcela"))



# .......... INSERTAR DATADAY a BBDD ...........
# ..............................................


#     .) Creo EXTENCION POSTGIS
#     .) DROP TABLE DataDay
#     .) INSERT DataDay
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


drop_table <- glue::glue_sql("
DROP TABLE IF EXISTS public.data_day CASCADE;", .con = con)

vacuum_analyze <- glue::glue_sql("
VACUUM ANALYZE public.data_day;", .con = con)


rpostgis::pgPostGIS(con, topology = TRUE, sfcgal = TRUE)
RPostgres::dbExecute(con, drop_table)
insert_data(data_day)
RPostgres::dbExecute(con, vacuum_analyze)


# .......... CREACIÓN de INDEX ...............
# ............................................

#     .) Uso el link = https://docs.microsoft.com/es-es/sql/relational-databases/sql-server-index-design-guide?view=sql-server-2017#nonclustered-index-architecture

#     .) Para probar los diferentes índices usaré el DATA.DAY_CREADO
#     .) Usaré la tabla TERBALL.NUCLIS_POBLACIO


drop_index_plot_origin <- glue::glue_sql("
DROP INDEX IF EXISTS public.data_day_plot_origin CASCADE;", .con = con)

drop_index_geom <- glue::glue_sql("
DROP INDEX IF EXISTS public.data_day_gist CASCADE;", .con = con)

drop_index_date <- glue::glue_sql("
DROP INDEX IF EXISTS public.data_day_date CASCADE;", .con = con)




create_index_plot_origin <- glue::glue_sql("
CREATE INDEX data_day_plot_origin 
ON public.data_day (plot_origin);", .con = con)

create_index_geom <- glue::glue_sql("
CREATE INDEX data_day_gist on public.data_day
using gist(geom);", .con = con)

create_index_date <- glue::glue_sql("
CREATE INDEX data_day_date 
ON public.data_day (date);", .con = con)


RPostgres::dbExecute(con, drop_index_plot_origin) 
RPostgres::dbExecute(con, drop_index_geom)
RPostgres::dbExecute(con, drop_index_date)

RPostgres::dbExecute(con, create_index_plot_origin)
RPostgres::dbExecute(con, create_index_date)
RPostgres::dbExecute(con, create_index_geom)


RPostgres::dbDisconnect(con)




