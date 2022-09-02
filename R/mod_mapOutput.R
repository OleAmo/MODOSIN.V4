#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
#'
#'
mod_mapOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("map_daily"), height = 600),
    shiny::uiOutput(ns('map_container'))
  )
}

#' mod_map server function
#'
#' @details mod_map is in charge of setting the points/polygons (sf) and rasters
#'   in the leaflet projection.
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives reactives
#' @param parent_session session object to change active tab
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
#'
mod_map <- function(
  input, output, session,
  data_reactives, main_data_reactives, 
  parent_session, lang
) {
  
  library(sf)
  source('data-raw/polygon_objects_creation.R')

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ---------------------------      OUTPUT MAP     ------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #       .) Indicamos DONDE se harà el OUTPUT
  #       .) Lo indicamos con      => output$map_daily_polygon
  #       .) Usamos la función     => renderLeaflet
  
  #       .) LEAFLET:
  #              .) Definimos un MAPA BASE ESTANDAR
  #              .) Con tipos de fondo (OSM, RELIEF,...)
  #              .) Sin ningun tipus de GEOMETRIA
  
  output$map_daily <- leaflet::renderLeaflet({
      

    leaflet::leaflet() %>%
      leaflet::setView(2.2018256,41.089058, zoom=7) %>%
      leaflet::addTiles(group = "OSM") %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief,
        group = translate_app('Relief', lang())
      ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = translate_app('Imagery', lang())
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c(translate_app('Relief', lang()), translate_app('Imagery', lang()),"OSM"),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      ) 
      
    
  })  
  
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ---------------------------      REACTIVE    --------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #       .) Especificamos los REACTIVOS
  #      
  #       .) Creación de MAPA en f(x) CLICK Proyectar  
  #               .) Significa que SHINY antes de CONTINUAR espera a tener INFO de layers
  #               .) Osea que la variable sea (Pronvincia, Comarca o No Polígono)
  #               .) Sino validamos, SHYNI se bloquea ya que al inicio da NULL
  
  #       .) SEGUNDO =>
  #               .) Defeinimos f(x) PROCETAR GEOMETRIA
  #               .) Indicamos en que casos la proyectamos
  
  
  
  # ............. CREATE DATA_DAY .................
  # ...............................................
  
  #   .) DATA DAY son los PLOTS de SOLO UNA FECHA
  #   .) ARGUMENTOS:
  #             .) FECHA
  #             .) SF = Creado por MODOSINDB
  
  
  table_create = function(fecha, sf){
    fecha_format <- as.Date(fecha)
    data_day <- sf %>%
      data.frame() %>%
      dplyr::filter(date == fecha_format)
    return(data_day)
  }
  

  
  # ............ MAPA PLOTS DATADAY ...............
  # ...............................................
  
  
  #   .) Una vez el usuario APRETE BOTON PROYECTAR
  #   .) Se proyectaran los PLOTS en f(X) de:
  #   .) VARIABLE / SIZE / QUANTILES
  
  
  leaflet_map_create <- eventReactive(data_reactives$boto_reactive, {
    
    # ......... INICIALIZAR DATOS ............
    # ........................................
    
    #      .) VARIABLE:
    #            .) Es la variable a PROYECTAR
    #            .) La obtenemos del REACTIVE constante => MOD_DATAINPUT
    #      .) DATA DAY:
    #            .) Uso la funcion TABLE_CREATE
    #            .) Necesito la FECHA = data_reactives$fecha_reactive
    #            .) Necesito el SF    = main_data_reactives$data_day
    
    
    origen <- data_reactives$origen_reactive 
    variable <- data_reactives$variable_reactive
    division <- data_reactives$division_reactive
    fecha <- data_reactives$fecha_reactive
    sf <- main_data_reactives$data_day
  
 
    data_day <- table_create(fecha,sf)
    
    #      .) PLOT ORIGEN
    #      .) Para definir el Plot_origen selccionado
    #      .) Usamos el ORIGEN del COMBO
    #      .) Y lo "traducimos" al string que usa el data_day_fire
    
    
    origenSelected <- function(a) {
      if (a == "P") {
        return("ifn")
      } else if (a == "A") {
        return("aiguestortes")
      } else if( a == "S"){
        return("matosec")
      } else {
        return("tots")
      }
    }
    
    origen_selected <- origenSelected(origen)
    
   
    # ......... PROYECTAR TABLA ..............
    # ........................................
    
    #      .) Creo un DF Filtrado
    #      .) Para usar el LEAFLET necesitamos:
    #            .) Longitud
    #            .) Latitud
    
    #     .) Para tener Long/Lat necesitamos:
    #            .) geometría en formato WKB
    
    #      .) GEOM to WKB  =>  sf:::st_as_sfc.WKB
    #      .) WKB to LON   =>  sf::st_coordinates(geom_WKB)[,1]
    #      .) WKB to LAT   =>  sf::st_coordinates(geom_WKB)[,2]
    
    
    # ....... INDICE de la VARIABLE  .........
    # ........................................
    
    #      .) Quiero SABER el ÍNDICE de la VARIABLE
    #           .) Por ejemplo si quiero PRECIPITATION
    #           .) Tengo que saber que es el 2
    #      .) Ya que USARE par AUTOMATIZAR los POPUPS y el res  => data_filter[[2]]
    
    
    num_i <- as.numeric(match(variable,names(data_day)))
    selected_var <- as.symbol(names(data_day)[num_i])
    
    
    # ....... FILTRADO PLOT ORIGIN ...........
    # ........................................
    
    #      .) la función ORGENG_SELECTED nos da 3 origenes (Aiguestortes, Nfi y Matosec)
    #      .) Si queremos que nos lo proyecte TODO
    #      .) Tenemo de decirle que NO FILTRE
    #      .) Por eso hago un PIPE con IF
    #           .) Y si ORIGEN NO ES T (T = es todos) me filtra por ORIGEN_SELECTED
    #           .) Si ES T, no hace filtro y lo PROYECTA TODO
    
    data_filter <- data_day %>%
      {if(origen_selected != "tots") dplyr::filter(., plot_origin == origen_selected ) else . } %>%
      dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
      dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
                    lat = sf::st_coordinates(.data$geometry)[,2])%>%
      dplyr::filter(!is.na(lon) | !is.na(lat))
    

    variable_valores <- round(data_filter[[2]], digits=2)
    
    # ...... PALETA DE COLORES CONTINUO ......
    # ........................................
    
    pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = data_filter[[2]])
    
    
    # .... PROBLEM  ...........
    # ........................
    
    #      .) with NA Values
    #      .) Shiny se BLOQUEA (peta)
    #      .) https://github.com/rstudio/leaflet/issues/615
    
    
    # ............... POP UP  ................
    # ........................................
    
    popInfo<-paste(
      "<h4 style= 'border-bottom: thin dotted #43464C; padding-bottom:4px; margin-bottom:4px;
         font-family: Tahoma, Geneva, sans-serif; color:#43464C;'> Plot_id = ",data_filter$plot_id,"</h4>

     <span style='color:#9197A6;'>
         ",variable," : ", variable_valores,"<br>",
     paste("Ubicacion: ",data_filter$plot_origin, sep=""),"<br>",
     paste("Fecha: ",data_filter$date, sep=""),"</span>"
    )
    
    # ..............................................
    # ............... Función LEAFLET  .............
    # ..............................................
    
    #     .) Usamos: LEAFLETPROXY
    #     .) https://rstudio.github.io/leaflet/shiny.html
    #     .) FUNCION:
    #              .) Usa el MAPA creado en  => output$map_daily 
    #              .) Y solo  PROYECTA ENCIMA lo que declaramos (DATA_DAY)
    #              .) Así visualmente cada vez que proyectamos no desaparece el FONDO
    
    #     .) DATA = DATA_FILTER 
    #     .) Son los POLTS FILTRADOS:
    #              .) UNA Fecha / UNA Variable / Lat + Long
    
    
    # ..... LEYENDA / PLOTS con PROXY .....
    # .....................................
    
    #     .) Si usamos PROXY tenemos que:
    #     .) BORRAR cada vez que proyectamos
    #              .) Plots   => usamos leaflet::clearGroup + Group
    #              .) Legend  => leaflet::clearControls() 
    
    
    
    # .............. PROYECCIÓN PLOTS  .............
    # ..............................................
    
    #     .) Empieza con un CONDICIONAL
    #     .) Si ORIGEN (Comobo Origen = No Plot) es DIFERENTE a NO PLOT
    #     .) Proyecte en función del tipo de Plot
    #     .) Si es NO PLOT
    #     .) Borra todos los Plots
    
    
    if(origen != "no"){
      
      
      set_view_all <- leaflet::leafletProxy('map_daily') %>% leaflet::setView(2.2018256,41.089058, zoom=7)
      set_view_AT <- leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.9400943,42.5614381, zoom=11)
      # set_view_OR  # Ordesa
      # set_view_MS  # Matosec
      # set_view_NFI # NFI
      
        
      # leaflet::leafletProxy('map_daily') %>%
      # leaflet::setView(2.3018256,41.089058, zoom=7) %>%
        
        
      set_view_AT %>% 
      leaflet::clearGroup('plots_layer') %>%
      leaflet::addCircleMarkers(
        data = data_filter,
        group = 'plots_layer',
        layerId = ~ plot_id,
        lat = ~ lat,
        lng = ~ lon,
        weight= 1,
        opacity= 0.8,
        fillOpacity= 0.6,
        radius= 6,
        color =  ~ pal(data_filter[[2]]),
        popup = popInfo) %>%
      leaflet::clearControls() %>%
      leaflet::addLegend(
        position = "bottomright",
        title = paste(as.character(selected_var),' (Contínua) '),
        pal = pal,
        values = data_filter[[2]],
        opacity = 1)
      
    } else {
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('plots_layer')
      
    }
    
    # ............ PROYECCIÓN DIVISIONES  ..........
    # ..............................................
    
    #     .) Empieza con un CONDICIONAL
    #     .) Si DIVISION (Comobo DIVISION = No Division) es DIFERENTE a NO DIVIDION
    #     .) Proyecte en función del tipo de Division
    #     .) Si es NO DIVISION
    #     .) Borra todos las DIVISIONES
    
  
    #     .) FUNCIÓN DIVSION SELECT
    #              .) En función del COMBO DIVISION 
    #              .) Proyectara uno o otro SF (Shapes)
    
    division_selected <- function(division) {
      if(division == "prov") {
        return(provincias_simplfy)

      } else if (division == "at") {
        return(aiguestortes_simplfy)
      } else if (division == "or") {
        return(ordesa_simplfy)
      }

    }


    if(division != "no"){
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('divisiones') %>%
        leaflet::addPolygons(
          data = division_selected(division),
          weight = 2,
          fillOpacity = 0,
          color = "#bf021b",
          group = "divisiones")
      
    } else {
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('divisiones')  
    }
    


    
})
  
 
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ---------------------------      OBSERVER     --------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #       .) Especificamos los EVENTOS
  #      
  #       .) PRIMERO => VALIDAMOS la FECHA
  #               .) Significa que SHINY antes de CONTINUAR espera a tener valor de FECHA
  #               .) Osea que la variable sea ('2021-5-11')
  #               .) Sino validamos, SHYNI se puede bloquear si al inicio el valor es NULL
  
  
  # .............. EVENTO 1r ...............
  # ........................................
  
  #     .) Ejecutamos FUNCIÓN 
  #     .) Indicamos en que casos la proyectamos
  
  
  shiny::observe({
    
    shiny::validate(shiny::need(data_reactives$fecha_reactive, 'fecha no activated') )
    
    leaflet_map_create()
 
 })
  
  
  # .............. EVENTO 2ndo .............
  # ........................................
  
  #     .) CLIC Información
  #     .) Cada vez que hacemos CLICK aL:Ç
  #              .) 'MAIN_PANEL_TABSET' = MAPA de LEFLET (Declarado en APP.R)
  #              .) Obtenemos INFO = ID, LAT, LONG
   
  
  shiny::observeEvent(
    eventExpr = input$map_daily_marker_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )

  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................
  
  #      .) Creamos MAP REACTIVES

  # ...... MAP REACTIVES .........
  # ..............................
  
  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $
  #      .) Devolvemos INFO DEL CLICK
  #              .) Lo usaremos a MOD_MAIN_DATAOUTPUT
  #              .) Así obtendremo ID para el TIME SERIE GRAFIC
  
  map_reactives <- shiny::reactiveValues()
  shiny::observe({

    map_reactives$map_daily_marker_click <- input$map_daily_marker_click

  })
  return(map_reactives)


  

}
