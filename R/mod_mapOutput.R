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
      leaflet::setView(1.7458675,41.6922353, zoom=8) %>%
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
    
    variable <- data_reactives$variable_reactive
    
    fecha <- data_reactives$fecha_reactive
    sf <- main_data_reactives$data_day
    
    data_day <- table_create(fecha,sf)
    
    
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
    #      .) Ya que USARE par AUTOMATIZAR los POPUPS y el resot  => data_filter[[2]]
    
    
    num_i <- as.numeric(match(variable,names(data_day)))
    selected_var <- as.symbol(names(data_day)[num_i])
    
    data_filter <- data_day %>%
      dplyr::filter(!is.na(data_day[[num_i]])) %>%
      dplyr::select(plot_id, selected_var, date, plot_origin, geom) %>%
      dplyr::mutate(lon = sf::st_coordinates(.data$geom)[,1],
                    lat = sf::st_coordinates(.data$geom)[,2])%>%
      dplyr::filter(!is.na(lon) | !is.na(lat))
    
    variable_valores <- round(data_filter[[2]], digits=2)
    
    # ...... PALETA DE COLORES CONTINUO ......
    # ........................................
    
    pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = data_filter[[2]])
    
    
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
    
    # ........ FUNCION SIZE RADIO ............
    # ........................................

    #     .) Función que determina el tamaño del Radio/Parcela
    #     .) En f(x) del que hayamos seleccionado en RADIO BUTTON SIZE
    #     .) Hará que el radio se:
    #            .) Estandard = SIMPRE VALOR 6
    #            .) Variable = en f(x) del rango de la Variable seleccionada


    
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
    
    
    # .......... MODIFICAR PLOTS ..........
    # .....................................
    
    #      .) Podemos variar los PLOTS en f(x) de:
    #               .) RADIUS
    #               .) CUANTILES
    
    #      .) RADIUS = Aplicamos f(x) SIZE_RADI
    #      .) CUANTILES = Aplicamos IF
    #               .) LEGEND = "Conti":
    
    #                    .) MARKER:  color = ~ PAL
    #                    .) LEGEND: pal = pal / values = data_filter[[2]] 
    
    #               .) LEGEND = "Quanti"
    #                    .) MARKER:  color = ~ QPAL
    #                    .) LEGEND: colors = qpal_colors / labels = qpal_labs
   
    
    
    leaflet::leafletProxy('map_daily') %>%
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
        radius = 6,
        color =  ~ pal(data_filter[[2]]),
        popup = popInfo) %>% 
      leaflet::clearControls() %>%
      leaflet::addLegend(
        position = "bottomright",
        title = paste(as.character(selected_var)),
        pal = pal,
        values = data_filter[[2]],
        opacity = 1)
    
    
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
