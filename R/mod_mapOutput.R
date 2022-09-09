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
        baseGroups = c(translate_app('Relief', lang()),translate_app('Imagery', lang()), "OSM"),
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
  
  
  # ............. REACTIVE ZOMM ...................
  # ...............................................
  
  #   .) Calcula el ZOOM en todos los momentos
  #   .) Lo usaremos mas adelante
  
  base_size <- shiny::reactive({
    
    current_zoom <- input$map_daily_zoom
    
    if (current_zoom <= 7) { size_transformed <- 3
      
    } else if (current_zoom == 8) { size_transformed <- 8
      
    } else if (current_zoom >= 9) { size_transformed <- 10
    
    }
    
    return(size_transformed)
  })
  
  
  radi_builder <- shiny::reactive({
    size_radi <- base_size()
    
    
    return(list( size_radi = size_radi))
  })
  
  
  

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ---------------------------      OBSERVE    --------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  # ............ PROYECCIÓN LEFLET ................
  # ...............................................
  
  
  #   .) AUTOMÁTICAMENTE 
  #   .) Se proyectan PLOTS y POLIGON en el mapa
  #   .) Antes de proyectar
  #            .) Se obtienen VARIABLES (fecha, variable, polígono,...)
  #            .) Con estas variables de filtra la tabla principal
  
  
  shiny::observe({
    
  
    
    # ......... INICIALIZAR DATOS ............
    # ........................................
    
    #      .) VARIABLE:
    #            .) Es la variable a PROYECTAR
    #            .) La obtenemos del REACTIVE constante => MOD_DATAINPUT
    #      .) DATA DAY:
    #            .) Uso la funcion TABLE_CREATE
    #            .) Necesito la FECHA = data_reactives$fecha_reactive
    #            .) Necesito el SF    = main_data_reactives$data_day
    #            .) Necesito el ORIGEN, VARIABLE, DIVISION
    
    #       .) VALIDAMOS la FECHA
    #               .) Significa que SHINY antes de CONTINUAR espera a tener valor de FECHA
    #               .) Osea que la variable sea ('2021-5-11')
    #               .) Sino validamos, SHYNI se puede bloquear si al inicio el valor es NULL
    
    shiny::validate(
      shiny::need(data_reactives$variable_reactive, 'no variable selected') 
    )
    
    fecha <- data_reactives$fecha_reactive
    origen <- data_reactives$origen_reactive 
    variable <- data_reactives$variable_reactive
    sf <- main_data_reactives$data_day
    
    
    # radi_builder <- radi_builder()
 
    data_day <- table_create(fecha,sf)
    
    #      .) PLOT ORIGEN
    #      .) Para definir el Plot_origen selccionado
    #      .) Usamos el ORIGEN del COMBO
    #      .) Y lo "traducimos" al string que usa el data_day_fire
    
    
    origenSelected <- function(a) {
      if (a =="T") {
        return(c("ifn","aiguestortes","matollar","ordesa") )
      }  else if (a == "PN") {
        return(c("aiguestortes","ordesa"))
      } else if (a == "P") {
        return("ifn")
      } else if (a == "A") {
        return("aiguestortes")
      } else if( a == "S"){
        return("matollar")
      } else if( a == "O"){
        return("ordesa")
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
    
    #      .) la función ORGENG_SELECTED nos da 4 origenes (Aiguestortes, Ordesa, Nfi y Matollar)
    #      .) Si queremos que nos lo proyecte TODO
    #      .) Tenemo de decirle que NO FILTRE
    #      .) Por eso hago un PIPE con IF
    #           .) Y si ORIGEN NO ES T (T = es todos) me filtra por ORIGEN_SELECTED
    #           .) Si ES T, no hace filtro y lo PROYECTA TODO
    
    data_filter <- data_day %>%
     
      dplyr::filter(.,plot_origin == origen_selected, !is.na(selected_var)) %>%
      dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
      dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
                    lat = sf::st_coordinates(.data$geometry)[,2])%>%
      dplyr::filter(!is.na(lon) | !is.na(lat))
    

    variable_valores <- round(data_filter[[2]], digits=2)
    
    
    
    
    # ....... ELIMINAR NA del DATA FILTER ...........
    # ...............................................
    
    #      .) No me sale
    
    
    # mod <- lfcdata::modosin()
    # modosin <- mod$get_data('data_day_fire_petita_2')
    # 
    # data_filter <- modosin %>%
    # 
    #   dplyr::filter(.,plot_origin == "ifn", date == as.Date('2022-01-15'), !is.na(DDS_q)  ) %>%
    #   dplyr::select(plot_id, DDS_q, date, plot_origin, geometry) %>%
    #   dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
    #                 lat = sf::st_coordinates(.data$geometry)[,2])%>%
    #   dplyr::filter(!is.na(lon) | !is.na(lat))
    # 
    # variable_valores <- round(data_filter[[2]], digits=2)
    # 
    # length(variable_valores)
    # 
    # data_filter %>% dplyr::filter(.,is.na(DDS_q)  )
    # 
    # x <- rep(10:55)
    # 
    # # Using rep() method
    # gfg <- append(x, c(0,100), 1)
    
    
    
    
    
    
    # ...... PALETA DE COLORES CONTINUO ......
    # ........................................
    
    #      .) Hay dos paletas = PLOT y LEGEND
    #      .) usamos reverse = FALSE/TRUE para alterar el orden de colores (plot y legend)
    
    #      .) La paleta de CUANTILES tienen que ser DIVERGENTE
    #      .) Significa que:
    #                  .) valor 100 = son valors SUPERIORES a los útlitmos 40 años
    #                  .) valor  50 = son valors IGUALES a los útlitmos 40 años
    #                  .) valor   0 = son valors INFERIORES a los útlitmos 40 años
    
    # ..... FUNCION QUANTIL ......
    # .............................
    
    #      .) Esta funcion detecta si 
    #      .) Una variable es QUANTIL o NO
    #      .) Devuelver TRUE o FALSE
    
    
    
    is_quantil <- function(variable) {
      grepl('_', variable, fixed = TRUE)
    }
    
    # c("green","white","red")
    # c("#48cf4f","white","#d14949")
    # "RdYlBu"
    
    
    if (is_quantil(variable)) {
      pal_plot <- leaflet::colorNumeric(palette = "RdYlBu", domain = data_filter[[2]] , reverse = FALSE)
      pal_legend <- leaflet::colorNumeric(palette = "RdYlBu", domain = data_filter[[2]] , reverse = TRUE)
    } else {
      pal_plot <- leaflet::colorNumeric(palette = "plasma", domain = data_filter[[2]] , reverse = TRUE)
      pal_legend <- leaflet::colorNumeric(palette = "plasma", domain = data_filter[[2]] , reverse = FALSE)
    }
  
    # ......... FUNCION SIZE RADI ............
    # ........................................
    
    #     .) La usamos para que los plots se vean correctamentes
    #     .) En IFN y TODOS del radio tiene que ser menor
    
    size_radi = function(a){

        if (a == "T" | a == "P") {
          return(4)
        } else {
          return(6)
          
        } 
        
    }
      
 
    
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
    
    # ........ LENGUA SELECTED  .........
    # ...................................
    
    #     .) Lengua Seleccionada
    
    lang_declared <- lang()

    # .............. PROYECCIÓN PLOTS  .............
    # ..............................................
    
    #     .) Empieza con un CONDICIONAL
    #     .) Si ORIGEN (Comobo Origen = No Plot) es DIFERENTE a NO PLOT
    #     .) Proyecte en función del tipo de Plot
    #     .) Si es NO PLOT
    #     .) Borra todos los Plots
    
  
      
      # ........... ZOOM PLOTS  ...........
      # ...................................
      
      #     .) Creamos f(x) SET_VIEW
      #     .) Dentro en funcion de ORIGEN
      #     .) Setearemos uno o otro zoom
      
      
      set_view <- function(a) {

        if (a == "T") {
          leaflet::leafletProxy('map_daily') %>% leaflet::setView(2.2018256,41.089058, zoom=7)
        } else if ( a == "P" ) {
          leaflet::leafletProxy('map_daily') %>% leaflet::setView(1.7458675,41.6922353, zoom=8)   
        } else if ( a == "PN" ) {
          leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.488007,42.6306324, zoom=9.5)
        } else if ( a == "A") {
          leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.9313699999999825,42.57097690195607, zoom=12)
        } else if ( a == "S") {
          leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.5213654,41.3684307, zoom=8)
        }  else if ( a == "O") {
          leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.0782512,42.6215114, zoom=11)
        }                                                     
        
      }
      
      # ............ PROYECCIÓN POLIGONOS  ..........
      # ..............................................
      
      
      #     .) Proyecta en función del tipo de ORIGEN
      #     .) FUNCIÓN DIVSION SELECT
      #              .) En función del COMBO ORIGEN 
      #              .) Proyectara uno o otro SF (Shapes)
      
      polygon_selected <- function(origen) {
        
        # switch(
        #   origen,
        #   "T" = all_polygons,
        #   "A" = aiguestortes,
        # )
        # 
        
        if(origen == "T") {  return(all_polygons)
        } else if (origen == "A") { return(aiguestortes)
        } else if (origen == "PN") { return(parques)
        } else if (origen == "O") { return(ordesa)
        } else if (origen == "P") { return(catalunya)  
        } else if (origen == "S") { return(provincias)
        }
        
      }
      
      
       
      # ............ CREACIÓN LABELS .................
      # ..............................................
      
      #     .) Creo los LABELS información que aparece 
      #     .) Esta info aparece en pasar el Mouse por encima
      #     .) https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf
      #     .) SPRINTF => pasa info a STRING
      #              .) CADA % indica un value ( s = string, g = double precision,...)
      #              .) El ORDEN de declarar values es el ORDEN de APARICION
      
      poly <- polygon_selected(origen)
      
      labels_poly <- sprintf( "<strong>%s</strong><br/>%s ",
                         translate_app( poly$Descrip,lang_declared), poly$name ) %>% lapply(htmltools::HTML)
      
                         
      labels_plot <- sprintf( "<strong>%s</strong><br/> %s <br/> %s = %g ",
                          translate_app("PLOT",lang_declared),
                          data_filter$plot_id, variable, variable_valores) %>% lapply(htmltools::HTML)
      
     
      # ........... LEYENDA NA PROBLEMA  .............
      # ..............................................
      
      #     .) El valor NA en la LEYENDA da  problemas
      #     .) Queda mal colocado
      #     .) SOLUCION
      #     .) https://github.com/rstudio/leaflet/issues/615
      
      # css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
      # html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
      # m %<>% htmlwidgets::prependContent(html_fix)                   # Insert into leaflet HTML code
      
      
      # ........ PROYECCIÓN PLOTS  ........
      # ...................................
      
      

      set_view(origen) %>%
        leaflet::clearGroup('polygons') %>%
        leaflet::clearGroup('plots_layer') %>%
        
        leaflet::addPolygons(
          data = polygon_selected(origen),
          weight = 2,
          opacity = 0.7,
          fillOpacity = 0,
          color = "#bf021b", 
          group = "polygons",
          label = labels_poly,
          highlightOptions = leaflet::highlightOptions(bringToFront = FALSE) ) %>%
        
        leaflet::addCircleMarkers(
          data = data_filter,
          group = 'plots_layer',
          layerId =  ~ plot_id,
          lat = ~ lat,
          lng = ~ lon,
          weight= 1,
          opacity= 0.8,
          fillOpacity= 0.6,
          radius = size_radi(origen),
          # radius = radi_builder$size_radi,
          # radius = base_size(),
          color = ~ pal_plot(data_filter[[2]]),
          # popup = popInfo,
          label = labels_plot,
          labelOptions =   labelOptions(interactive = TRUE)) %>%
        
        leaflet::clearControls() %>%
        leaflet::addLegend(
          position = "bottomright",
          title = translate_app(variable, lang_declared),
          pal = pal_legend,
          values = data_filter[[2]],
          labFormat = labelFormat(transform = function(x) rev(x)),
          opacity = 1)  
      
      
      
    
})
  
 
 

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # -------------------------    OBVSERVE EVENT     ------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  # ............... CLIK MAPA ..............
  # ........................................
  
  #     .) CLICK MAPA
  #     .) Cada vez que hacemos CLICK obtenemos:
  #              .) INFO del PLOT (ID, LAT, LON,...lo que indiquemos en ADD_CIRCLE_MARKERS)
  #              .) ACTIVAR PESTAÑA TIME_SERIE
  #     .) PROCESO:
  #              .) Creamos un INPUT$value: 
  #                      .) Lo enviamos fuera del archivo al final de ESTE SCRIPT
  #                      .) Lo usaremos en MOD_MAIN_DATA_OUTPUT para saber el ID del PLOT
  #              .) ACTIVAMOS PESTÑA
  #                      .) Le indicamos el ID de CONTENDEOR DE TABS:s
  #                              .) 'MAIN_PANEL_TABSET_PLOTS' = MAPA de LEFLET (Declarado en APP.R)
  #                      .) Le indicamos la PESTAÑA A ACTIVAR:
  #                              .) SELECTED

   
  shiny::observeEvent(
    eventExpr = input$map_daily_marker_click,
    handlerExpr = {
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset_plots',
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
