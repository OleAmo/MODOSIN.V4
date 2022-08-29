#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
#'
#'
mod_mapOutput_polygon <- function(id) {

  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("map_daily_polygon"), height = 600),
    shiny::uiOutput(ns('map_container_polygon'))
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

#' @export
#'
#'
mod_map_polygon <- function(
  input, output, session,
  data_reactives_polygon,modosin_data_polygon, 
  parent_session, lang
) {
  # library(sf)
  # source('data-raw/polygon_objects_creation.R')
  
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
  
  output$map_daily_polygon <- leaflet::renderLeaflet({
    
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
  # ---------------------------      OBSERVER     --------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #       .) Especificamos los EVENTOS
  #       .) 
  #       .) PRIMERO => VALIDAMOS el LAYERS SELECT
  #               .) Significa que SHINY antes de CONTINUAR espera a tener INFO de layers
  #               .) Osea que la variable sea (Pronvincia, Comarca o No Polígono)
  #               .) Sino validamos, SHYNI se bloquea ya que al inicio da NULL
  
  #       .) SEGUNDO =>
  #               .) Defeinimos f(x) PROCETAR GEOMETRIA
  #               .) Indicamos en que casos la proyectamos


  
  shiny::observe({
    
    shiny::validate(shiny::need(data_reactives_polygon$layers_select, 'no layers selected'))

    layers <-data_reactives_polygon$layers_select
    
 
    # --------------------------------------------------------------------------
    # ------------------------- GEOMETRÍA PROYECCIÓN  --------------------------
    # --------------------------------------------------------------------------

    #   .) Es el MAPA de LEAFLET que sale por defecto al principio
    #   .) Una vez el usuario APRETA BOTON PROYECTA MAPA
    #   .) Se visualiza los PLOTS
    
    
    geom_proyection  = function(poly,layers){
      
      #   .) LABEL:
      #   .) Definimos el HTML que mostrarà en pasar el mouse
      #   .) Usamos del ATRIBUTO nom y area_km2 de los POLÍGONSO
      
      # .......... DIFERENTES LABELS ...........
      # ........................................
      
      #          .) Creo 2 tipos de LABELS
      #          .) Ya que 2 SF usan AREA Km2
      #          .) Y 2 otro SF usan AREA Ha
      
      a <- sprintf( "<strong>%s</strong><br/>%g Àrea / km<sup>2</sup>",
                    poly$nom, poly$area_km2 ) %>% lapply(htmltools::HTML)
      
      b <- sprintf("<strong>%s</strong><br/>%g Àrea / Ha",
                     poly$nom , poly$Area_Ha ) %>% lapply(htmltools::HTML)
      
      
      if(layers == "provincia" | layers == "comarca" ) { labels <- a } 
      else if (layers == "nucleos" | layers =="embass") {  labels <- b }
      
      # ......... DIFERENTES ESTILOS ...........
      # ........................................
      
      #          .) Tengo 4 SF diferentes
      #          .) Creo ESTILOS diferentes
      
      fill_poly <- function (a){
        if (a == "provincia")      { return("red")
        } else if (a == "comarca") {  return("green")
        } else if (a == "nucleos") { return("black")
        } else if (a == "embass")  { return("#5cc8fa") }
      }
      
      color_poly <- function (a){
        if (a == "provincia")       {  return("#870522")
        } else if (a == "comarca")  { return("#03400d")
        } else if (a == "nucleos" ) {  return("black")
        } else if (a == "embass")   { return("blue")  }
      }
      
      weight_poly <- function (a){
        if (a == "nucleos") { return(1)
        } else              { return(0.5) }
      }
      
      
      # ..............................................
      # ............... Función LEAFLET  .............
      # ..............................................
      
      #     .) Usamos: LEAFLETPROXY
      #     .) https://rstudio.github.io/leaflet/shiny.html
      #     .) FUNCION:
      #              .) Usa el MAPA creado en  => output$map_daily_polygon 
      #              .) Y solo  PROYECTA ENCIMA lo que delclaramos (POLIGONOS)
      #              .) Así visualmente no desaparece cada vez el FONDO
      
      #     .) DATA (Poly)
      #     .) Son los POLIGONOS seleccionados COMBO BOX de LAYERS
      
      
      
      leaflet::leafletProxy('map_daily_polygon') %>%
        leaflet::clearGroup('polygon_layer') %>%
        leaflet::addPolygons(
           data = poly,
           group = 'polygon_layer',
           color = color_poly(layers),
           weight = weight_poly(layers),
           smoothFactor = 0.5,
           opacity = 1.0,
           fillOpacity = 0.4,
           fillColor =   fill_poly(layers),
           highlightOptions = highlightOptions(
             color = "white",
             weight = 2,
             fillOpacity = 0.4,
             fillColor =  ~ ifelse(layers == "provincia", "green", "red"),
             bringToFront = TRUE),
           label = labels,
           labelOptions = labelOptions(
             style = list("font-weight" = "normal", padding = "3px 8px"),
             textsize = "15px",
             direction = "auto")
          )
      
    }
    
    
    # --------------------------------------------------------------------------
    # ----------------------- ACTIVAR/DESACTIVAR CAPAS  ------------------------
    # --------------------------------------------------------------------------
    
    #      .) Aplicaremos la f(x) GEOM_PROYECTION
    #      .) Lo haré en función del valor de SELECCION del COMBO LAYERS
    #      .) En el caso de NUCLEOS:
    #              .) Aplico f(x) AREA_SELECTED
    #              .) Filtra los NUCLEOS en función Rango Area
    
    
    if (layers == "provincia"){ geom_proyection(provincias,layers) }
    else if (layers == "comarca") {  geom_proyection(comarcas,layers) }
    else if (layers == "nucleos") {
      
          area <- data_reactives_polygon$nucleos_area
          
          # .......... FUNCION AREA ..............
          # ......................................
          
          #      .) Función que calcula
          #      .) MAX / MIN:
          #              .) A partir del valor de comobo de NUCLE_ARE
          #              .) Hace un SPLIT y obtiene los valores
          #      .) NUECLEOS
          #              .) Devuelve NUCLEOS de POBLACION
          #              .) FILTER = que esten entre AREA Max y AREA Min
          
          
          area_selected <- function(nucleos,area) {
            a_min <- area %>%
              strsplit(split = "-") %>% .[[1]] %>% .[1] %>%
              as.numeric()
            a_max <- area %>%
              strsplit(split = "-") %>% .[[1]] %>% .[2] %>%
              as.numeric()
            
            res <- nucleos %>% dplyr::filter(., Area_Ha >= a_min & Area_Ha < a_max)
            return(res)
          }
          
          if (area  == "0-10") { geom_proyection( area_selected(nucleos,area),layers) }
          else if (area  == "10-25") { geom_proyection( area_selected(nucleos,area),layers) }
          else if (area  == "25-50") { geom_proyection( area_selected(nucleos,area),layers) }
          else if (area  == "50-100") { geom_proyection( area_selected(nucleos,area),layers) }
          else if (area  == "100-250") {  geom_proyection( area_selected(nucleos,area),layers) }
          else if (area  == "1000-99999") { geom_proyection( area_selected(nucleos,area),layers) }
      
      
    } else if (layers == "embass") { geom_proyection(embass,layers)
    } else if (layers == "no_polygon") {
      
      leaflet::leafletProxy('map_daily_polygon') %>%
        leaflet::clearGroup('polygon_layer')

    }
    
    
    
  })
  
  

  
  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    # map_reactives$map_daily_polygon_shape_click <- input$map_daily_polygon_shape_click
    # map_reactives$map_daily_polygon_marker_click <- input$map_daily_polygon_marker_click

  })
  return(map_reactives)
  
  
  
  
  
  

}
