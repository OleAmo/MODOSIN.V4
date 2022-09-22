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
  
  
  # ............. REACTIVE ZOMM ...................
  # ...............................................
  
  #   .) Calcula el ZOOM en todos los momentos
  #   .) Lo usaremos mas adelante
  
  
  radi_size <- shiny::reactive({
    
    current_zoom <- input$map_daily_zoom
    
    #   .) MATOLLAR
    #            .) ORIGEN = "S"
    #            .) Hay pocas parcelas en un gran extension
    #            .) Por eso el TAMAÑO para cada zoom serà el doble que el estandard
    
    shiny::validate( 
      shiny::need(data_reactives$origen_reactive, 'no origen selected') 
    )
    
    origen <- data_reactives$origen_reactive 
    
    ifelse(origen == "S", n <- 1.7,n <- 1)
    
    #   .) ZOOM ESTANDARD
    #            .) En f(x) del zoom
    #            .) Aplicamos un tamaño
    
    size_base <- 650
    
    if (current_zoom <= 5) { size_transformed <- size_base * 0.25 * n
      
    } else if (current_zoom == 6) { size_transformed <- size_base * 4 * n 
    
    } else if (current_zoom == 7) { size_transformed <- size_base * 4 * n
    
    } else if (current_zoom == 8) { size_transformed <- size_base * 2 * n
    
    } else if (current_zoom == 9) { size_transformed <- size_base * n  
    
    } else if (current_zoom == 10) { size_transformed <- size_base * n 
    
    } else if (current_zoom == 11) { size_transformed <- size_base * 0.6 * n 
    
    } else if (current_zoom == 12) { size_transformed <- size_base * 0.3 * n
    
    } else if (current_zoom >= 13) { size_transformed <- size_base * 0.3 * n
    
    } 
  
    
    return(size_transformed)
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
    
    #      .) VARIABLES REACTIVE:
    #            .) Son la que usaremos para PROYECTAR PLOTS
    #            .) Vienen de DATA_REACTIVE (fecha, origen, variable)
    #            .) Vienen de MAIN_DATA_REACTIVE (sf)
  
    #      .) VARIABLES FUNCION:
    #            .) Lengua y Radi
    #            .) Vienen de funciones tb REACTIVAS
    
    
    shiny::validate(
      shiny::need(data_reactives$variable_reactive, 'no variable selected') 
    )
    
    fecha <- data_reactives$fecha_reactive
    origen <- data_reactives$origen_reactive 
    variable <- data_reactives$variable_reactive
    leyenda_modif <- data_reactives$legend_modify_reactive  
    sf <- main_data_reactives$data_day
    
    # ........ LENGUA SELECTED  .........
    # ...................................
    
    #     .) Lengua Seleccionada
    
    lang_declared <- lang()
    
    # ....... SIZE en f(x) ZOOM  ........
    # ...................................
    
    #     .) El radio de los PLOTS
    #     .) Varia en f(x) del zoom usado
    #     .) es un REACTIVE definido anteriormente
    
    radi <- radi_size()
    
    # ............. CREATE DATA_DAY .................
    # ...............................................
    
    #   .) DATA DAY son los PLOTS de SOLO UNA FECHA
    #   .) Creamos una función para crearla
    #   .) ARGUMENTOS:
    #             .) FECHA = data_reactives$fecha_reactive
    #             .) SF    = main_data_reactives$data_day
    
    
    table_create = function(fecha, sf){
      fecha_format <- as.Date(fecha)
      data_day <- sf %>%
        data.frame() %>%
        dplyr::filter(date == fecha_format)
      return(data_day)
    }
    
    data_day <- table_create(fecha,sf)
    
    
    # ............ ALL TYPES ORIGIN .................
    # ...............................................
    
    #   .) Automatizo el calculo de saber TODOS los Plots ORIGIN
    #   .) Así la opción TODOS PLOTS del COMBO Tipos de Origen Plot
    #   .) Siempre devolverá TOTDOS los TIPOS de Plots Origin
    
    origin_types <- unique(data_day$plot_origin)
    long_ot <- length(origin_types)
    
    origin_types_vec <- c()
    
    for (i in 1:long_ot) {
      origin_types_vec <- append(origin_types_vec, origin_types[i])
    }
    
     
    
    # ............. PLOT ORIGEN .................
    # ...............................................
    
    #   .) Para OBTENER el PLOT_ORIGIN seccionado
    #   .) Usamos el ORIGEN del COMBO
    #   .) Y lo "traducimos" al string que usa el DATA_DAY_FILTER
    
    origenSelected <- function(a) {
      
      switch(a,
              "T"  = origin_types_vec,
              "PN" = c("aiguestortes","ordesa"),
              "P"  = "ifn",
              "A"  = "aiguestortes",
              "S"  = "matollar",
              "O"  = "ordesa"
            )
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
    
    #      .) la función ORGENG_SELECTED nos da 6 origenes
    #           .) (Todo, Paraque Nacional, Aiguestortes, Ordesa, Nfi y Matollar)
    
    #      .) El ORIGEN_SELECTED puede dar vectores:
    #           .) c("aiguestortes","ordesa"),
    #           .) c("ifn","aiguestortes","matollar","ordesa"),
    #           .) "matollar"
    
    #      .) Por eso usamo para filtrar el  =>  %in% 
    #      .) Así nos filtra por MULTIPLIES VALORES
    
    
    # ....... FILTRADO in case PERIMETRE ...........
    # .............................................
    
    #      .) En el caso de AIGUESTORTES y ORDESA
    #      .) Tenemos que agregar los PLOTS de la zona Perimetral
    #      .) PROCESO
    #           .) Tenemos que hacer un intersect espacial para AIGUESTORTES
    #           .) DATA_DAY vs PERIMETRO
    
    #           .) Para ORDESA NO hacemo INTERSECT
    #           .) Ya que de parcela del IFN4 de fera de Catalunya i Matollar
    #           .) Solo hay Ordesa Interior Parque y Perímetro
    
    
    
    if(origen == "A"| origen == "PN") {
      
      switch (origen,
              "A"  = perimetre <- peri_aiguestortes,
              "PN" = perimetre <- peri_total
      )
      
      #PLOTS INTERSECT
      df_perimeter <- data_day %>% st_as_sf() %>%
      
        sf::st_join(., perimetre, join = st_intersects, left = FALSE  ) %>%
        dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
        dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
                      lat = sf::st_coordinates(.data$geometry)[,2])
      #PLOTS TYPE
      df <- data_day %>% st_as_sf() %>%
         
        dplyr::filter(.,plot_origin %in% origen_selected) %>%
        dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
        dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
                      lat = sf::st_coordinates(.data$geometry)[,2])  
      

      data_filter <-  rbind(df,df_perimeter)
      
    } else {
      
      data_filter <- data_day %>%
        
        dplyr::filter(.,plot_origin %in% origen_selected) %>%
        dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
        dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
                      lat = sf::st_coordinates(.data$geometry)[,2])
    
      
    }
    
    
    # ......... VARIABLES PALETA ..........
    # .....................................
    
    #      .) Creamos VARIABLES VALORES  
    #      .) Creamos VARIABLES VALORES no NA
    #      .) Son la valores de variables que se presentan en:
    #            .) Paleta Color Plot
    #            .) Paleta Color Leyenda (los mismos sin valores NA)
    
   
    variable_valores <- round(data_filter[[2]], digits=2)
    

    #      .) ELIMINAMOS los NA de la LEYENDA
    #      .) Sino visualmente se complica
    #      .) Y se tendría que vanviar el CSS de la Leyenda

    
    variable_valores_noNA <- variable_valores[!is.na(variable_valores)]
    
    
    # ..... PROBLEMA NA valores ORDESA ......
    # ........................................
    
    #      .) SOLUCIÓN
    #            .) Detecto SI TODOS los valors de VARIABLES VALORES son NA
    #            .) unique(is.na(variable_valores))
    #            .) Pero AÑADO  [1]  
    #            .) ya que aveces devuelve FALSE y TRUE ala vez
    #            .) Y solo no interesa el 1r BOOLEANO
    
    #      .) Como que DOMANIN son TODOS NA
    #      .) Le asigno un valor ...0 x ejemplo
    #      .) Y entonces ya no peta.
    #      .) Marca todos los plots en gris
    
    
    # ============  CREACIÓN PALETA COLORES PLOT/LEGEND  =============
    # ================================================================
    
    #      .) Creo toda la ESTRUCTURA
    #      .) El resultado final es
    #             .) pal_plot
    #             .) pal_legend 
     
    
    if( unique(is.na(variable_valores))[1] ){

      value <- 0
      pal_plot <- leaflet::colorNumeric(palette = palettes_dictionary[[variable]][['pal']], domain = value, reverse = FALSE)
      pal_legend <- leaflet::colorNumeric(palette = palettes_dictionary[[variable]][['pal']], domain = value, reverse = TRUE)
    
      value_legend <- variable_valores 
      
 
    } else {
      
      # ........ VALUE / VALUE_LEGEND .........
      # ........................................
        
      #      .) Creo dos tipos de valores
      #      .) VALUE        = Valores del PLOT
      #      .) VALUE_LEGEND = Valores de la Leyenda ( 2 TIPOS )
      
      #      .) RESTRICCIONES:
      #             .) SIMPRE RANGO (0-100)
      #             .) Para c("DDS", "REW_q","DDS_q","LFMC_q")
      
      #             .) SIMPRE RANGO (0-9)
      #             .) Para c("SFP","CFP")
      
      #             .) El resto de su MAX a su MIN
      #             .) Pero LEYENDA sin NA
      
      
      
      
      if ( is.element(variable, c("DDS", "REW_q","DDS_q","LFMC_q")) ) {

        x1 <- data_filter[[2]]
        x2 <- append(x1, 0, 0)
        value <- append(x2, 100, 0)

        y1 <- variable_valores_noNA
        y2 <- append(y1, 0, 0)
        value_legend <- append(y2, 100, 0)

      } else if (is.element(variable, c("SFP","CFP"))) {

        x1 <- data_filter[[2]]
        x2 <- append(x1, 0, 0)
        value <- append(x2, 9, 0)

        y1 <- variable_valores_noNA
        y2 <- append(y1, 0, 0)
        value_legend <- append(y2, 9, 0)

      } else {

        value <- data_filter[[2]]
        value_legend <- variable_valores_noNA

      }
      

        
        # ........ VARIABLE TIPO QUANTIL .........
        # ........................................
        
        #      .) Creo una f(x)
        #      .) Para saber si una VARIABLE es tipo QUANTIL (DDS_q,...)
        #      .) Devuelve TRUE
        
        
        is_quantil <- function(variable){ 
          
          grepl("_",variable, fixed = TRUE) 
          
        }
        
        # ...... LEYENDA para VALORES EXTREMOS ........
        # ..............................................
        
        #      .) Creo un leyenda para valores extremos
        #      .) Son tipos de color que pueden ayudar a visualizar algunos resultados
        
        #      .) https://stackoverflow.com/questions/49126405/how-to-set-up-asymmetrical-color-gradient-for-a-numerical-variable-in-leaflet-in
        
        #      .) Es una leyenda donde si hay pocs valores grandes y muchos de pequeños
        #      .) Corrige el rango de la escala de color para que los menores destaquen
        
        #      .) RC1 = 20  colores para RAMPA (valores GRANDES)
        #      .) RC2 = 180 colores para RAMPA (valores PEQUEÑOS)
        
       
       red <- "#a60818"
       yellow <- "#f7fc60"
       blue <- "#272cc2"
        
       rc1 <- colorRampPalette(colors = c("red", "orange"), space = "Lab")(20)
       rc2 <- colorRampPalette(colors = c("orange", "white"), space = "Lab")(180)
       
       rc3 <- colorRampPalette(colors = c(red,yellow), space = "Lab")(20)
       rc4 <- colorRampPalette(colors = c(yellow,blue), space = "Lab")(180)
        
        rampcols_a <- c(rc1, rc2)
        rampcols_b <- c(rc3, rc4)
         
        switch (leyenda_modif,
                "estandard" = palete_value <- palettes_dictionary[[variable]][['pal']],
                "tip_1"     = palete_value <- rampcols_a,
                "tip_2"     = palete_value <- rampcols_b
        )
        
        
        # ........ PALETA PLOT / LEGEND  .........
        # ........................................
        
        #      .) Hay dos paletas = PLOT y LEGEND
        #      .) usamos reverse = FALSE/TRUE para alterar el orden de colores (plot y legend)
        
        #      .) PAL_PLOT    = Paleta Color Plots
        #      .) PAL_LEGEND  = Paleta Color Leyenda
        
        #      .) SI es variable QUANTIL o NO QUANTIL
        #      .) El sentido (REVERSE) de colores Leyenda y Plots es de diferente
        

        if (is_quantil(variable)) {   
          
          pal_plot   <- leaflet::colorNumeric(palette = palete_value, domain = value , reverse = FALSE)
          pal_legend <- leaflet::colorNumeric(palette = palete_value, domain = value_legend , reverse = TRUE)
          
        } else {
          
          pal_plot   <- leaflet::colorNumeric(palette = palete_value, domain = value , reverse = TRUE)
          pal_legend <- leaflet::colorNumeric(palette = palete_value, domain = value_legend , reverse = FALSE)
            
        }
     
    }


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
  
      
      
       
      # ............ CREACIÓN LABELS .................
      # ..............................................
      
      #     .) Creo los LABELS información que aparece 
      #     .) Esta info aparece en pasar el Mouse por encima
      #     .) https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf
      #     .) SPRINTF => pasa info a STRING
      #              .) CADA % indica un value ( s = string, g = double precision,...)
      #              .) El ORDEN de declarar values es el ORDEN de APARICION
      
                         
      labels_plot <- sprintf( "<strong>%s</strong><br/> %s <br/> %s = %g ",
                          translate_app("PLOT",lang_declared),
                          data_filter$plot_id, variable, variable_valores) %>% lapply(htmltools::HTML)
      
     
      # ........ PROYECCIÓN PLOTS  ........
      # ...................................
      
      
      leaflet::leafletProxy('map_daily') %>%
      leaflet::clearGroup('plots_layer') %>%
        leaflet::addCircles(
          data = data_filter,
          group = 'plots_layer',
          layerId = ~plot_id,
          lat = ~ lat,
          lng = ~ lon,
          weight= 0,
          opacity= 0.8,
          fillOpacity= 0.6,
          radius = radi,
          color = ~ pal_plot(data_filter[[2]]),
          label = labels_plot,
          labelOptions = labelOptions(interactive = TRUE)) %>%

        leaflet::clearControls() %>%
        leaflet::addLegend(
          position = "bottomright",
          title = translate_app(variable, lang_declared),
          pal = pal_legend,
          values = value_legend,
          labFormat = labelFormat(transform = function(x) rev(x)),
          opacity = 1)

      

      
})
  
 
 

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # -------------------------    OBVSERVE EVENT     ------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  # =============== CLIK MAPA ==============
  # ========================================
  
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
    eventExpr = input$map_daily_shape_click,
    handlerExpr = {
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset_plots',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )
  
  
  # ============= ZOOM POLIGONS ============
  # ========================================
  
  #     .) Dependiendo de la selección del COMOBO ORIGEN
  #     .) Haremos un ZOOM a uno o otro polígono
  #       
  
  
  shiny::observeEvent(
    eventExpr = data_reactives$origen_reactive,
    handlerExpr = {
      
      origen <- data_reactives$origen_reactive
      
      # .......... ZOOM POLIGONS ..........
      # ...................................
      
      #     .) Creamos f(x) SET_VIEW
      #     .) Dentro en funcion de ORIGEN
      #     .) Setearemos uno o otro zoom
      
      
      set_view <- function(a) {
        
        switch(a,
               
           "T" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(2.2018256,41.089058, zoom=7),
           "P" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(1.7458675,41.6922353, zoom=8),  
           "PN" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.502806,42.533565, zoom=10),
           "A" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.9313699,42.5709769, zoom=11),
           "S" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(2.2018256,41.089058, zoom=7),
           "O" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.0519259,42.6598781, zoom=11)
        )
      }   
      
      
      # ............ PROYECCIÓN Zonas PERIFÉRICAS ...............
      # ..............................................
      
      #     .) Primero PROYECTAMOS Periferia
      #     .) Así quedará por debajo de los otro polígonos
      
      #     .) Proyeccion independiente a los Parques Naturales
      #     .) El color y grosor seran diferentes
      
      if (origen == "A" | origen == "PN" | origen == "O") {
        
        switch (origen,
                "A"  = perimetre <- peri_aiguestortes,
                "O"  = perimetre <- peri_ordesa  ,
                "PN" = perimetre <- peri_total
        )
        
        set_view(origen) %>%
          leaflet::clearGroup('polygons_perimetre') %>%
          leaflet::addPolygons(
            data = perimetre,
            weight = 1.1,
            opacity = 0.5,
            fillOpacity = 0,
            color = "#004202",
            group = "polygons_perimetre")
        
      } else {
        
        set_view(origen) %>% 
          leaflet::clearGroup('polygons_perimetre')
      }
      
      
      # ............ PROYECCIÓN POLIGONOS  ..........
      # ..............................................
      
      
      #     .) Proyecta en función del tipo de ORIGEN
      #     .) FUNCIÓN DIVSION SELECT
      #              .) En función del COMBO ORIGEN 
      #              .) Proyectara uno o otro SF (Shapes)
      
      polygon_selected <- function(origen) {
        
        switch(
          origen,
          "T" = all_polygons,
          "A" = aiguestortes,
          "PN" = parques,
          "O" = ordesa,
          "P" = catalunya, 
          "S" = provincias )
      }
      
      # ............ STYLES POLIGONOS  ..........
      # ..............................................
      
      
      #     .) En funcion de PARQUES NACIONALE o NO
      #     .) Tendremos uno o otro style
      
      color_polygon <- function(origen){
      
        if(origen == "A" | origen == "PN" | origen == "O"){ return("#c41606") }
        else { return("#65656e")  }
      }
      
      opacity_polygon <- function(origen){
        
        if(origen == "A" | origen == "PN" | origen == "O"){ return(1) } 
        else { return(0.5)  }
      }
      
      weight_polygon <- function(origen){
        
        if(origen == "A" | origen == "PN" | origen == "O"){ return(1.5) } 
        else { return(1)  }
      }
      

      set_view(origen) %>%
        leaflet::clearGroup('polygons') %>%
        leaflet::addPolygons(
          data = polygon_selected(origen),
          weight = weight_polygon(origen),
          opacity = opacity_polygon(origen),
          fillOpacity = 0,
          color = color_polygon(origen), 
          group = "polygons")
      
      
      
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
    
    map_reactives$click_circle <- input$map_daily_shape_click
                             
  })
  return(map_reactives)


  

}
