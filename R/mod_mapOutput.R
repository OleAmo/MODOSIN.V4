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
    
    ifelse(origen == "S", n <- 2,n <- 1)
    
    #   .) ZOOM ESTANDARD
    #            .) En f(x) del zoom
    #            .) Aplicamos un tamaño
    
    if (current_zoom <= 5) { size_transformed <- 750 * 0.25 * n
      
    } else if (current_zoom == 6) { size_transformed <- 750 * 4 * n 
    
    } else if (current_zoom == 7) { size_transformed <- 750 * 4 * n
    
    } else if (current_zoom == 8) { size_transformed <- 750 * 2 * n
    
    } else if (current_zoom == 9) { size_transformed <- 750 * n  
    
    } else if (current_zoom == 10) { size_transformed <- 750 * n 
    
    } else if (current_zoom == 11) { size_transformed <- 750 * 0.6 * n 
    
    } else if (current_zoom == 12) { size_transformed <- 750 * 0.3 * n
    
    } else if (current_zoom >= 13) { size_transformed <- 750 * 0.3 * n
    
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
    
    #      .) VARIABLE:
    #            .) Es la variable a PROYECTAR
    #            .) La obtenemos del REACTIVE constante => MOD_DATAINPUT
    
    
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
    legend_type <- data_reactives$legend_reactive  
    
    radi <- radi_size()
    
    print(paste0('ZOOM = ',input$map_daily_zoom,' / RADI = ',radi))
    
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
    
    # ............. PLOT ORIGEN .................
    # ...............................................
    
    #   .) Para OBTENER el PLOT_ORIGIN seccionado
    #   .) Usamos el ORIGEN del COMBO
    #   .) Y lo "traducimos" al string que usa el DATA_DAY_FILTER
    
    
    origenSelected <- function(a) {
      
      switch(a,
              "T"  = c("ifn","aiguestortes","matollar","ordesa"),
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
    
    #      .) la función ORGENG_SELECTED nos da 4 origenes (Aiguestortes, Ordesa, Nfi y Matollar)
    #      .) Si queremos que nos lo proyecte TODO
    #      .) Tenemo de decirle que NO FILTRE
    #      .) Por eso hago un PIPE con IF
    #           .) Y si ORIGEN NO ES T (T = es todos) me filtra por ORIGEN_SELECTED
    #           .) Si ES T, no hace filtro y lo PROYECTA TODO
    
    data_filter <- data_day %>%
     
      dplyr::filter(.,plot_origin == origen_selected) %>%
      dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
      dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
                    lat = sf::st_coordinates(.data$geometry)[,2]) 
    

    variable_valores <- round(data_filter[[2]], digits=2)
    
    #      .) ELIMINAMOS los NA de la LEYENDA
    #      .) Sino visualmente se complica
    #      .) Y se tendría que vanviar el CSS de la Leyenda
    
    variable_valores_legend <- variable_valores[!is.na(variable_valores)]
     
    

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
    
 

    # if (is_quantil(variable)) {
    #   pal_plot <- leaflet::colorNumeric(palette = "RdYlBu", domain = data_filter[[2]] , reverse = FALSE)
    #  
    #   pal_legend <- leaflet::colorNumeric(palette = "RdYlBu", domain = data_filter[[2]] , reverse = TRUE)
    # } else {
    #   pal_plot <- leaflet::colorNumeric(palette = "plasma", domain = data_filter[[2]] , reverse = TRUE)
    #   pal_legend <- leaflet::colorNumeric(palette = "plasma", domain = data_filter[[2]] , reverse = FALSE)
    # }
    
    if (is_quantil(variable)) {
      pal_plot <- leaflet::colorNumeric(palette = palettes_dictionary[[variable]][['pal']], domain = data_filter[[2]] , reverse = FALSE)
      
      pal_legend <- leaflet::colorNumeric(palette = palettes_dictionary[[variable]][['pal']], domain = data_filter[[2]] , reverse = TRUE)
    } else {
      pal_plot <- leaflet::colorNumeric(palette = palettes_dictionary[[variable]][['pal']], domain = data_filter[[2]] , reverse = TRUE)
      pal_legend <- leaflet::colorNumeric(palette = palettes_dictionary[[variable]][['pal']], domain = data_filter[[2]] , reverse = FALSE)
    }
    
    
    
    
    
    # ...... PALETA DE COLORES QUANTIL .......
    # ........................................
    
    #       .) Para hacer los QUANTILES
    #       .) NECESSITAMOS: Valores Únicos de la variable (TODOS PLOTS)
    #                .) DF_UNIQUE <-data_filter[[2]] %>% unique()
    
    # .ATENCIÓN:
    
    #       .) Si una variable tiene SIEMPRE el MISMO VALOR
    #       .) Ejemplo DDS = (0, 0, 0, 0, ....0)
    #       .) Tenemos que corregir para que no se bloqueo el LEAFLET
    #                .) DF_UNIQUE = Le damos 5 valores
    #                .) Así el QPAL_LABS = Crearà las etiquetas correctas
    
    
    # modosindb <- lfcdata::modosin()
    # data_day <- modosindb$get_data('data_day_fire_petita_2')
    # 
    # variable <- "REW_q"
    # fecha <- "2022-1-15"
    # origen_selected <- "ifn"
    # 
    # num_i <- as.numeric(match(variable,names(data_day)))
    # selected_var <- as.symbol(names(data_day)[num_i])
    # 
    # data_filter <- data_day %>%
    # 
    #   dplyr::filter(.,plot_origin == origen_selected, date == fecha) %>%
    #   dplyr::select(plot_id, selected_var, date, plot_origin, geometry) %>%
    #   dplyr::mutate(lon = sf::st_coordinates(.data$geometry)[,1],
    #                 lat = sf::st_coordinates(.data$geometry)[,2])
    # 
    # 
    # variable_valores <- round(data_filter[[2]], digits=2)
    
    val <- data_filter[[2]]
    value <- val[!is.na(val)]  # eliminamos los NA
    
    max <- max(value) 
    min <- min(value)
    
    if(    max ==  min  ) {
      df_unique <- sort(c(max, max+0.011, max+0.012, max+0.013, max+0.014))
    } else {
      df_unique <- value  %>% unique()
    }
    
    #       .) Creamos Función COLORQUANTILE
    #       .) QPAL = Le indicamos:
    #                .) PALETA ("RdYlBu")
    #                .) DATOS (únicos)
    #                .) N = numero de breaks
    #       .) QPAL_COLORS = Usando la función nos da COLORES para cada BREAK
    #       .) QPAL_LABS 1 = para cada break indicamos los valores
    #       .) QPAL_LABS 2 = creamos el RANGO escrito
    
    
    qpal <- leaflet::colorQuantile("RdYlBu", df_unique, n = 5)    # Función COLORQUANTILE
    qpal_colors <- unique(qpal(sort(df_unique)))         # Colores para cada BRAKE
    qpal_labs <- quantile(round(df_unique, digits = 2), seq(0, 1, .2)) # Valores para cada Break
    
    #       .) Aparte CREAMOS el Rango de Break (20%, 40%,...)
    #       .) Y lo unimos con PASTE = VEC + QPAL_LABS 
    
    vec <- c()
    long <- length(qpal_labs)
    
    # Crear VECTOR con cada break (20% 40% ...)
    for (i in 1:long) {          
      n <- names(qpal_labs[i])
      vec <- append(vec, n)
    }
    
    # creamos el Rango Escrito (inicial)
    qpal_labs <- paste(lag(qpal_labs), round(qpal_labs,digits = 2), sep = " - ")[-1]
    
    
    # creamos el Rango Escrito (FINAL) = QPAL_LABS + VEC + QCOLORS
    long <- length(vec)          
    for (i in 2:long) {
      qpal_labs[[i-1]] <- paste('[',vec[i],'] ',qpal_labs[[i-1]])
      
    }
    
    
    # ........... EJEMPLO CUANTIL ............
    # ........................................
    
    #       .) Para la LEYENDA nos hace falta
    #       .) QPAL_COLORS (  colores que saldran en la leyenda)
    #       .) QPAL_LABELS ( % y Rango de Valores de la leyenda)
    
    
    #       QPAL_COLORS:
    #       [1] "#D7191C" "#FDAE61" "#FFFFBF" "#ABD9E9" "#2C7BB6"
    
    #       VEC:
    #       [1] "0%"   "20%"  "40%"  "60%"  "80%"  "100%"
    
    #       QPAL_LABS:
    #       [1] "0.27 - 0.27" "0.29 - 0.29" "0.3 - 0.3"   "0.32 - 0.32" "0.37 - 0.37"
    
    #       QPAL_LABS (unido):
    #       [1] "[ 20% ]  0.27 - 0.27"  "[ 40% ]  0.29 - 0.29"  "[ 60% ]  0.3 - 0.3"   
    #       [4] "[ 80% ]  0.32 - 0.32"  "[ 100% ]  0.37 - 0.37"
    
    
    
    
    
  
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
      
      
      
      
     if (legend_type == "estandar") {
       
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
           # color =  ~ qpal(data_filter[[2]]),
           label = labels_plot,
           labelOptions = labelOptions(interactive = TRUE)) %>%
         
         leaflet::clearControls() %>%
         leaflet::addLegend(
           position = "bottomright",
           title = translate_app(variable, lang_declared),
           
           pal = pal_legend,
           values = variable_valores_legend,
           labFormat = labelFormat(transform = function(x) rev(x)),
           
           
           # colors = qpal_colors,
           # labels = qpal_labs,
           
           
           opacity = 1)
       
       
     } else {
       
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
           # color = ~ pal_plot(data_filter[[2]]),
           color =  ~ qpal(data_filter[[2]]),
           label = labels_plot,
           labelOptions = labelOptions(interactive = TRUE)) %>%
         
         leaflet::clearControls() %>%
         leaflet::addLegend(
           position = "bottomright",
           title = translate_app(variable, lang_declared),
           
           # pal = pal_legend,
           # values = variable_valores_legend,
           # labFormat = labelFormat(transform = function(x) rev(x)),
           
           
           colors = qpal_colors,
           labels = qpal_labs,
           
           
           opacity = 1)
       
       
     }
      
      
      # leaflet::leafletProxy('map_daily') %>%
      # leaflet::clearGroup('plots_layer') %>%
      #   leaflet::addCircles(
      #     data = data_filter,
      #     group = 'plots_layer',
      #     layerId = ~plot_id,
      #     lat = ~ lat,
      #     lng = ~ lon,
      #     weight= 0,
      #     opacity= 0.8,
      #     fillOpacity= 0.6,
      #     radius = radi,
      #     # color = ~ pal_plot(data_filter[[2]]),
      #     color =  ~ qpal(data_filter[[2]]),
      #     label = labels_plot,
      #     labelOptions = labelOptions(interactive = TRUE)) %>%
      # 
      #   leaflet::clearControls() %>%
      #   leaflet::addLegend(
      #     position = "bottomright",
      #     title = translate_app(variable, lang_declared),
      #     
      #     # pal = pal_legend,
      #     # values = variable_valores_legend,
      #     # labFormat = labelFormat(transform = function(x) rev(x)),
      #     
      #     
      #     colors = qpal_colors,
      #     labels = qpal_labs,
      # 
      #     
      #     opacity = 1)
      
      
      
    

      
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
               "P"  = leaflet::leafletProxy('map_daily') %>% leaflet::setView(1.7458675,41.6922353, zoom=8),  
               "PN" = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.488007,42.6306324, zoom=10),
               "A"  = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.9313699999999825,42.57097690195607, zoom=12),
               "S"  = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.5213654,41.3684307, zoom=8),
               "O"  = leaflet::leafletProxy('map_daily') %>% leaflet::setView(0.0782512,42.6215114, zoom=11)
        )
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
