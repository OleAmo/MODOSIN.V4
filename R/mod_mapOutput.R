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
    
    
    value <- data_filter[[2]]
    max <- max(value)
    min <- min(value)

    if(    max(value) ==  min(value)  ) {
      df_unique <- sort(c(max(value),max(value)+0.011,max(value)+0.012,max(value)+0.013,max(value)+0.014))
    } else {
      df_unique <- value %>% unique()
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
    
    # ........ FUNCION SIZE RADIO ............
    # ........................................
    
    #     .) Función que determina el tamaño del Radio/Parcela
    #     .) En f(x) del que hayamos seleccionado en RADIO BUTTON SIZE
    #     .) Hará que el radio se:
    #            .) Estandard = SIMPRE VALOR 6
    #            .) Variable = en f(x) del rango de la Variable seleccionada
    
    
    size_radi = function(a){
      
      # ..... FUNCION RANGO/PALETA COLOR  ......
      # ........................................
      
      #     .) Hay rango (min-max) de paletas que son demasiado grandes
      #     .) Ej. => LAI (0-34.000) y visualiza parcelas demasiado grandes
      #     .) Para evitar eso
      #            .) Calculo el Mínimo / Máximo
      #            .) Y si es > 20 le asigno un tope de 6 como radio
      #            .) Si es < 20 el radio serà en f(x) de la variable
      
      
      num <- as.numeric(match(variable,names(data_filter)))  # indice de la variable en DATA_FILTER
      
      summary <- summary(data_filter[num])                   # calculo SUMMARY en funcion del Índice de la variable
      
      min <- summary[1] %>% strsplit(split = ":") %>% 
        .[[1]] %>% .[2] %>% as.numeric()
      
      max <- summary[6] %>% strsplit(split = ":") %>% 
        .[[1]] %>% .[2] %>% as.numeric()
      
      if(a == "const"){ return(6) }             # si seleccionamos CONSTANTE en el combo => nos darà radio 6 siempre
      else {
        if( abs(max-min) > 20 ){ return(8) }      # si seleccionamos VARIABLE en el combo
        else {  return(~ variable_valores) }      # si (max-min) > 20 nos darà radio de 6 siempre
      }}
    
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
   
    
    
    if(data_reactives$legend_reactive == "conti") {

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
          radius= size_radi(data_reactives$size_reactive),
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
          radius= size_radi(data_reactives$size_reactive),
          color =  ~ qpal(data_filter[[2]]),
          popup = popInfo) %>%
        leaflet::clearControls() %>%
        leaflet::addLegend(
          position = "bottomright",
          title = paste(as.character(selected_var),' (Quantiles) '),
          colors = qpal_colors,
          labels = qpal_labs,
          opacity = 1)
   
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
