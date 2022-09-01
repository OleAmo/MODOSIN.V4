#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @details mod_mainData always return the data in the 3043 projection
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,map_reactives reactives from modules
#' @param catdroughtdb object to access the meteoland db
#' @param lang lang selected
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, map_reactives,
  modosindb, lang
) {
  

  # ....... WAITER / HOSTESS ..........
  # ...................................
  
  #       .) https://shiny.john-coene.com/waiter/
  #       .) Paquete de R que permite crear LOADING SCREENS
  
  #       .) INICIALIZAMOS:
  #              .) Fuera de REACTIVE careamos OBJECTO con la classe:
  #              .) WAITER::HOSTESS$new
  #       .) SEGUNDO
  #              .) SET_LOADER = image SVG, tipo progress y fill direction
  
    
  

  hostess_plots <- waiter::Hostess$new(infinite = TRUE)
  hostess_plots$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))

  
  # **************************************************************************************
  # ------------------------------     REACTIVE   ----------------------------------------
  # **************************************************************************************
  
 
  
  # ***************************************************
  # ------------------    DATA DAY   ------------------
  # ***************************************************
  
  library(data.table)
  
  data_day <- shiny::reactive({
    
    
    #       .) Devolvemos el SF de todos Plots / Todas fechas
    #       .) Creamos el LOADING SCREEN minentra cargamos PLOTS
    
    #       .) IMPORTANTE:
    #       .) Antes de todo VALIDAMOS FECHA
    #       .) Sin esto la app se bloquea
    
    
    shiny::validate(
      shiny::need(data_reactives$fecha_reactive, 'No date selected')
    )
    
    # ....... WAITER / HOSTESS ..........
    # ...................................
    
    #       .) TERCERO:
    #       .) Definir LUGAR de aparición = ID ( en APP.R / mainPanel)
    #       .) Definir => Get_Loader() definido anteriormente + HTML H3 + P
    

    waiter_map <- waiter::Waiter$new(
      id = 'overlay_div',
      html = shiny::tagList(
        hostess_plots$get_loader(),
        shiny::h3(translate_app("progress_plots", lang())),
        shiny::p(translate_app("progress_detail_plots", lang()))
      ),
      color = "#E8EAEB"  # color del fondo
       
       
    )
    #       .) CUARTO: Show MAP + Star HOSTESS
    #       .) QUINTO: Definir EXIT Hostess / Map 
    
    
    waiter_map$show()
    hostess_plots$start()
    on.exit(hostess_plots$close(), add = TRUE)
    on.exit(waiter_map$hide(), add = TRUE)
    
     
    
    # ........... GET DATA ..............
    # ...................................
    
    #       .) Usamos MODOSIN DB (Definido en APP.R)
    #       .) Llamamos al M?todo GET DATA
    #       .) Creamos el SF data_day
    #       .) Es el SF de TODOS los PLOTS / TODAS las fechas
    #       .) Despu?s ya lo filtraremos

    # ..... PROBA 1ra x la APP ......
    # ...............................
    # data_day <- modosindb$get_data("data_day_petita")
    
    # ..... PROVA 2da x la APP ......
    # ...............................
    # data_day <- modosindb$get_data("data_day_fire_petita")
    
    # ..... Definitiva 2..........
    # ...........................
    data_day <- modosindb$get_data("data_day_fire")
    
    # ..... Definitiva 1..........
    # ...........................
    # data_day <- modosindb$get_data()
    return(data_day)
  })

  
  
  
  
  # ***************************************************
  # -----------------    TIME SERIE   -----------------
  # ***************************************************
  
  
  
  time_serie <- shiny::reactive({
    
    
    shiny::validate(
      shiny::need(main_data_reactives$data_day, 'No data_day selected'),
      shiny::need(map_reactives$map_daily_marker_click, 'No clicked')
    )
    
    # ......... INICIALIZAR DATOS ............
    # ........................................
    
    #      .) FECHA / VARIABLE  / data_day_clicked_plot
    #      .) Obtenidos de los COMBOS 
    #      .) PLOT ID clickado en MAPA
    #      .) DATA_DAY
    
    variable <- data_reactives$variable_reactive
    fecha <- data_reactives$fecha_reactive
 
    
    # ............ CLICK PLOT ID .............
    # ........................................
    
    #      .) USAMOS = "map_reactives$map_daily_marker_click"
    #      .) DECLARADO en los EVENTOS en MOD_MAPOUTPUT.R
    
    #      .) obtengo PLOT ID 
    #      .) Declarado en LEAFLET ADDCircleMARKERS ( layerId = ~ plot_id)
    #      .) Usando $id
    
    
    click_plot_id <- map_reactives$map_daily_marker_click$id
    

    # .............. DATA DAY  ...............
    # ........................................
    
    #       .) Son los PLOTS de TODO el a?o
    #       .) Definidio en ESTE MISMO MODULO
    
    data_day<- main_data_reactives$data_day

    
    
    data_day_clicked_plot <- data_day %>% dplyr::filter(plot_id == click_plot_id)
    
    #      .) NUM_i
    #             .) N?mero de la columnas de la variable
    #             .) Lo usaremos par obtener TODAS los valores de UNA VARIABLE
    
    #      .) FECHA INICIAL = 1r dia de datos
    #             .) Lo usaremos para indicar el inicio de la fechas de gr?ficos

    #      .) VALUE DATE = valor de la variable en la fecha concreta
    #             .) Lo usaremos en LABEL EVENT
    
    num_i <- as.numeric(match(variable,names(data_day_clicked_plot)))
    fecha_inicial <- data_day_clicked_plot$date[1]
    value_date <- data_day_clicked_plot %>%
      dplyr::filter(date == fecha) %>%
      .[num_i] %>%
      .[[1]] %>%
      round(., digits = 4)

    #      .) LABEL EVENT
    #             .) Texto que saldr? en el grafico
    #             .) Indica fecha seleccionada + valor de variable escogido
    
    #      .) UNITS
    #             .) Son la DESCRIPCI?N de la VARIABLE y las UNIDADES
    #             .) Lo usaremos al AXIS Y
    
    #      .) LABEL AXIS
    #             .) Texto que saldr? en la coordenada Y
    #             .) Indica fecha seleccionada + valor de variable escogido
    

    label_event <- paste(fecha," = ",variable," (",value_date,") ")
    units <- translate_app(variable, "eng")
    label_axis <- paste(toupper(variable)," [ ",units," ]")
    
    # ..................... GRAPHS ......................
    # ...................................................
    
    #       .) Creo FORMATO TS (Time Serie)
    #       .) Aplico EDICION con DYGRAPHS
    
    #             .) MAIN = T?tulo
    #             .) AXIS = edito las Y
    #             .) OPTIONS = edito gr?fico
    #             .) SERIE = texto del men? que sale en mover el mouse
    #             .) EVENT = en la fecha concreta escribir texto
    #             .) SHADING = Sombreado entre fecha y fecha
    #             .) RANGE SELECTOR = para hacer zoom al gr?fico


    data_day_graph <- ts(data_day_clicked_plot[num_i][[1]], frequency = 1, start = as.Date(fecha_inicial))
    
    res <- data_day_graph %>%
              dygraphs::dygraph(. , main = paste(toupper(variable)," (Anual) - PLOT ( Id = ",click_plot_id," / ",fecha,")")) %>%
              dygraphs::dyAxis("y", label = label_axis )%>%
              dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
              # dygraphs::dySeries(label = variable) %>%
              dygraphs::dyEvent(fecha, label_event, labelLoc = "bottom") %>%
              dygraphs::dyShading(from = as.Date(fecha)-7, to = as.Date(fecha)+7, color = "#f2a7a7") %>%
              dygraphs:: dyRangeSelector()

    return(res)
    
    

  })
  
  
  
  

  # map_shape_sf_builder <- shiny::reactive({
  #   shiny::validate(
  #     shiny::need(map_reactives$map_daily_shape_click, 'no map click')
  #   )
  # 
  #   clicked_poly <- map_reactives$map_daily_shape_click
  #   polygon_object <- rlang::eval_tidy(
  #     rlang::sym(glue::glue("{tolower(data_reactives$display_daily)}_polygons"))
  #   ) %>%
  #     dplyr::filter(
  #       poly_id == clicked_poly$id
  #     )
  # 
  #   return(polygon_object)
  # })
  # 
  # nfi_plots_sf_builder <- shiny::reactive({
  #   shiny::validate(
  #     shiny::need(map_reactives$map_daily_marker_click, 'no map click')
  #   )
  # 
  #   clicked_marker <- map_reactives$map_daily_marker_click
  # 
  #   point_sel <- tibble::tibble(
  #     point_id = clicked_marker$id,
  #     long = clicked_marker$lng,
  #     lat = clicked_marker$lat
  #   ) %>%
  #     sf::st_as_sf(
  #       coords = c('long', 'lat'),
  #       crs = 4326
  #     )
  # 
  #   return(point_sel)
  # }



  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................
  
  #      .) Creamos MAIN DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS
  
  # .... MAIN DATA REACTIVE ......
  # ..............................
  
  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $
  #      .) Devolvemos DATA_DAY
  
  main_data_reactives <- shiny::reactiveValues()
  
  shiny::observe({
   
     main_data_reactives$data_day <- data_day()
     main_data_reactives$timeserie <-  time_serie()
     
  })
  
  return(main_data_reactives)

}
