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

    # ............ PROBES ...........
    # ...............................
    # data_day <- modosindb$get_data("data_day_petita")
    
    # data_day <- modosindb$get_data("data_day_fire_petita")
    
    # data_day <- modosindb$get_data("data_day_fire_petita_2")
    
    data_day <- modosindb$get_data("data_day_fire_petita_3")
    
    # ..... Definitiva 2..........
    # ...........................
    # data_day <- modosindb$get_data("data_day_fire")
    
    # ..... Definitiva 1..........
    # ...........................
    # data_day <- modosindb$get_data()
    
    return(data_day)
  })

  
  
  
  
  # ***************************************************
  # -----------------    TIME SERIE   -----------------
  # ***************************************************
  
  
  
  time_serie <- shiny::reactive({
    
    # ......... INICIALIZAR .............
    # ...................................
    
    #       .) LANG = F(x) definida en APP.R
    #       .) DATES_LANG = Cambio de nomenclatura de lengua
    
    
    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'
    )
    
    
    shiny::validate(
      shiny::need(main_data_reactives$data_day, 'No data_day selected'),
      shiny::need(map_reactives$click_circle$id , 'No clicked Circle')
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
    
    #      .) USAMOS = "map_reactives$click_circle"
    #      .) DECLARADO en los EVENTOS en MOD_MAPOUTPUT.R
    
    #      .) obtengo PLOT ID 
    #      .) Declarado en LEAFLET ADDCircle ( layerId = ~ plot_id)
    #      .) Usando $id
    
    
    click_plot_id <- map_reactives$click_circle$id
    
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

    
    
    #      .) LABEL EVENT
    #             .) Texto que saldrà en el grafico
    #             .) Indica fecha seleccionada + valor de variable escogido
    
    #      .) UNITS
    #             .) Son la DESCRIPCI?N de la VARIABLE y las UNIDADES
    #             .) Lo usaremos al AXIS Y
    
    #      .) LABEL AXIS
    #             .) Texto que saldrá en la coordenada Y
    #             .) Indica fecha seleccionada + valor de variable escogido
    
    #      .) VAR DEF
    #             .) xxxxxxxxxxxxxxxx
    
    #      .) VAR SHORT
    #             .) yyyyyyyyyyyyyyyy
    
    var_def <- translate_app(variable, lang_declared)
    var_short <- translate_app(paste0("short_",variable), lang_declared)
    var_short_q <- translate_app("short_Quantile", lang_declared)

    
    # ................ DYGRAPHS DATOS ...................
    # ...................................................
    
    #       .) Creo FORMATO TS (Time Serie)
    #       .) Es el formato que define los VALORES a Reprentar
    #       .) Definimos Frecuencia y fecha start


    data_day_graph <- ts(data_day_clicked_plot[num_i][[1]], frequency = 1, start = as.Date(fecha_inicial))
    
    max_value <- as.numeric(max(data_day_clicked_plot[num_i][[1]]))
    min_value <- as.numeric(min(data_day_clicked_plot[num_i][[1]]))
    
    # ................ LAYER DYGRAPH ....................
    # ...................................................
    
    #       .) Si queremos representar 2 grpaficos a la vez
    #       .) Necesitamos un LAYER con todas la capas
    #       .) Usamos el CBIND para unirlar
    #       .) En este caso es UNA SOLCA CAPA
    #       .) Mas adelante especificamos que si la VARIABLES es QUANTIL
    #       .) El LAYER tendrá 2 capas
    
    data_days_layers <- cbind(data_day_graph)
    
    
    # ..... VALUE DATA QUANTILE  .....
    # ................................
    
    #       .) Si los values tienen value en quantil
    #       .) Tendremos que crear A LA VEZ 2 GRÁFICOS
    #       .) Y por lo tanto no hará falta un VALUE_DATA_QUANTILE
    
    
    variables_quantiles <- c("REW_q","DDS_q","LFMC_q")
    variables_pre_quantiles <- c("REW","DDS","LFMC")
    
    

    if(variable %in% variables_pre_quantiles | variable %in% variables_quantiles) { 
      
      
      
      var   <- ifelse(variable %in% variables_pre_quantiles, variable , strsplit(variable,"_q")[[1]][1])
      var_q <- ifelse(variable %in% variables_pre_quantiles, paste0(variable,"_q"), variable)
      
      num_i <- as.numeric(match(var,names(data_day_clicked_plot)))
      num_i_q <- as.numeric(match(var_q,names(data_day_clicked_plot)))
      
      data_day_graph <- ts(data_day_clicked_plot[num_i][[1]], frequency = 1, start = as.Date(fecha_inicial))
      data_day_graph_q <- ts(data_day_clicked_plot[num_i_q][[1]], frequency = 1, start = as.Date(fecha_inicial))
      
      data_days_layers <- cbind(data_day_graph, data_day_graph_q)
      
      # .... ESTIL DYGRAPH ....
      # .......................
      
      var_def <- translate_app(var, lang_declared)
      var_short <- translate_app(paste0("short_",var), lang_declared)
      
      label_axis_q <- translate_app("quantiles_axis_label", lang_declared)
      
    }
    
    
    
    
    
    
    
    # .................. RANG VALUE .....................
    # ...................................................
    
    #       .) Creo una FUNCION 
    #       .) Para una correcta visualización de los TIME SERIES
    #       .) Necesitamos delimitar el MAX-MIN del Axis de las Y
    #       .) CFP / SFP          de 0 - 9     ( +1 para que se vea bien)
    #       .) REW .... LFMC_q    de 0 - 100   ( +10 para que se vea bien)
    #       .) "PET" ...  "DFMC"  de MIN - MAX ( + 1% max para que se vea bien)
    
    valueRange <- function(variable){
      switch (variable,
          "CFP" = c(0, 10),
          "SFP" = c(0, 10),
          "REW" = c(0, 110),
          "DDS" = c(0, 110),
          "REW_q" = c(0, 110),
          "DDS_q" = c(0, 110),
          "LFMC_q" = c(0, 110),
          
          "PET"    = c(min_value, max_value + 0.01*max_value), 
          "Precipitation"= c(min_value, max_value + 0.01*max_value),
          "LFMC"   = c(min_value, max_value + 0.01*max_value),
          "DFMC"   = c(min_value, max_value + 0.01*max_value)
      )
    }
    

    # ............... DYGRAPHS EDICION ..................
    # ...................................................
    
    #       .) Aplico EDICION con DYGRAPHS
    
    #             .) MAIN = Título
    #             .) AXIS = edito las Y
    #             .) OPTIONS = edito gráfico
    #             .) SERIE = texto del menú que sale en mover el mouse
    #             .) EVENT = en la fecha concreta escribir texto
    #             .) SHADING = Sombreado entre fecha y fecha
    #             .) RANGE SELECTOR = para hacer zoom al gráfico
    
  
    
    if (variable %in% variables_pre_quantiles ) {

    data_days_layers %>%
      dygraphs::dygraph(. , main = paste("Plot_id = ",click_plot_id)) %>%
      
      dygraphs::dySeries(label = var_short, axis = 'y') %>%
      dygraphs::dyAxis("y", label = var_def, valueRange = valueRange(var)) %>%

      dygraphs::dySeries(label = var_short_q, axis = 'y2') %>%
      dygraphs::dyAxis("y2", label = label_axis_q, valueRange = valueRange(var_q)) %>%
      
      dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.1)  %>%
      dygraphs::dyEvent(fecha, fecha, labelLoc = "top")

    } else if (variable  %in% variables_quantiles) {
      
      data_days_layers %>%
        dygraphs::dygraph(. , main = paste("Plot_id = ",click_plot_id)) %>%
        
        dygraphs::dySeries(label = var_short, axis = 'y') %>%
        dygraphs::dyAxis("y", label = var_def, valueRange = valueRange(var)) %>%
        
        dygraphs::dySeries(label = var_short_q, axis = 'y2') %>%
        dygraphs::dyAxis("y2", label = label_axis_q, valueRange = valueRange(var_q)) %>%
      
        dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.1)  %>%
        dygraphs::dyEvent(fecha, fecha, labelLoc = "top")
      
      
    } else {
      data_days_layers %>%
        dygraphs::dygraph(. , main = paste("Plot_id = ",click_plot_id)) %>%
        
        dygraphs::dySeries(label = var_short, axis = 'y') %>%
        dygraphs::dyAxis("y", label = var_def, valueRange = valueRange(variable)) %>%
        
        dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.1)  %>%
        dygraphs::dyEvent(fecha, fecha, labelLoc = "top")
    }


    # return(res)
    

  })


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
