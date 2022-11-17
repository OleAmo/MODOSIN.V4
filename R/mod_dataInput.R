#' @title mod_dataInput and siteDrought_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container')
    )
  )
}

#' siteDrought_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @export
siteDrought_data <- function(   
  input, output, session,
  siteDroughtdb, lang, main_data_reactives
   
) {

  # renderUI ####
  output$mod_data_container <- shiny::renderUI({
    
    # ......... INICIALIZAR .............
    # ...................................
    
    #       .) NS = IDs únicos
    #       .) LANG = F(x) definida en APP.R
    #       .) DATES_LANG = Cambio de nomenclatura de lengua
    
    ns <- session$ns
    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'              
    )
    

    # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # # ------       FECHAS DATE INPUT     ---------
    # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    # #       .) OBJETIVO
    # #       .) Quiero LIMITAR el CALENDARIO (DateInput)
    # 
    # #       .) NECESITO
    # #       .) Date Max / Date Min
    # #       .) En función de la TABLA (Data_day) de la BBDD
    # 
    # #       .) PROBLEMA
    # #       .) Mientras se CARGA la TABLA dela BBDD
    # #       .) Necesitamos UNA DATA para que el CALENDARIO no se BLOQUE
    # 
    # #       .) SOLUCION
    # #       .) Cuando DATA_DAY es NULL (se està cargando la TABLA)
    # #       .) Asignaré al CALENDARIO una MAX DATE y MIN DATE respecto la data de HOY
    # #       .) Después usarà el MAX y MIN en f(x) de la TABLA
    # 
    # data_day <- main_data_reactives$data_day
    # 
    # 
    # if( is.null((data_day))){
    # 
    #   date_max <- Sys.Date()+1
    #   date_min <- Sys.Date()-1
    # 
    # } else {
    # 
    #   # data_day <- main_data_reactives$data_day
    # 
    #   date_max <- max(data_day$date)
    #   date_min <- min(data_day$date)
    # 
    # 
    # }
    # 
    # 
    # 
    # dif_days <- as.numeric(difftime(date_max, date_min, units = "days"))
    # date_midel <- as.Date(date_min) + round(dif_days/2, digits = 0)
    
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ------       FECHAS DATE INPUT     ---------
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #       .) Queremos el rango de fechas de toda la base de datos
    #       .) Pero la queremos ANTES de que carge toda la bbdd
    #       .) Lo hacemos usando GETQUERY
    #       .) De forma inmediata nos da el rango de fechas
    
    
    dates_available <- pool::dbGetQuery(
      siteDroughtdb$.__enclos_env__$private$pool_conn, glue::glue(
        ' SELECT
        MAX(date) as "MAX_DATE",
        MIN(date) as "MIN_DATE"
        FROM data_day_fire_petita_3 '
      )
    )

    date_max <- as.Date(dates_available[[1]])
    date_min <- as.Date(dates_available[[2]])
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ----------     ETIQUETAS HTML5    ----------
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #       .) TAGLIST rea una definición de etiqueta HTML
    #       .) Creamos los elementos HTML5 con TAGS
    #       .) DROPDOWNS (SelectIntpu),...
    
    
    

    drought_vars <- c("REW","DDS") %>%
      magrittr::set_names(translate_app(., lang_declared))
    climate_vars <- c("PET", "Precipitation") %>%
      magrittr::set_names(translate_app(., lang_declared))
    fire_vars <- c("LFMC","DFMC","SFP","CFP") %>%
      magrittr::set_names(translate_app(., lang_declared))
    quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
      magrittr::set_names(translate_app(., lang_declared))
    
    
     
    shiny::tagList(
        
      
        # ...... SELECCION VARIABLE CAMBIENTE .....
        # .........................................
        
        #      .) Creamos un UIOUTPUT
        #      .) Queremos un SELECTINPUT que varie en f(x) de ORIGEN
        #      .) Si el ORIGEN es = MATOLLAR
        #      .) El select INPUT varia
        
        
        # shiny::uiOutput(
        #   ns('selectInput')
        # ),
        # 
      
        
        shiny::selectInput(
          ns('variable'), translate_app('var_daily_label', lang_declared),
          choices = shiny_set_names(list(
            'drought variables' = drought_vars,
            'climate variables' = climate_vars,
            'fire variables' = fire_vars,
            'Percentiles variables' = quantiles_vars
          ), lang_declared)
        ),
        
     
        

      # ........ PROBLEMA FECHA ...........
      # ...................................
      
      #       .) PROBLEMA
      #       .) Aveces el desplegable de DATE queda debajo del NAV
      #       .) Para solucionar-lo
      #           .) https://developer.mozilla.org/es/docs/Web/CSS/z-index
      #           .) Uso los Z-INDEX del CSS
      #           .) El Z-INDEX indica PRIORIDA de aparecer ENCIMA
      #           .) Como MAYOR el Z-INDEX mas encima de todo
      #       .) El cambio lo hago en archivo CSS (siteDrought_settings.R)
    
        
      shiny::dateInput(
        ns("fecha"), translate_app('date_daily_label', lang_declared),
        value = date_max,
        format = "yyyy/mm/dd",
        max = date_max,
        min = date_min
      ),
      
      
      # ...... SELECCION ORIGEN PLOT ......
      # ...................................
      
      shiny::selectInput(
        ns('origen'), translate_app('plot_origin_label', lang_declared),
        shiny_set_names(c(
          "T"="T",
          "P"="P",
          "PN"="PN",
          "A"="A",
          "O"="O",
          "S"="S"), lang_declared)
      ),  
      
      # ..... RADIO BUT LEGEND COLOR ......
      # ...................................
      
      #      .) Dejo COMENTADA el CANVIO de COLOR de LEYENDA
      #      .) Me espero a hablar-lo con Miquel y Víctor
      
      shiny:: radioButtons(
        ns("legend_modify"),translate_app("type_legend_label", lang_declared),
        shiny_set_names(c("estandard_label" = "estandard",
                          "1st_label" = "tip_1",
                          "2nd_label" = "tip_2"),lang_declared)
      ),
      
      
      # ... CHEK BUTTON LEGEND INVERT .....
      # ...................................
      
      #      .) Check Button
      #      .) Para invertir Leyenda
      
      shinyWidgets::prettyCheckbox(
        ns('legend_check'),
        translate_app('reverse_legend', lang_declared),
        status = 'success', shape = 'curve', fill = TRUE
      )
      
      

    ) # end of tagList

  }) 
  
  
  
  # %%%%%%%%%%%%%%%%%%%%%%   OBSERVE EVENT  %%%%%%%%%%%%%%%%%%%%%%
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #      .) Es un OBSERVEREVENT de => ORIGEN 
  #      .) Activa un UPDATESELECTINPUT
  
  #      .) Cada vez que variamos ORIGEN
  #      .) MODIFICA el SELECT INPUT Original
  #              .) Si seleccionamos MATOLLAR
  #              .) No aparecerá FOC CAPAÇADA
  
  
  
  
  
  shiny::observeEvent(
    eventExpr = input$origen,
    handlerExpr = {
      
      origen <- input$origen
      
      lang_declared <- lang()
      dates_lang <- switch(
        lang_declared,
        'cat' = 'ca',
        'spa' = 'es',
        'eng' = 'en'
      )
      
      switch (origen,
              "S" = fire_variables <- c("LFMC","DFMC","SFP"),
              fire_variables <- c("LFMC","DFMC","SFP","CFP")
      )
      
      
      drought_vars <- c("REW","DDS") %>%
        magrittr::set_names(translate_app(., lang_declared))
      climate_vars <- c("PET", "Precipitation") %>%
        magrittr::set_names(translate_app(., lang_declared))
      fire_vars <- fire_variables %>%
        magrittr::set_names(translate_app(., lang_declared))
      quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
        magrittr::set_names(translate_app(., lang_declared))
        
      

      shiny::updateSelectInput(
        session,
        'variable',
        choices = shiny_set_names(list(
          'drought variables' = drought_vars,
          'climate variables' = climate_vars,
          'fire variables' = fire_vars,
          'Percentiles variables' = quantiles_vars
        ), lang_declared) #,
        #selected = tail(x, 1)
      )

      
  })
  
 
  
  
  
  
  
  # %%%%%%%%%%%%%%%%%%%%  DEVOLVER REACTIVOS  %%%%%%%%%%%%%%%%%%%%
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #      .) Creamos DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS
  
  # ...... DATA REACTIVE .........
  # ..............................
  
  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $
  
  
  data_reactives <- shiny::reactiveValues()
  
  # ...... DATA OBSERVE ..........
  # ..............................
  
  #      .) Creamos dentro de DATA_REACTIVE
  #      .) Todos los diferentes apartados con $
  
  shiny::observe({  
    
    data_reactives$fecha_reactive  <- input$fecha             
    data_reactives$variable_reactive <- input$variable
    data_reactives$origen_reactive <- input$origen             
    data_reactives$legend_check <- input$legend_check                    
    data_reactives$legend_modify_reactive <- input$legend_modify             
    
    
  })
  
  # ......... RETURN .............
  # ..............................
  
  #      .) Quiero tener constantemente 4 valores CONTROLADOS
  #      .) FECHA / VARIABLE / ORIGEN / LEGEND MODIFY
  #      .) Cuando VARIAN
  #      .) Las RETORNO para que otros MÓDULOS los aprovechen (el MAPS de LEAFLET)


  return(data_reactives)
}
