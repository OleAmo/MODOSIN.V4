#' @title modosin_dataInput  and modosin_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
modosin_dataInput <- function(id) {
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

#' modosin_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @export
modosin_data <- function(
  input, output, session,
  modosindb, lang
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


    # ....... FECHAS SELECT INPUT ........
    # ...................................
    
    #       .) Calculo MAX / MIN fecha
    #       .) En función de las dif pruebas de DDBB
    
      
    # DATA DAY = petita_2
    # date_max <- '2022-02-06'
    # date_min <- '2022-01-01'
    
    # DATA DAY = petita_3
    date_max <- '2022-07-06'
    date_min <- '2022-06-01'
    
    
    # DATA DAY = datay_day_fire
    # date_max <- '2022-09-18'
    # date_min <- '2021-09-19'
    
    dif_days <- as.numeric(difftime(date_max, date_min, units = "days"))
    
    date_midel <- as.Date(date_min) + round(dif_days/2, digits = 0)
    
     
    shiny::tagList(
        
      
        # ...... SELECCION VARIABLE CAMBIENTE .....
        # .........................................
        
        #      .) Creamos un UIOUTPUT
        #      .) Queremos un SELECTINPUT que varie en f(x) de ORIGEN
        #      .) Si el ORIGEN es = MATOLLAR
        #      .) El select INPUT varia
        
        
        shiny::uiOutput(
          ns('selectInput_vars')
        ),
        

      # ........ SELECCION FECHA ..........
      # ...................................
      
      #       .) PROBLEMA
      #       .) Aveces el desplegable de DATE queda devajo del NAV
      #       .) Para solucionar-lo
      #           .) https://developer.mozilla.org/es/docs/Web/CSS/z-index
      #           .) Uso los Z-INDEX del CSS
      #           .) El Z-INDEX indica PRIORIDA de aparecer ENCIMA
      #           .) Como MAYOR el Z-INDEX mas encima de todo
      
      shiny::tags$style(type = "text/css", ".datepicker { z-index: 99999 !important; }"),
     
        
      shiny::dateInput(
        ns("fecha"), translate_app('date_daily_label', lang_declared),
        value = date_midel,
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
      
      # ......... RADIO BUTTONS ...........
      # ...................................
      
      
      shiny:: radioButtons(
        ns("legend_modify"),translate_app("type_legend_label", lang_declared),
        shiny_set_names(c("estandard_label" = "estandard", 
                          "1st_label" = "tip_1", 
                          "2nd_label" = "tip_2"),lang_declared)
      )
      

    ) # end of tagList

  })           

  

 
  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................

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
    data_reactives$legend_modify_reactive <- input$legend_modify
    
    

  })
  
  
  # ............ OBSERVE EVENT ................
  # ...........................................
  
  #      .) Es un OBSERVER de un SOLO REACTIVO
  #      .) En este caso el ORIGEN
  
  #      .) En variar ORIGEN (Combo)
  #      .) Hay un SWITCH que definie FIRE_VARS
  #      .) Y un OUTPUT RENDER UI
  #      .) Defeine un NUEVO SelectInput
  #      .) El SelectInput SE CREA donde el OUTPUT define ( $selectInput_vars )
  
  
  shiny::observeEvent(
    eventExpr = input$origen,
    handlerExpr = {
      
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
      
      # ....... VARIABLES MATOLLAR ........
      # ...................................
      
      #       .) En el caso MATOLLAR
      #       .) NO HAY POTENCIAL FUEGO COPA (Ya que son tienen suficiente altura)
      #       .) Por lo tanto:
      #       .) Cuando se seleccione MATOLLAR ("S")
      #       .) No aparecerá CFP
      
      origen <-  input$origen
      
      switch (origen,
              "S" = fire_variables <- c("LFMC","DFMC","SFP"),
              fire_variables <- c("LFMC","DFMC","SFP","CFP")
      )
      
      # ...... VARIABLE SELECTINPUT .......
      # ...................................
      
      #       .) Variables según MIQUEL
      #           .) sequía:              REW, DDS
      #           .) variable climáticas: PET, Precipitation
      #           .) variables incendio:  "LFMC","DFMC","SFP","CFP"
      #           .) quantiles : 
      
      drought_vars <- c("REW","DDS") %>%
        magrittr::set_names(translate_app(., lang_declared))
      climate_vars <- c("PET", "Precipitation") %>%
        magrittr::set_names(translate_app(., lang_declared))
      fire_vars <- fire_variables %>%
        magrittr::set_names(translate_app(., lang_declared))
      quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
        magrittr::set_names(translate_app(., lang_declared))
      

      output$selectInput_vars <- shiny::renderUI({
        
        shiny::selectInput(
          ns('variable'), translate_app('var_daily_label', lang_declared),
          choices = shiny_set_names(list(
            'drought variables' = drought_vars,
            'climate variables' = climate_vars,
            'fire variables' = fire_vars,
            'quantiles variables' = quantiles_vars
          ), lang_declared)
        )
        
        
      })
      
      
      
    })
  

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente 4 valores CONTROLADOS
  #      .) FECHA / VARIABLE / ORIGEN / LEGEND MODIFY
  #      .) Cuando VARIAN
  #      .) Las RETORNO para que otros MÓDULOS los aprovechen (el MAPS de LEAFLET)




  return(data_reactives)
}
