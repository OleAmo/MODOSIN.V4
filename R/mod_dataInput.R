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
    

    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ------       FECHAS DATE INPUT     ---------
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #       .) OBJETIVO
    #       .) Quiero LIMITAR el CALENDARIO (DateInput)
    
    #       .) NECESITO
    #       .) Date Max / Date Min
    #       .) En función de la TABLA (Data_day) de la BBDD
    
    #       .) PROBLEMA
    #       .) Mientras se CARGA la TABLA dela BBDD
    #       .) Necesitamos UNA DATA para que el CALENDARIO no se BLOQUE
    
    #       .) SOLUCION
    #       .) Cuando DATA_DAY es NULL (se està cargando la TABLA)
    #       .) Asignaré al CALENDARIO una MAX DATE y MIN DATE respecto la data de HOY
    #       .) Después usarà el MAX y MIN en f(x) de la TABLA
    
    data_day <- main_data_reactives$data_day


    if( is.null((data_day))){
      
      date_max <- Sys.Date()+1
      date_min <- Sys.Date()-1

    } else {
      
      # data_day <- main_data_reactives$data_day

      date_max <- max(data_day$date)
      date_min <- min(data_day$date)

     
    }

    
    dif_days <- as.numeric(difftime(date_max, date_min, units = "days"))
    date_midel <- as.Date(date_min) + round(dif_days/2, digits = 0)
    
    
    
    
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ----------     ETIQUETAS HTML5    ----------
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #       .) TAGLIST rea una definición de etiqueta HTML
    #       .) Creamos los elementos HTML5 con TAGS
    #       .) DROPDOWNS (SelectIntpu),...
     
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
      
      # ..... RADIO BUT LEGEND COLOR ......
      # ...................................
      
      #      .) Dejo COMENTADA el CANVIO de COLOR de LEYENDA
      #      .) Me espero a hablar-lo con Miquel y Víctor
      
      # shiny:: radioButtons(
      #   ns("legend_modify"),translate_app("type_legend_label", lang_declared),
      #   shiny_set_names(c("estandard_label" = "estandard",
      #                     "1st_label" = "tip_1",
      #                     "2nd_label" = "tip_2"),lang_declared)
      # )
      
      
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


  
  # %%%%%%%%%%%%%%%%%%   OBSERVE SELECT INPUT  %%%%%%%%%%%%%%%%%%%
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #      .) Es un OBSERVER de => LENGUA / ORIGEN 
  #      .) Crea un SELECTINPUT en el OUTPUT RENDER UI 
  
  #      .) Cada vez que variamos LENGUA / ORIGEN
  #      .) SE CREA un nuevo SELECT INPUT
  #      .) Donde el OUTPUT indica ( $selectInput_vars )
  
 
  shiny::observe({   
    
    # ........ VALUES REACTIVES .........
    # ...................................
    
    #       .) Valores REACTIVES
    #              .) LANGUAGE
    #              .) ORIGEN

    
    shiny::validate(shiny::need(input$origen, 'origen no selected') )
    
      lang_reactive   <- shiny::reactive({ lang_declared <- lang()})
      origen_reactive <- shiny::reactive({ input$origen })
      
      
      # ......... INICIALIZAR .............
      # ...................................
      
      #       .) NS = ID's únicos
      #       .) LENGUA = Reactive
      #       .) ORIGEN = Reactive
      
      ns <- session$ns
      
      lang_declared <- lang_reactive()
      origen <-  origen_reactive()

      
      
      # ..... MATOLLAR / INCIENDIOS .......
      # ...................................
      
      #       .) En el caso MATOLLAR
      #       .) NO HAY POTENCIAL FUEGO COPA (Ya que son tienen suficiente altura)
      #       .) Por lo tanto:
      #       .) Cuando se seleccione MATOLLAR ("S")
      #       .) No aparecerá CFP
    
      
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
      
      
      # ............. OUTPUT ..............
      # ...................................
      
      #       .) Indicamos DONDE se harà el cambio  ($selectInput_vars)
      #       .) Indicamos QUE HAREMOS (crear SelectInput)

      output$selectInput_vars <- shiny::renderUI({
        
        shiny::selectInput(
          ns('variable'), translate_app('var_daily_label', lang_declared),
          choices = shiny_set_names(list(
            'drought variables' = drought_vars,
            'climate variables' = climate_vars,
            'fire variables' = fire_vars,
            'Percentiles variables' = quantiles_vars
          ), lang_declared)
        )
        
        
      })
      
      
      
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
    # data_reactives$legend_modify_reactive <- input$legend_modify
    
  })
  
  # ......... RETURN .............
  # ..............................
  
  #      .) Quiero tener constantemente 4 valores CONTROLADOS
  #      .) FECHA / VARIABLE / ORIGEN / LEGEND MODIFY
  #      .) Cuando VARIAN
  #      .) Las RETORNO para que otros MÓDULOS los aprovechen (el MAPS de LEAFLET)


  return(data_reactives)
}
