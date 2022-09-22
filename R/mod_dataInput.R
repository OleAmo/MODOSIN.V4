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

    # ....... SELECCION VARIABLE ........
    # ...................................
    
    #       .) Variables según MIQUEL
    #           .) soil moisture: Theta, Psi, REW
    #           .) soil moisture: Theta, Psi, REW
    #           .) climate: PET
    #           .) evaporative surface: LAI
    #           .) water balance: Infiltration, RunOff, DeepDrainage, Esoil, Eplant
    #           .) drought stress: DDS
    
    
    
    
    # if ( !is.null(input$origen) ) {
    #    origen <- input$origen
    #   switch (origen,
    #           "S" = variables_lista <- c("LFMC","DFMC","SFP"),
    #           variables_lista <- c("LFMC","DFMC","SFP","CFP")
    #   )
    # } else {
    #   variables_lista <- c("LFMC","DFMC","SFP","CFP")
    # }
    

    
    drought_vars <- c("REW","DDS") %>%
      magrittr::set_names(translate_app(., lang_declared))
    climate_vars <- c("PET", "Precipitation") %>%
      magrittr::set_names(translate_app(., lang_declared))
    fire_vars <- c("LFMC","DFMC","SFP","CFP") %>%
      magrittr::set_names(translate_app(., lang_declared))

    
    
    
    quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
        magrittr::set_names(translate_app(., lang_declared))
      
      
      
    shiny::tagList(
        
        # ....... SELECCION VARIABLE ........
        # ...................................
        
        shiny::selectInput(
          ns('variable'), translate_app('var_daily_label', lang_declared),
          choices = shiny_set_names(list(
            'drought variables' = drought_vars,
            'climate variables' = climate_vars,
            'fire variables' = fire_vars,
            'quantiles variables' = quantiles_vars
          ), lang_declared)
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
        value = "2022-1-15",
        format = "yyyy/mm/dd",
        
        # DATA DAY = petita_2
        # max = '2022-02-06',
        # min = '2022-01-01'
        
        # DATA DAY = datay_day_fire
         min = '2021-09-02', # Sys.Date() -364
         max = '2022-09-01' # Sys.Date() -1

       
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

  
  # shiny::observeEvent(
  #   eventExpr = input$origen,
  #   handlerExpr = {
  #     if(input$origen == "S"){
  #       list_variables <- c("LFMC","DFMC","SFP")
  #     } else {
  #       list_variables <- c("LFMC","DFMC","SFP","CFP")
  #     }
  #     
  #     print(list_variables)
  #     
  #   },
  #   priority = 1000
  # )
 
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
  
  

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente 2 valores ACTIVOS
  #      .) FECHA / VARIABLE
  #      .) Son los que me darán la TABLA y la VARIABLE a VISUALIZAR




  return(data_reactives)
}
