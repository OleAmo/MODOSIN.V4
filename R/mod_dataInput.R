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
    #               .) soil moisture: Theta, Psi, REW
    #               .) soil moisture: Theta, Psi, REW
    #               .) climate: PET
    #               .) evaporative surface: LAI
    #               .) water balance: Infiltration, RunOff, DeepDrainage, Esoil, Eplant
    #               .) drought stress: DDS
    


    soil_moisture_vars <- c("Theta", "Psi", "REW") %>%
      magrittr::set_names(translate_app(., lang_declared))
    climate_vars <- c("PET", "Precipitation") %>%
      magrittr::set_names(translate_app(., lang_declared))
    evap_surface_vars <- c('LAI') %>%
      magrittr::set_names(translate_app(., lang_declared))
    fwb_vars <- c("Interception", "Infiltration", 'Runoff', 'DeepDrainage', 'Esoil', 'Eplant') %>%
      magrittr::set_names(translate_app(., lang_declared))
    drought_stress_vars <- c("DDS", "LFMC") %>%
      magrittr::set_names(translate_app(., lang_declared))

    shiny::tagList(
      
      shiny::selectInput(
        ns('variable'), translate_app('var_daily_label', lang_declared),
        choices = shiny_set_names(list(
          'Soil moisture' = soil_moisture_vars,
          'Climate' = climate_vars,
          'Evaporative surface' = evap_surface_vars,
          'Water balance' = fwb_vars,
          'Drought stress' = drought_stress_vars), lang_declared)
      ),

      # ........ SELECCION FECHA ..........
      # ...................................

      shiny::dateInput(
        ns("fecha"), translate_app('date_daily_label', lang_declared),
        value = "2021-10-23",
        format = "yyyy/mm/dd",
        max = "2022-08-22",
        min =  "2021-08-23"
      ),
      
      # ......... ACCION BUTTON ...........
      # ...................................
      
      actionButton(ns("boto"), "Proyectar"),
      actionButton(ns("boto_save"), "Guardar"),

     
      

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
    data_reactives$variable_reactive<- input$variable
    data_reactives$boto_reactive <- input$boto
    data_reactives$boto_save_reactive <- input$boto_save
     
    

  })

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente 2 valores ACTIVOS
  #      .) FECHA / VARIABLE
  #      .) Son los que me darán la TABLA y la VARIABLE a VISUALIZAR




  return(data_reactives)
}
