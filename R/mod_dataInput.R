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
    


      climate_vars <- c("PET", "Precipitation") %>%
        magrittr::set_names(translate_app(., lang_declared))
      soil_moisture_vars <- c("REW","REW_q") %>%
        magrittr::set_names(translate_app(., lang_declared))
      drought_stress_vars <- c("DDS","DDS_q") %>%
        magrittr::set_names(translate_app(., lang_declared))
      fire_vars <- c("LFMC","LFMC_q","DFMC","SFP","CFP") %>%
        magrittr::set_names(translate_app(., lang_declared))
      
      shiny::tagList(
        
        # ....... SELECCION VARIABLE ........
        # ...................................
        
      shiny::selectInput(
        ns('variable'), translate_app('var_daily_label', lang_declared),
        choices = shiny_set_names(list(
          'Climate' = climate_vars,
          'Soil moisture' = soil_moisture_vars,
          'Drought stress' = drought_stress_vars,
          'fire variables' = fire_vars), lang_declared)
        ),

      # ........ SELECCION FECHA ..........
      # ...................................

      shiny::dateInput(
        ns("fecha"), translate_app('date_daily_label', lang_declared),
        value = "2022-1-15",
        format = "yyyy/mm/dd",
        # max = '2022-03-25',
        # min = '2022-01-01'
        max = '2022-08-22',
        min = '2021-08-23'
      ),
      
      
      # ...... SELECCION ORIGEN PLOT ......
      # ...................................
      
      shiny::selectInput(
        ns('origen'), translate_app('plot_origin_label', lang_declared),
        shiny_set_names(c(
          "T"="T",
          "P"="P", 
          "A"="A",
          "O"="O",
          "S"="S"), lang_declared)
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

     
    

  })

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente 2 valores ACTIVOS
  #      .) FECHA / VARIABLE
  #      .) Son los que me darán la TABLA y la VARIABLE a VISUALIZAR




  return(data_reactives)
}
