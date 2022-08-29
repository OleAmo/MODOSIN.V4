#' @title modosin_dataInput  and modosin_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
modosin_dataInput_polygon <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container_polygon')
    )
  )
}

#' modosin_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param modosindb modosin db
#' @param lang lang reactive
#'
#' @export
modosin_data_polygon <- function(
  input, output, session,
  modosindb, lang
) {

  output$mod_data_container_polygon <- shiny::renderUI({
    
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

    shiny::tagList(
      
      # ....... SELECCION CAPAS ...........
      # ...................................
      
      shiny::selectInput(
        ns('layers_select'), translate_app('layers_label', lang_declared),
        choices = shiny_set_names(list(
          "sub_layer_label" = shiny_set_names(list(
              'Provincia_select_entorno' = "provincia" ,
              'Comararca_select_entorno' = "comarca",  
              'Embass_select_entorno' = "embass", 
              'No_Polygon_label' = "no_polygon"), lang_declared), 
          
          "sub_layer_label_2" = shiny_set_names(list(
              'Nucleos_select_entorno' = "nucleos"), lang_declared)
        ), lang_declared)
   
      ),
      
      # ....... ENTORNO HIDDEN ...........
      # ..................................
      
      
      shinyjs::hidden(
        shiny::div(
          id = ns('file_upload_panel'),
          shiny::selectInput(
            ns('nucleos_area'), translate_app('entorno_hidden', lang_declared),
            choices = list(
              "< 10 Ha" = "0-10",
              "10 - 25 Ha" = "10-25",
              "25 - 50 Ha" = "25-50",
              "50 - 100 Ha" = "50-100",
              "100 - 250 Ha" = "100-250",
              " > 1000 Ha" = "1000-99999"
            )
          )
        )
      )

 

    ) # end of tagList

  })
  
  
  # ..................... OBSERVER HIDDENS  ......................
  # ..............................................................
  
  #      .) 
  
 
  shiny::observe({

    shiny::validate(
      shiny::need(input$layers_select, 'no type')
    )
    display_entorno <- input$layers_select

    if (display_entorno == 'nucleos') {
      shinyjs::show('file_upload_panel')
    } else {
      shinyjs::hide('file_upload_panel')
    }
  })


  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................

  #      .) Creamos DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS

  # ...... DATA REACTIVE .........
  # ..............................

  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $


  data_reactives_polygon <- shiny::reactiveValues()

  # ...... DATA OBSERVE ..........
  # ..............................

  #      .) Creamos dentro de DATA_REACTIVE
  #      .) Todos los diferentes apartados con $

  shiny::observe({

    data_reactives_polygon$layers_select <- input$layers_select
    data_reactives_polygon$nucleos_area  <- input$nucleos_area

  })

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente los valores REACTIVOS
  #                   .) LAYER SELECT = Primer COMBO (Provincias, Comarcas,...)
  #                   .) NUCLEOS AREA = Segundo COMBO (diferentes tipos de área ) 



  return(data_reactives_polygon)
}
