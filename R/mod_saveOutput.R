#' @title mod_saveOutput and modosin_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_saveOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_save_container')
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
mod_save <- function(
  input, output, session,
  main_data_reactives, data_reactives, lang
   
) {

  # renderUI ####
  output$mod_save_container <- shiny::renderUI({
    
    
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
    
    
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # -------------------------     ESTRUCTURA HTML5   ------------------------------
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    

    
    #       .) TAGLIST rea una definición de etiqueta HTML
    #       .) Creamos los elementos HTML5 con TAGS
    #       .) DROPDOWNS (SelectIntpu),...
     
    shiny::tagList(
      shiny::div(
        
        shiny::fluidRow(translate_app("save_main_label", lang_declared), style = "text-align: center;"),  
        br(),
          
        shiny::fluidRow(
            
            shiny::column(6,
                          
                   shiny::downloadButton(ns("map_save"), translate_app("save_map_button", lang_declared))
                   ),
            
            shiny::column(6,
                          
                    # ......... ACTION BUTTON ...........
                    # ...................................
                          
                   shiny::downloadButton(ns("table_save"), translate_app("save_table_button",lang_declared)),
                   br(),
                   br(),
                   # shiny::p(,, style = "text-align: center;")
                   
                   # ......... RADIO BUTTONS ...........
                   # ...................................
                   
                  
                   shiny:: radioButtons(
                     ns("data_columns"),translate_app("data_colum_label", lang_declared),
                     shiny_set_names(c("col_vis" = "col_vis", 
                                       "col_all" = "col_all"),lang_declared)
                   ),
                   
                   
                   # shiny::p(, style = "text-align: center;"),
                   # ......... RADIO BUTTONS ...........
                   # ...................................
                   
                   
                   shiny:: radioButtons(
                     ns("data_format"),translate_app("select_format_label", lang_declared),
                     shiny_set_names(c("csv" = "csv", 
                                       "xlsx" = "xlsx"),lang_declared)
                   )
                   
                  )
  
        ) # end fluid_Row
      ) # end div
    ) # end of tagList

  })  
  
  

  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # --------------------------  DOWNLOAD REACTIVE   ------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

  #      .) Es un OBSERVER de un SOLO REACTIVO - ESPECIAL
  #      .) se divide en 2 PARTES:
  #               .) REACTIVE VALUE
  #               .) DonlLoadHandler
  
  #      .) DOWNLOADHANDLER
  #      .) NECEISITA
  #               .) FILENAME Function  => nombre de archivo
  #               .) CONTENT  Function  => f(x) WRITE
  
 
  # .......... REACTIVE ...............
  # ...................................
  
  datasetInput <- reactive({

    
    fecha <- data_reactives$fecha_reactive
    sf <- main_data_reactives$data_day  

    variable <- data_reactives$variable_reactive
    num_i <- as.numeric(match(variable,names(sf)))
    selected_var <- as.symbol(names(sf)[num_i])
    
    
    if( input$data_columns == "col_all" ) {
      sf  %>%
        dplyr::filter(date == fecha) %>%
        dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
                      lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
                      lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
                      lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
        ) %>% data.frame() %>% dplyr::select(-geometry)  
      
    } else {
      sf %>%
        dplyr::filter(date == fecha) %>%
        dplyr::select(plot_id, selected_var, date, plot_origin) %>%
        dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
                      lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
                      lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
                      lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
        ) %>% data.frame() %>% dplyr::select(-geometry)
    }
    
  })
  
  
  # ........ DOWNLOADHANDLER ..........
  # ...................................
  
  
  
  # ....... FORMATO SELECTED ..........
  # ...................................
  
  #      .) XXXXXXXXXXXXX
  
  format_selected <- function() {
    switch (input$data_format,
            "csv" = format <- ".csv", 
            "xlsx" = format <- ".xlsx" 
    )
  }
  
  # ....... COLUMN SELECTED ...........
  # ...................................
  
  #      .) XXXXXXXXXXXXX
  
  column_selected <- function() {
    switch (input$data_columns,
            "col_vis" = format <- "_specific_columns", 
            "col_all" = format <- "_all_columns" 
    )
  }
  
  # ......... DATE STRING .............
  # ...................................
  
  #      .) XXXXXXXXXXXXX
  
  date_str <- toString(Sys.Date()) %>% 
    stringr::str_split(.,"-") %>% .[[1]] %>%
    stringr::str_c(., collapse = "")
  
  

  output$table_save <- downloadHandler(
    filename = function() {
      paste(date_str,column_selected(),format_selected(), sep = "")
    },
    content = function(file) {
      
      # ....... WRITE TYPE ...........
      # .............................
      
      #      .) XXXXXXXXXXXXX
   
      switch (input$data_format,
              "csv" = write.csv(datasetInput(), file, row.names = FALSE),
              "xlsx" = writexl::write_xlsx(datasetInput(), file) 
              )
     

    }
  )

   
  
 
  

}
