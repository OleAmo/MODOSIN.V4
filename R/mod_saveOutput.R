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
    
    # ********************************************
    # ----------     ETIQUETAS HTML5    ----------
    # ********************************************
    
    #       .) TAGLIST rea una definición de etiqueta HTML
    #       .) Creamos los elementos HTML5 con TAGS
    #       .) DROPDOWNS (SelectIntpu),...
     
    shiny::tagList(
      shiny::div(
        
        shiny::fluidRow(translate_app("save_main_label", lang_declared), style = "text-align: center;"),  
        br(),
          
        shiny::fluidRow(
            
            shiny::column(3,
                   shiny::actionButton(ns("map_save"), translate_app("save_map_button", lang_declared))
                   ),
            
            shiny::column(9,
                          
                    # ......... ACTION BUTTON ...........
                    # ...................................
                          
                   shiny::actionButton(ns("table_save"), translate_app("save_table_button",lang_declared)),
                   br(),
                   "¿Todos los datos o solo las columnas visibles?",
                   
                   # ......... RADIO BUTTONS ...........
                   # ...................................
                   
                   
                   shiny:: radioButtons(
                     ns("legend_modify"),translate_app("type_legend_label", lang_declared),
                     shiny_set_names(c("estandard_label" = "estandard", 
                                       "1st_label" = "tip_1", 
                                       "2nd_label" = "tip_2"),lang_declared)
                   )
                  )
  
        ) # end fluid_Row
      ) # end div
    ) # end of tagList

  })           

  


  
 
  

}
