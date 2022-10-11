#' @title mod_helpInput and siteDrought_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_helpInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_help_container')
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
help_data <- function(
  input, output, session,
  siteDroughtdb, lang, main_data_reactives
   
) {

  # renderUI ####
  output$mod_help_container <- shiny::renderUI({
    
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
    fire_vars <- c("LFMC","DFMC","SFP","CFP") %>%
      magrittr::set_names(translate_app(., lang_declared))
    quantiles_vars <- c("REW_q","DDS_q","LFMC_q") %>%
      magrittr::set_names(translate_app(., lang_declared))
    
   
     # ********************************************
    # ----------     ETIQUETAS HTML5    ----------
    # ********************************************
    
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
        

      
      shiny::selectInput(
        ns('variable_help'), translate_app('var_daily_label', lang_declared),
        choices = shiny_set_names(list(
          'drought variables' = drought_vars,
          'climate variables' = climate_vars,
          'fire variables' = fire_vars,
          'quantiles variables' = quantiles_vars
        ), lang_declared)
      ),
      
      # ...... DESCRIPCIÓN CORTA VARIABLE .......
      # .........................................
      
      #      .) Creamos un UIOUTPUT
      #      .) Queremos una descripción que varié
      #      .) Cuando seleccionamos una VARIABLE

      
      shiny::uiOutput(
        ns('help_variables')
      ),
      

    ) # end of tagList

  })           

  


  # ............ OBSERVE EVENT ................
  # ...........................................
  
  #      .) Es un OBSERVER de un SOLO REACTIVO
  #      .) En este caso dela VARIABLE SELECCIONADA
  
  #      .) En seleccionar una VARIABLE (Combo)
  #      .) Hay un OUTPUT RENDER UI
  #      .) Que mostrará una Definición corta de VARIABLE 
  #      .) La definición SE CREA donde el OUTPUT define ( $selectInput_vars )
  
  
  shiny::observeEvent(
    eventExpr = input$variable_help,
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
      
      
      # ..... DESCRIPCIÓN CORTA VARIABLE .....
      # ......................................
      
      #       .) Definimos VARIABLE seleccionada (Combo)
      #       .) Usamos la f(x) TRANSLATE APP para crear-la
      
      
      # ........ VERISON FINAL ......
      # .............................
      
      # var_selected <- input$variable_help
      # var_selected_help <- paste0('help_',var_selected)
      # 
      # translate_app(var_selected_help,lang_declared)
      
      
      # ....... VERISON PRUEBA ......
      # .............................
      
      #       .) Versión hasta que no tenga las definiciones finals
      
      
      var_selected <- input$variable_help
      var_selected_help <- paste0('help_',var_selected)
      
      output$help_variables <- shiny::renderUI({
        
        part <- "...bla,bla,bla..."
        res <- translate_app(var_selected_help,lang_declared)
        shiny::HTML(paste(part,res,part,sep="<br/>"))
        
      })
      
  })
  

}
