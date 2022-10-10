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
      
        
        shiny::fluidRow(translate_app("save_main_label", lang_declared), style = "text-align: center;"),  
        shiny::br(),
        shiny::fluidRow(
          
  
          # -----------------     ROW BUTTONS    ----------------
          # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          
          #       .) Fila de BUTTONS
          #       .) MAP_SAVE / TABLE_SAVE
          
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, align = 'center',
                          
                # .......... BUTTON MAP .............
                # ...................................
                
                #       .) Button que DESCARGA GEOPACK 
                            
                shiny::downloadButton(ns("map_save"), translate_app("save_map_button", lang_declared))
            ),
            
            shiny::column(6, align = 'center',
                          
                # ......... TABLE BUTTON ............
                # ...................................
                
                #       .) Button que DESCARGA TABLA
                #       .) Puede ser en formato CSV/XLSX
                #       .) Puede tener TODAS COLUMNAS VARIABLES / UN COLUMNA VARIABLE              
                            
                shiny::downloadButton(ns("table_save"), translate_app("save_table_button",lang_declared))
            )
          ),
          
         
          # ------------------    ROW COLUMNS    ----------------
          # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          
          #       .) Fila de COLUMNAS
          #       .) DATA_COLUMN / FORMAT COLUMNS
          
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, offset = 6, align = 'center',
              
              # ......... RADIO BUTTONS ...........
              # ...................................
              
              #       .) Botones selección COLUMNAS
              #       .) Dos tipos = TODAS COLUMNAS VARIABLE / UNA COLUMNA VARIABLE
              
              
              shinyWidgets::prettyRadioButtons(
                ns("data_columns"),translate_app("data_colum_label", lang_declared),
                shiny_set_names(c("col_vis" = "col_vis",
                                  "col_all" = "col_all"),lang_declared),
                status = 'success', fill = TRUE, shape = 'round'
                
              ),
              
              # ......... RADIO BUTTONS ...........
              # ...................................
              
              #       .) Botones selección FORMATO
              #       .) Dos tipos = CSV / XLSX
              
              shinyWidgets::prettyRadioButtons(
                ns("data_format"),translate_app("select_format_label", lang_declared),
                shiny_set_names(c("csv"  = "csv",
                                  "xlsx" = "xlsx"),lang_declared),
                status = 'success', fill = TRUE, shape = 'round'
              )
              
              
            ) # end colum
          )# end fluid row
        )# end fluid row  
    ) # end of tagList

  })  
  
  

  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ---------------------    ESTRUCTURA BOTON DOWNLOAD  --------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

  #      .) Se compone de 2 ESTRUCTURAS
  #               .) REACTIVE
  #               .) DOWNLOADHANDLER
  

  # ****************  REACTIVE TABLE  ******************
  # ****************************************************
  
  #      .) REACTIVE
  #      .) En f(x) del tipo de DATA_COLUMNS
  #      .) Nos devuelve un DF 
  #               .) con TODAS COLUMNAS
  #               .) con SOLO COLUMNA VARIABLE SELECCIONADA
  #      .) CREAMOS
  #               .) lat/long (ETRS89)
  #               .) lat/long (WGS84)
  #      .) ELIMINAMOS
  #               .) Columna GEOMETRY
  #               .) Ya que en el EXCEL y CSV no aporta info

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
      
    } else if (input$data_columns == "col_vis") {
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
  
  # *****************  REACTIVE MAP  *******************
  # ****************************************************
  
  #      .) REACTIVE
  #      .) SIEMPRE devuelve el mismo DF
  #      .) INCLUYE Gemoetry (ya que es un GEOPACK)
  
  mapInput <- reactive({
    
    fecha <- data_reactives$fecha_reactive
    sf <- main_data_reactives$data_day  
    
    variable <- data_reactives$variable_reactive
    num_i <- as.numeric(match(variable,names(sf)))
    selected_var <- as.symbol(names(sf)[num_i])
    
    sf  %>%
      dplyr::filter(date == fecha) %>%
      dplyr::mutate(lon_WGS84 = sf::st_coordinates(.data$geometry)[,1],
                    lat_WGS84 = sf::st_coordinates(.data$geometry)[,2],
                    lon_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,1],
                    lat_ETRS89 = sf::st_coordinates(sf::st_transform(.data$geometry, 25831))[,2]
      ) %>% data.frame()
    
  })
  
  
  # ****************  DOWNLOADHANDLER  ******************
  # ****************************************************
  
  #      .) NECEISITA
  #           .) FILENAME Function  => nombre de archivo
  #           .) CONTENT  Function  => f(x) WRITE
  
  
  # ................ VARIABLES ...................
  # ..............................................
  
  #      .) Variables Necesarias para el SAVE TABLE
  #          .) FORMATO
  #          .) COLUMN_SELECTED
  #          .) DATE_STRING
  
  # ----- FORMATO SELECTED -----
  # ----------------------------
  
  #      .) Función que asigna EXENCIÓN del archivo
  #      .) CSV o XLSX
  
  format_selected <- function() {
    switch (input$data_format,
            "csv" = format <- ".csv", 
            "xlsx" = format <- ".xlsx" 
    )
  }
  
  # ----- COLUMN SELECTED ------
  # ----------------------------
  
  #      .) Función que asigna parte del nombre
  #      .) Del archivo a descargar
  
  column_selected <- function() {
    switch (input$data_columns,
            "col_vis" = format <- "_specific_columns", 
            "col_all" = format <- "_all_columns" 
    )
  }
  
  # ----- DATE STRING ----------
  # ----------------------------
  
  #      .) Función que asigna crea STRING DATE
  #      .) Une Year/month/day en un solo STRING sin guiones
  
  date_str <- toString(Sys.Date()) %>% 
    stringr::str_split(.,"-") %>% .[[1]] %>%
    stringr::str_c(., collapse = "")
  
  
 
  # .............. DOWNLOADHANDLER ...............
  # ..............................................
  
  #      .) Es un OUTPUT
  #      .) En este caso irá al ID = TABLE_SAVE
  #      .) ESTRUCUTRA:
  #               .) FILENAME Function  => nombre de archivo
  #               .) CONTENT  Function  => f(x) WRITE
  
  
  # ******** TABLE SAVE ********
  # ****************************
  
  #      .) DOWNLOADHANDLER
  #      .) Para crear f(x) TABLE DWONLOAD

 
   output$table_save <- downloadHandler(
    filename = function() {
      paste(date_str,column_selected(),format_selected(), sep = "")
    },
    content = function(file) {
      
      # ....... WRITE TYPE ...........
      # .............................
      
      #      .) en f(x) del formato
      #      .) usaremos WRITE.CSV o WRITE_XLSX para descargar
   
      switch (input$data_format,
              "csv" = write.csv(datasetInput(), file, row.names = FALSE),
              "xlsx" = writexl::write_xlsx(datasetInput(), file) 
              )
     

    }
  )
   
   
   
   # ******** TABLE SAVE ********
   # ****************************
   
   #      .) DOWNLOADHANDLER
   #      .) Para crear f(x) MAP DWONLOAD

   output$map_save <- downloadHandler(
     filename = function() {
       paste(date_str,'_map.gpkg', sep = "")
     },
     content = function(file) {
       
       # ....... WRITE GEOPACK ...........
       # .............................
       
       #      .) Usamos f(x) ST_WRITE
       #      .) La extensión en FILENAME es .GPKG
       
       sf::st_write(mapInput(), file)
       
       
     }
   )
  
 
  

}
