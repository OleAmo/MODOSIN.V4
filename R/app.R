#' function to launch the catdrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
modosin_app <- function() {
  library(leaflet)
  source('data-raw/translations.R')
  source('data-raw/polygon_objects_creation.R')
  source('data-raw/palette_builder.R')
  
  ### DB access ################################################################
  modosindb <- lfcdata::modosin()

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    " var socket_timeout_interval;
      var n = 0;

      $(document).on('shiny:connected', function(event) {
        socket_timeout_interval = setInterval(function() {
          Shiny.onInputChange('alive_count', n++)
        }, 10000);
      });

      $(document).on('shiny:disconnected', function(event) {
        clearInterval(socket_timeout_interval)
      }); "
  )



  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'modosinApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )
  

  # ++++++++++++++++++++++++++++++++ //  UI // ++++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## UI ####

  ui <- shiny::tagList(

    # ............... INICIALIZAR ................
    # ............................................

    #       .) SHINYJS / Waiter / Hostess
    #       .) Head:
    #             .) JS files
    #             .) CSS files

    # shinyjs
    shinyjs::useShinyjs(),
    # waiter
    waiter::use_waiter(),
    waiter::use_hostess(),

    # css
  
    shiny::tags$head(
      # js script,
      shiny::tags$script(keep_alive_script),
      
      
      # # custom css
      # shiny::includeCSS(system.file('resources', 'plotdrought.css', package = 'modosinApp')),
      # # corporative image css
      # shiny::includeCSS(system.file('resources', 'corp_image.css', package = 'modosinApp')),

      # custom css
      shiny::includeCSS("inst/resources/plotdrought.css"),
      # corporative image css
      shiny::includeCSS("inst/resources/corp_image.css"),
      
      # shiny::includeCSS("inst/resources/plotdrought_v2.css"),
      
    ),
    
    

    # **************************************************************************************
    # ------------------------------  //  NAVBAR   // --------------------------------------
    # **************************************************************************************

    #       .) Titulo
    #       .) 2 Pestañas   = tabPanel
    #                 .) Explora
    #                 .) Especificación Técnica
    #       .) Dropdown Lenguas  =pickerInput
    

    navbarPageWithInputs(
      # opts
      title = 'SiteDrought App',
      id = 'nav', collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector

      # ............ Dropdown Lenguas ..............
      # ............................................

      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),
      
      
      
      # **************************************************************************************
      # ----------------------  //  Pestaña PLOTS Data_Day   // -------------------------------
      # **************************************************************************************
      
      #       .) Tiene dos parte:
      #       .) MENÚ Izq = sidebarPanel
      #       .) MAPA     = mainPanel BLUE
      
      
      # navbarPage contents
      shiny::tabPanel(
        title = mod_tab_translateOutput('main_tab_translation'),
        shiny::sidebarLayout(
          ## options
          # position = 'left', fluid = TRUE,

          # //////////////////////////////////////////////
          # ------------    MENÚ IZQUIERDA   -------------
          # //////////////////////////////////////////////

          #       .) 3 Pestañas
          #              .) DATOS
          #              .) GUARDAR
          #              .) AYUDA

          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # this is gonna be a tabsetPanel, for data selection, save and help.
            # tabset panel
            shiny::tabsetPanel(
                  id = 'sidebar_tabset', type = 'pills',

                  # ............... Pestaña PLOTS ..............
                  # ............................................
                  
                  #       .) Pestaña que visualiza PLOTS
                  #       .) En función de FECHA / VARIABLE
                  
                  
                  # data panel
                  shiny::tabPanel(
                    title = mod_tab_translateOutput('data_translation'),
                    value = 'data_inputs_panel',
                    mod_dataInput('modosin_DATA')
                  ), # end of data panel

                  # .............. Pestaña GUARDAR .............
                  # ............................................
                  
                  #       .) Pestaña para GUARDAR
                  #       .) Guardamos lo visualizado
                  #       .) En diferente formato
                  
                  # save panel
                  shiny::tabPanel(
                    title = mod_tab_translateOutput('save_translation'),
                    value = 'save_panel',
                    # mod_saveOutput('mod_saveOutput')
                  ),
                  
                  # .............. Pestaña AYUDA ...............
                  # ............................................
                  
                  #       .) Pestaña de AYUDA
                  #       .) Ofrece descripción de diferentes VARIABLES
                  #       .) Es una descripción corta
                  
                  # help panel
                  shiny::tabPanel(
                    # title = mod_tab_translateOutput('help_translation'),
                    title = mod_tab_translateOutput('help_translation'),
                    value = 'help_panel',
                    mod_helpInput('help_data')
                  )
            )
          ), # end of sidebarPanel
          
          # //////////////////////////////////////////////
          # -------------    MAPA DERECHA   --------------
          # //////////////////////////////////////////////

          #       .) 2 Pestañas
          #                 .) MAPA
          #                 .) SERIES TEMPORALS

          mainPanel = shiny::mainPanel(
            width = 8,
            shiny::div(
              id = 'overlay_div',
              shiny::tabsetPanel(
                id = 'main_panel_tabset_plots', type = 'pills',

                # ......... MAPA .........
                # ........................
                shiny::tabPanel(
                  # 'map',
                  title = mod_tab_translateOutput('map_translation'),
                  value = 'map_panel',
                  mod_mapOutput('mod_mapOutput')
                ),

                # .... SERIE TEMPORAL ....
                # ........................
                shiny::tabPanel(
                  title = mod_tab_translateOutput('series_tab_translation'),
                  value = 'series_panel',
                  mod_tsOutput('mod_tsOutput')
                )
              )
            )
          )
        )
      ),   

      # .................. Pestaña ESPECIFICACIONES TECNICAS ...................
      # ........................................................................

      #       .) Tiene UNA parte:
      #       .) Ecplicacion de la APP

      shiny::tabPanel(
        title = mod_tab_translateOutput('tech_specs_translation'),
        value = 'tech_spec_panel',
        # mod_techSpecsOutput('mod_techSpecsOutput')
        
        
      )


    ) # end of navbar
  ) # end of UI


  # ++++++++++++++++++++++++++++++ // SERVER // +++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## SERVER ####
  server <- function(input, output, session) {

    # .......... LANGUAGE REACTIVE ...............
    # ............................................

    #       .) Detecta el cambio de Lenguaje
    #       .) Lo Módulos depende este

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # ............... ACTIVAR FUNCIONES DE MODULOS .................
    # ..............................................................

    # ....... DATA INPUTS ..........
    # ..............................

    #       .) modosin_data  = f(x) a LLAMAR ( principal del Módulo )
    #       .) modosin_DATA  = Es el ID usaremos a la UI cuando llamemos la f(x)
    #       .) modosindb     = DDBB del LfcData
    #       .) Lang          = lenguaje del REACTIVE

    
    # ....... 1ra PESTAÑA ..........
    # ..............................
    
    # data_input
    data_reactives <- shiny::callModule(
      modosin_data ,'modosin_DATA', modosindb, 
      lang,  main_data_reactives
     
    )
    
    # help_input
    help_reactives <- shiny::callModule(
      help_data ,'help_data', modosindb, 
      lang,  main_data_reactives
      
    )
    
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput',
      data_reactives, main_data_reactives,
      session, lang
    )
    
    # main data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput',
      data_reactives, map_reactives,
      modosindb, lang
    )
    
   
    # ....... TIMESERIES ...........
    # ..............................
    
    timseries_reactives <- shiny::callModule(
      mod_ts, 'mod_tsOutput',
      data_reactives, main_data_reactives,
      lang
    )
    
    # # save
    # shiny::callModule(
    #   mod_save, 'mod_saveOutput',
    #   main_data_reactives, data_reactives,
    #   lang
    # )
    # # technical specifications module
    # shiny::callModule(
    #   mod_techSpecs, 'mod_techSpecsOutput',
    #   lang
    # )
    #
     
    # ..... TABS TRANSLATIONS ......
    # ..............................
    
    #       .) Uso la función => CALL_MODULE_FUNCTION
    #       .) Función creada en HELPERS.R
    
    #       .) Necesita 2 ATRIBUTOS 
    #                 .) TABS = Todas la etiquetas a traducir
    #                 .) LANG = Lengua seleccionada
    
    
    # TODAS las etiquetas a TRADUCIR
    tabs <- c('main_tab_translation','data_translation',
              'map_translation','series_tab_translation','save_translation',
              'save_translation','tech_specs_translation','help_translation')
    
    # Funcion que llama a TODOS los CALL MODULES
    callModule_function(tabs,lang)
    
  

  } # end of server function

  # Run the application
  modosinApp <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(modosinApp)

}
