# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)


navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' translate app function
#'
#' translate the app based on the lang selected


# ............. FUNCION TRANSLATE ...........
# ...........................................

#       .) Función que traducirá
#       .) Usará el DICCIONARIO ya creado (language_dictionary)
#       .) ARGUMENTOS.
#                .) LANG = Lengua per definida en menu del NAV
#                .) ID = código que usarà el DICCIONARIO para saber QUE TRADUCIR

translate_app <- function(id, lang) {
  
  id %>%
    purrr::map_chr(
      ~ language_dictionary %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            
            # ........ NO PROBLEM ENCODING .......
            # ....................................
            
            #    .) dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
            
            # ........ SI PROBLEM ENCODING .......
            # ....................................
            
            #    .) A veces SHINY no transforma a UTF-8
            #    .) La fórmula para hacerlo es 
            #    .) Encoding(text) <- "UTF-8"
            

             text <- dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
             
             Encoding(text) <- "UTF-8"
             text 

             

            }
        }
    )
}

 
# ........ FUNCION TRANSLATE NAMES ..........
# ...........................................

#       .) La usaré EN MOD_DATAINPUT
#       .) La usaré para SETEAR NAMES

shiny_set_names <- function(nom,lang) {
  nom %>%
  magrittr::set_names(translate_app(names(.), lang))
}
 

# ........ FUNCION CALL MODULES ..........
# ...........................................

#       .) Cada elemento de la APP a traducir
#       .) Necesita un CALLMODULE
#       .) Hago una FUNCIÓN para automatizar y no reescribir de nuevo

#       .) Para pasar de un STRING a CODE R usamos
#       .) GLUE + EVAL + PARSE (TEXT) + EVAL

callModule_function <- function(tabs,lang){
  
  tabs <- tabs
  
  # Función CREA CallModule de STRING a R CODE
  CM <- function(tabs,a){
    glue::glue("
        shiny::callModule(
          mod_tab_translate, '",tabs[a],"',
          '",tabs[a],"', lang
        )") %>%
          eval() %>%
           parse(text = .) %>%
             eval()
  }
  
  
  # Repetición de TODOS los CALL MODULES
  for (i in 1:length(tabs)) {
    CM(tabs,i)
    
  }
  
}








