#' @title mod_techSpecsOutput and mod_techSpecs
#'
#' @description Shiny module to show the technical specifications text
#'
#' @param id shiny id
#'
#' @export
mod_techSpecsOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns('techSpecs'))
  )
}

#' mod_techSpecs server function
#'
#' @details mod_techSpecs generates the crossvalidations table
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_techSpecsOutput
mod_techSpecs <- function(
  input, output, session,
  lang
) {

  output$techSpecs <- shiny::renderUI({
    
    # ...... RMD TRANSLATE ........
    # .............................
    
    #       .) Cuando tenga los 3 MD (Markdown) traducidos
    #       .) Aplicaré este còdigo

    markdown_translated <- glue::glue(  "SiteDrought_technical_especifications_{lang()}.md" )
    
    
    # ...... RMD NO TRANSALATE ........
    # .................................
    
    #       .) solo tengo MD Catalan
    #       .) uso un solo MD
    
    # markdown_translated <-  "SiteDrought_technical_especifications_cat.md" 
     
    # .......... RENDER RMD ..............
    # ....................................
      
    #       .) TAGLIST = Pestaña en la que vamos a visualizar
    #       .) FLUID PAGE = Tipo de Pestaña
    #       .) WITHMATHJAX = Para visualizar FORMULAS del MD
    #       .) READLINE = Para activar el RENDER RMARKDOWN
    #       .) RENDER MARKDOWN = Renderiza un MD
       
    
    shiny::tagList(
      shiny::fluidPage(
          shiny::withMathJax(
            shiny::HTML(
                 readLines(
                   encoding = "UTF-8",
                   rmarkdown::render(  
                     input = system.file('resources', markdown_translated, package = 'siteDroughtApp'),
                     output_format = rmarkdown::html_fragment(),
                     quiet = TRUE
                   )
                )
             )
          )
       )
    )
 

  })

}
