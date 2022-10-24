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

    # markdown_translated <- glue::glue(
    #   "SiteDrought_technical_especifications_{lang()}.md"
    # )
    
    markdown_translated <- glue::glue(
     
      # .......... HTML .............
      # .............................
      
      #       .) POSITIVO = detecta bine el RMD
      #       .) NEGATIVO = canvia el CSS de TODO
      
      # "SiteDrought_technical_especifications_cat.html"
      
      # .......... RMD ..............
      # .............................
      
      #       .) POSITIVO = detecta bien el CSS
      #       .) NEGATIVO = no detecta bien el RMD (tema apartados a,b,c,)
      
      "SiteDrought_technical_especifications_cat.Rmd"

    )
    

    shiny::tagList(
      shiny::fluidPage(
        shiny::withMathJax(
            shiny::includeMarkdown(  
             system.file('resources', markdown_translated, package = 'siteDroughtApp')
             
          )
        )
      )
    )
 

  })

}
