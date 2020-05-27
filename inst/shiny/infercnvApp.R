#' Run infercnv shiny app
#'
#' This function is used to run the infercnv app. 
#'
#' @return The Shiny App Runs 
#' @export
#' @examples
#' if(interactive()){
#'   infercnvApp()
#' }
#'

infercnvApp <- function() {
    appDir <- system.file("shiny", package = "singleCellTK")
    # shiny::shinyOptions()
    shiny::runApp(appDir, display.mode = "normal")
}
