#' Run Shiny App on local machine
#'
#' This function helps to deploy the shiny app in an easy way
#' on the local machine.
#'
#' @import shiny
#'
#' @export

runAppLocal = function() {

  appDir <- system.file("shinyApp", package = "VisBayesOpt")

  shiny::runApp(appDir, display.mode = "normal")
}
