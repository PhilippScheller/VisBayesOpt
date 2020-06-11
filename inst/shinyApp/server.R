#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(checkmate)
library(BBmisc)
# source("ui-helpers.R", local = TRUE)

# Define server logic
server <- function(input, output) {
  storage = reactiveValues(mboObj1 = NULL, MboPlot1 = NULL, MboPlotProgress1 = NULL,
                           ShinyMbo1 = NULL)

  output$mbo1Check = renderText({
    # prevent error message when path is still empty
    validate(need(input$mbo1$datapath != "", "Please select a data set"))
    # check if provied file can be loaded
    if (is.error(try(readRDS(input$mbo1$datapath), silent = TRUE))) {
      paste("Uploaded file is not a compatible object to 'readRDS'")
    } else {
      storage$mboObj1 = readRDS(input$mbo1$datapath)
      # check if uploaded object is of a valid class for mbo
      if (test_class(storage$mboObj1, c("OptState"))) {
        paste("Upload successfull")
      } else {
        paste(
          "Uploaded file is not of class OptState"
        )
      }
    }
  })

  output$mbo1Plot = renderPlot({
    validate(need(!is.null(storage$MboPlotProgress1) , ""))
    plot_mboObj1 = storage$MboPlotProgress1$plot()

    return(plot_mboObj1)
  })

  # output$mbo1Ui = renderUI({
  #   validate(need(!is.null(storage$ui_mboObj1) , ""))
  #
  #   return(storage$ui_mboObj1)
  # })

  output$mbo1Summary = renderText({
    validate(need(!is.null(storage$mboObj1) , ""))

    storage$MboPlot1 = MboPlot$new(storage$mboObj1)
    storage$MboPlotProgress1 = MboPlotProgress$new(storage$mboObj1)
    storage$ShinyMbo1 = MboShiny$new(storage$MboPlot1)
    out = storage$ShinyMbo1$generateSummaryTextUiShiny(silent = FALSE)
  })

}
