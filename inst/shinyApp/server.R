#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(checkmate)

# Define server logic
server <- function(input, output) {
  storage = reactiveValues(mbo1Obj = NULL, mboPlot1 = NULL)

  output$mbo1Check = renderText({
    # prevent error message when path is still empty
    validate(need(input$mbo1$datapath != "", "Please select a data set"))
    # check if provied file can be loaded
    if (is.error(try(readRDS(input$mbo1$datapath), silent = TRUE))) {
      paste("Uploaded file is not a compatible object to 'readRDS'")
    } else {
      storage$mbo1Obj = readRDS(input$mbo1$datapath)
      # check if uploaded object is of a valid class for mbo
      if (test_multi_class(
        storage$mbo1Obj,
        c(
          "MBOResult",
          "MBOSingleObjResult",
          "OptResult",
          "TuneResult"
        )
      )) {
        paste("Upload successfull")
      } else {
        paste(
          "Uploaded file is not of class MBOResult, MBOSingleObjResult, OptResult or TuneResult"
        )
      }
    }
  })

  output$mbo1Plot = renderPlot({
    validate(need(!is.null(storage$mbo1Obj) , ""))

    storage$mboPlot1 = mboPlotProgress$new(storage$mbo1Obj$final.opt.state)
    storage$ui_mbo1Obj = storage$mboPlot1$generateParamUiShiny()

    plot_mbo1Obj = storage$mboPlot1$plot()
    return(plot_mbo1Obj)
  })

  output$mbo1Ui = renderUI({
    validate(need(!is.null(storage$ui_mbo1Obj) , ""))

    return(storage$ui_mbo1Obj)
  })






}
