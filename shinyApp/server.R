#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(checkmate)

# Define server logic 
server <- function(input, output) {

  storage = reactiveValues(mbo1Obj=NULL)
  
  output$mbo1Check = renderText({
      # prevent error message when path is still empty
      validate(
        need(input$mbo1$datapath != "", "Please select a data set")
      )
      # check if provied file can be loaded
      if (is.error(try(readRDS(input$mbo1$datapath), silent = TRUE))) {
        paste("Uploaded file is not a compatible object to 'readRDS'")
      } else {
        storage$mbo1Obj = readRDS(input$mbo1$datapath)
        # check if uploaded object is of a valid class for mbo
        if (test_multi_class(storage$mbo1Obj, c("MBOResult", "MBOSingleObjResult", "OptResult", "TuneResult"))) {
          paste("Upload successfull")
        } else {
          paste("Uploaded file is not of class MBOResult, MBOSingleObjResult, OptResult or TuneResult")
        }
      }
    })
  
    output$mbo1Plot = renderPlot({
      validate(
        need(!is.null(storage$mbo1Obj) , "")
      )
      
      example_mbo1Obj = mboPlotProgress$new(storage$mbo1Obj$final.opt.state)
      plot_mbo1Obj = example_mbo1Obj$plot()
      return(plot_mbo1Obj)
    })
       
  
    
}
