#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(checkmate)
library(BBmisc)
# source("ui-helpers.R", local = TRUE)

# Define server logic
server <- function(input, output) {
  storage = reactiveValues(check = NULL, mboObj1 = NULL, MboPlot1 = NULL, MboPlotProgress = NULL,
                           ShinyMbo1 = NULL)
  output$mbo1Check = renderUI({
    # prevent error message when path is still empty
    validate(need(input$mbo1$datapath != "", "Please select a data set"))
    # check if provied file can be loaded
    if (is.error(try(readRDS(input$mbo1$datapath), silent = TRUE))) {
      storage$check = NULL
      return(p("Uploaded file is not a compatible object to 'readRDS'", style = "color:red"))
    } else {
      storage$mboObj1 = readRDS(input$mbo1$datapath)
      # check if uploaded object is of a valid class for mbo
      if (test_class(storage$mboObj1, c("OptState"))) {
        storage$check = "ok"
        return(p("Upload successfull", style = "color:green"))
      } else {
        storage$check = NULL
        return(p("Uploaded file is not of class OptState", style = "color:red"))
      }
    }
  })

  # Summary of mbo run
  output$mbo1Summary = renderTable({
    validate(need(storage$check == "ok", ""))

    storage$MboPlot1 = MboPlot$new(storage$mboObj1)
    storage$MboPlotProgress = MboPlotProgress$new(storage$mboObj1)
    storage$ShinyMbo1 = MboShiny$new(storage$MboPlot1)
    sumTable = storage$ShinyMbo1$generateSummaryTable(silent = FALSE)
    return(sumTable)
  })

  output$headerSummary = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Characteristics of Mbo Run")))
  })

  # Plot performance over iterations
  output$PerformancePlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    plot_performance = storage$MboPlotProgress$plot()
    return(plot_performance)
  })

  output$headerPerformance = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Optimization Progress: Performance Over Time")))
  })

  # Plot input space
  output$InputSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    mboObj = MboPlotInputSpace$new(storage$mboObj1)
    plot_inputSpace = mboObj$plotInputSpace(type = "overlay", plot = "distribution")
    return(plot_inputSpace)
  })

  output$headerInputSpace = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Input Space Priors and Posteriors")))
  })

  # Plot search space
  output$SearchSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    mboObj = MboPlotInputSpace$new(storage$mboObj1)
    plot_searchSpace = mboObj$plotInputSpace(type = "overlay", plot = "iteration")
    return(plot_searchSpace)
  })

  output$headerSearchSpace = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Evaluated Space of Optimization")))
  })

  # Plot distance to neighbor
  output$Dist2NeighborPlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    mboObj = MboPlotDistToNeighbor$new(storage$mboObj1)
    plot_distToNeighbor = mboObj$plotDistToNeighbor("min", 1L)
    return(plot_distToNeighbor)
  })

  output$headerDist2Neighbor = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Exploration vs Exploitation")))
  })
}






######## Backup

# output$mbo1Ui = renderUI({
#   validate(need(!is.null(storage$ui_mboObj1) , ""))
#
#   return(storage$ui_mboObj1)
# })
