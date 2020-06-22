#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(checkmate)
library(BBmisc)
# source("ui-helpers.R", local = TRUE)

# Define server logic
server <- function(input, output, session) {
  storage = reactiveValues(check = NULL, mboObj1 = NULL, MboPlot1 = NULL,
                           par_names_pdp = NULL, mbo_opt_path = NULL)
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
    mbo_plot = MboPlot$new(storage$mboObj1)
    mbo_shiny = MboShiny$new(mbo_plot)

    storage$table_mbo_summary = mbo_shiny$generateSummaryTable()
    return(storage$table_mbo_summary)
  })

  output$headerSummary = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Characteristics of Mbo Run")))
  })

  # Plot performance over iterations
  output$PerformancePlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    mbo_progress = MboPlotProgress$new(storage$mboObj1)
    plot_performance = mbo_progress$plot()
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
    plot_inputSpace = mboObj$plot(include_prior = TRUE)
    return(plot_inputSpace)
  })

  output$headerInputSpace = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Input Space")))
  })

  # Plot search space
  output$SearchSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    mboObj = MboPlotSearchSpace$new(storage$mboObj1)
    plot_searchSpace = mboObj$plot()
    return(plot_searchSpace)
  })

  output$headerSearchSpace = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Evaluated Space of Optimization")))
  })

  # Plot distance to neighbor
  output$Dist2NeighborPlot = renderPlot({
    req(input$distToNeighbor_measure)
    validate(need(storage$check == "ok", ""))


    mboObj = MboPlotDistToNeighbor$new(storage$mboObj1)
    plot_distToNeighbor = mboObj$plot(input$distToNeighbor_measure)
    return(plot_distToNeighbor)
  })

  output$headerDist2Neighbor = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Exploration vs Exploitation")))
  })

  ######## Diagnostic section

  # Plot opt path

  #create r6 class object 'mbo_opt_path'
  observe({
    req(storage$mboObj1)
    storage$mbo_opt_path = MboPlotOptPath$new(storage$mboObj1)
  })
  # update selectInput based on features in 'mbo_opt_path'
  observe({
    req(storage$mbo_opt_path)
    choices = names(storage$mbo_opt_path$opt_state$opt.path$par.set$pars)
    updateSelectInput(session, inputId =  "pdp_feature",
                      choices = choices)
  })
  # create plot
  output$OptPathPlot = renderPlot({
    req(input$highlight_iteration, storage$mbo_opt_path)
    validate(need(storage$check == "ok", ""))

    plot_optPath = storage$mbo_opt_path$plot(highlight_iter = input$highlight_iteration,
                                             feature = input$pdp_feature)
    return(plot_optPath)
  })

  output$headerOptPath = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Visualize Model Single Iteration")))
  })

  # Plot runtime
  output$RuntimePlot = renderPlot({
    req(input$highlight_iteration)
    validate(need(storage$check == "ok", ""))


    mboObj = MboPlotRuntime$new(storage$mboObj1)
    plot_runtime = mboObj$plot(highlight_iter = input$highlight_iteration)
    return(plot_runtime)
  })

  output$headerRuntime = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Runtime split")))
  })

}






######## Backup

# output$mbo1Ui = renderUI({
#   validate(need(!is.null(storage$ui_mboObj1) , ""))
#
#   return(storage$ui_mboObj1)
# })
