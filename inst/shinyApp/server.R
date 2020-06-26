#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(checkmate)
library(BBmisc)
# source("ui-helpers.R", local = TRUE)

# Define server logic
server <- function(input, output, session) {
  storage = reactiveValues()
  mbo_models = reactiveValues()
  mbo_plots = reactiveValues()

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


  # create R6 class plot objects
  observe({
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_plot_dist_neighbor = MboPlotDistToNeighbor$new(storage$mboObj1)
    mbo_models$mbo_progress = MboPlotProgress$new(storage$mboObj1)
    mbo_models$mbo_input_space = MboPlotInputSpace$new(storage$mboObj1)
    mbo_models$mbo_search_space = MboPlotSearchSpace$new(storage$mboObj1)

    mbo_models$mbo_runtime = MboPlotRuntime$new(storage$mboObj1)
    mbo_models$mbo_opt_path = MboPlotOptPath$new(storage$mboObj1)
  })

  # Summary of mbo run
  # output$mbo1Summary = renderTable({
  #   validate(need(storage$check == "ok", ""))
  #   mbo_plot = MboPlot$new(storage$mboObj1)
  #   mbo_shiny = MboShiny$new(mbo_plot)
  #
  #   storage$table_mbo_summary = mbo_shiny$generateSummaryTable()
  #   return(storage$table_mbo_summary)
  # })

  output$headerSummary = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Characteristics of Mbo Run")))
  })

  #Plot performance over iterations
  output$PerformancePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_plots$plot_performance = mbo_models$mbo_progress$plot()
    return(mbo_plots$plot_performance)
  })

  # Plot input space
  output$InputSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_plots$plot_inputSpace = mbo_models$mbo_input_space$plot()
    return(mbo_plots$plot_inputSpace)
  })

  # Plot search space
  output$SearchSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_plots$plot_searchSpace = mbo_models$mbo_search_space$plot()
    return(mbo_plots$plot_searchSpace)
  })

  # Plot distance to neighbor
  output$Dist2NeighborPlot = renderPlot({
    # The R6 class 'MboShiny' names the generated uis based on their names in the function, e.g.in
    # 'MboPlotDistToNeighbor' the plot function is 'plot(dist_measure)' thus the ui is names 'dist_measure'.

    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_plot_dist_neighbor$set_param_vals(list(dist_measure = input$dist_measure)) #adjust for selection from input
    mbo_plots$plot_distToNeighbor = mbo_models$mbo_plot_dist_neighbor$plot() # plot based on selected input


    return(mbo_plots$plot_distToNeighbor)
  })


  # ######## Diagnostic section

  # Plot opt path
  output$OptPathPlot = renderPlot({
    req(input$highlight_iter, input$feature)
    validate(need(storage$check == "ok", ""))

    mbo_models$mbo_opt_path$set_param_vals(list(highlight_iter = input$highlight_iter, feature = input$feature))
    mbo_plots$plot_optPath = mbo_models$mbo_opt_path$plot()
    return(mbo_plots$plot_optPath)
  })

  # Plot runtime
  output$RuntimePlot = renderPlot({
    req(input$highlight_iter)
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_runtime$set_param_vals(list(highlight_iter = input$highlight_iter)) #adjust for selection from input
    mbo_plots$plot_runtime = mbo_models$mbo_runtime$plot()
    return(mbo_plots$plot_runtime)
  })

  # create uis for plot param_set for tab 'Visualize mlrMBO Run'
  output$ui_run = renderUI({

    validate(need(storage$check == "ok", ""))
    models = list(mbo_plot_dist_neighbor = mbo_models$mbo_plot_dist_neighbor)
    names = names(models)
    uis = generateUi(models, names)
    unique_uis = removeDuplicateUi(uis)

    return(unique_uis)
  })

  # create uis for plot param_set for tab 'Diagnostic Tool for Single Iteration'
  output$ui_diagnost = renderUI({
    validate(need(storage$check == "ok", ""))

    models = list(mbo_runtime = mbo_models$mbo_runtime, mbo_opt_path = mbo_models$mbo_opt_path, )
    names = names(models)
    uis = generateUi(models, names)
    unique_uis = removeDuplicateUi(uis)

    # print(unique_uis)
    return(unique_uis)
  })
}






######## Backup

# output$mbo1Ui = renderUI({
#   validate(need(!is.null(storage$ui_mboObj1) , ""))
#
#   return(storage$ui_mboObj1)
# })


# observe({
#   req(storage$mboObj1)
#   storage$mbo_opt_path = MboPlotOptPath$new(storage$mboObj1)
# })
# # update selectInput based on features in 'mbo_opt_path'
# observe({
#   req(storage$mbo_opt_path)
#   choices = names(storage$mbo_opt_path$opt_state$opt.path$par.set$pars)
#   updateSelectInput(session, inputId =  "pdp_feature",
#                     choices = choices)
# })
# # create plot
# output$OptPathPlot = renderPlot({
#   req(input$highlight_iteration, storage$mbo_opt_path)
#   validate(need(storage$check == "ok", ""))
#
#   plot_optPath = storage$mbo_opt_path$plot(highlight_iter = input$highlight_iteration,
#                                            feature = input$pdp_feature)
#   return(plot_optPath)
# })
