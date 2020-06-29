#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(checkmate)
library(BBmisc)
source("server-helpers.R", local = TRUE)

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

  #FIXME: works but needs implementation for updating params via 'set_param_vals()' function
  # model_names = list(MboPlotDistToNeighbor = "MboPlotDistToNeighbor", MboPlotProgress = "MboPlotProgress",
  #                    MboPlotInputSpace = "MboPlotInputSpace", MboPlotSearchSpace = "MboPlotSearchSpace",
  #                    MboPlotOptPath = "MboPlotOptPath")
  #
  #
  # # generate plots for all present 'MboPlot...()' functions
  # mbo_plots = reactive({
  #   validate(need(storage$check == "ok", ""))
  #   req(model_names)
  #
  #   mbo_class_objects = generateMboClasses(model_names, storage$mboObj1) #create all R6 class objects
  #   mbo_plots = generateMboPlots(mbo_class_objects) #create all plots
  #   mbo_plots$MboPlotDistToNeighbor$set_param_vals(list(dist_measure = input$dist_measure))
  #   return(mbo_plots)
  # })

  # create R6 class plot objects
  observe({
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_summary = MboSummary$new(storage$mboObj1)
    mbo_models$mbo_dist_neighbor = MboPlotDistToNeighbor$new(storage$mboObj1)
    mbo_models$mbo_progress = MboPlotProgress$new(storage$mboObj1)
    mbo_models$mbo_input_space = MboPlotInputSpace$new(storage$mboObj1)
    mbo_models$mbo_search_space = MboPlotSearchSpace$new(storage$mboObj1)

    mbo_models$mbo_runtime = MboPlotRuntime$new(storage$mboObj1)
    mbo_models$mbo_opt_path = MboPlotOptPath$new(storage$mboObj1)
    mbo_models$mbo_fit = MboPlotFit$new(storage$mboObj1)
    mbo_models$mbo_uncertainty = MboPlotEstimationUncertainty$new(storage$mboObj1)
  })

  #Summary of mbo run
  output$mbo1Summary = renderTable({
    validate(need(storage$check == "ok", ""))
    mbo_shiny = MboShiny$new(mbo_models$mbo_summary)
    table_mbo_summary = mbo_shiny$generateSummaryTable()
    return(table_mbo_summary)
  })

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
    mbo_models$mbo_input_space$set_param_vals(list(include_prior = as.logical(input$include_prior)))
    mbo_plots$plot_inputSpace = mbo_models$mbo_input_space$plot()
    return(mbo_plots$plot_inputSpace)
  })

  # Plot search space
  output$SearchSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_search_space$set_param_vals(list(include_y = as.logical(input$include_y)))
    mbo_plots$plot_searchSpace = mbo_models$mbo_search_space$plot()
    return(mbo_plots$plot_searchSpace)
  })

  # Plot distance to neighbor
  output$Dist2NeighborPlot = renderPlot({
    # The R6 class 'MboShiny' names the generated uis based on their names in the function, e.g.in
    # 'MboPlotDistToNeighbor' the plot function is 'plot(dist_measure)' thus the ui is names 'dist_measure'.
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_dist_neighbor$set_param_vals(list(dist_measure = input$dist_measure)) #adjust for selection from input
    mbo_plots$plot_distToNeighbor = mbo_models$mbo_dist_neighbor$plot() # plot based on selected input
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

  # Plot fit
  output$FitPlot = renderPlot({
    req(input$highlight_iter)
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_fit$set_param_vals(list(highlight_iter = input$highlight_iter)) #adjust for selection from input
    mbo_plots$plot_fit = mbo_models$mbo_fit$plot()
    return(mbo_plots$plot_fit)
  })

  # Plot uncertainty
  output$UncertaintyPlot = renderPlot({
    req(input$highlight_iter)
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_uncertainty$set_param_vals(list(highlight_iter = input$highlight_iter)) #adjust for selection from input
    mbo_plots$plot_uncertainty = mbo_models$mbo_uncertainty$plot()
    return(mbo_plots$plot_uncertainty)
  })

  # create uis for plot param_set for tab 'Visualize mlrMBO Run'
  output$ui_run = renderUI({

    validate(need(storage$check == "ok", ""))
    models = list(mbo_dist_neighbor = mbo_models$mbo_dist_neighbor, mbo_input_space = mbo_models$mbo_input_space,
                  mbo_search_space = mbo_models$mbo_search_space)
    names = names(models)
    uis = generateUi(models, names)
    unique_uis = removeDuplicateUi(uis)

    return(uis)
  })

  # create uis for plot param_set for tab 'Diagnostic Tool for Single Iteration'
  output$ui_diagnost = renderUI({
    validate(need(storage$check == "ok", ""))

    models = list(mbo_opt_path = mbo_models$mbo_opt_path, mbo_runtime = mbo_models$mbo_runtime)
    names = names(models)
    uis = generateUi(models, names) # calls 'MboShiny()' for various 'models'
    unique_uis = removeDuplicateUi(uis) # removes uis which are present in several plots (e.g. 'hihlight_iter')
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
