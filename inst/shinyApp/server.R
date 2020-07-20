#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(checkmate)
library(ParamHelpers)
library(BBmisc)
source("server-helpers.R", local = TRUE)

# Define server logic
server <- function(input, output, session) {
  storage = reactiveValues()
  mbo_models = reactiveValues()
  mbo_plots = reactiveValues()

  output$mbo1Check = renderUI({
    # prevent error message when path is still empty
    validate(need(input$mbo1$datapath != "", "Select a data set"))
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
  # fully automatic generation of Uis
  # model_names = list(MboPlotDistToNeighbor = "MboPlotDistToNeighbor", MboPlotProgress = "MboPlotProgress",
  #                    MboPlotInputSpace = "MboPlotInputSpace", MboPlotSearchSpace = "MboPlotSearchSpace",
  #                    MboPlotOptPath = "MboPlotOptPath")
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
    mbo_models$mbo_dependencies = MboPlotDependencies$new(storage$mboObj1)

    mbo_models$mbo_runtime = MboPlotRuntime$new(storage$mboObj1)
    mbo_models$mbo_opt_path = MboPlotOptPath$new(storage$mboObj1)
    mbo_models$mbo_fit = MboPlotFit$new(storage$mboObj1)
    mbo_models$mbo_uncertainty = MboPlotEstimationUncertainty$new(storage$mboObj1)
  })

  #Summary of mbo run
  output$mbo1Summary = renderTable({
    validate(need(storage$check == "ok", ""))
    mbo_shiny = MboShiny$new(mbo_models$mbo_summary)
    storage$table_mbo_summary = mbo_shiny$generateSummaryTable()
    return(storage$table_mbo_summary)
  })
  output$mbo1Summary1 = renderTable({
    req(storage$table_mbo_summary)
    return(storage$table_mbo_summary)
  })
  output$headerSummary1 = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Characteristics of MBO Run")))
  })

  output$headerSummary = renderText({
    validate(need(storage$check == "ok", ""))
    return(paste(h4("Characteristics of MBO Run")))
  })

  #Plot performance over iterations
  output$PerformancePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_plots$plot_performance = mbo_models$mbo_progress$plot()
    storage$CurrPlot = mbo_plots$plot_performance
    return(mbo_plots$plot_performance)
  })

  # Plot input space
  output$InputSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_input_space$set_param_vals(list(include_init_design_sampling_distribution = as.logical(input$include_init_design_sampling_distribution)))
    mbo_plots$plot_inputSpace = mbo_models$mbo_input_space$plot(search_space_components = input$dep_choice)
    storage$CurrPlot =  mbo_plots$plot_inputSpace
    return(mbo_plots$plot_inputSpace)
  })

  # Plot search space
  output$SearchSpacePlot = renderPlot({
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_search_space$set_param_vals(list(include_y = as.logical(input$include_y),
                                                    include_init_design = as.logical(input$include_init_design)))
    mbo_plots$plot_searchSpace = mbo_models$mbo_search_space$plot(search_space_components = input$dep_choice)
    storage$CurrPlot =  mbo_plots$plot_searchSpace
    return(mbo_plots$plot_searchSpace)
  })

  # Plot distance to neighbor
  output$Dist2NeighborPlot = renderPlot({
    # The R6 class 'MboShiny' names the generated uis based on their names in the function, e.g.in
    # 'MboPlotDistToNeighbor' the plot function is 'plot(dist_measure)' thus the ui is names 'dist_measure'.
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_dist_neighbor$set_param_vals(list(dist_measure = input$dist_measure,
                                                     include_init_design = as.logical(input$include_init_design))) #adjust for selection from input
    mbo_plots$plot_distToNeighbor = mbo_models$mbo_dist_neighbor$plot() # plot based on selected input
    storage$CurrPlot =  mbo_plots$plot_distToNeighbor # needed for export plot if required by user
    return(mbo_plots$plot_distToNeighbor)
  })

  # Plot dependencies
  # FIXME: this plot is manually included since it would need an option to make a character parameter set without a defined length to choose search space components
  output$DependenciesPlot = renderPlot({
    validate(need(storage$check == "ok", ""))

    mbo_models$mbo_dependencies$set_param_vals(list(color_y = as.logical(input$color_y)))

    mbo_plots$plot_dependencies = mbo_models$mbo_dependencies$plot(search_space_components = input$dep_choice)
    storage$CurrPlot =  mbo_plots$plot_dependencies
    return(mbo_plots$plot_dependencies)
  })
  output$dep_choice = renderUI({
    validate(need(storage$check == "ok", ""))
    req(storage$mboObj1)
    checkboxGroupInput("dep_choice", "Select value for search_space_components",
                                               choices = getParamIds(storage$mboObj1$opt.path$par.set),
                                               selected = getParamIds(storage$mboObj1$opt.path$par.set)[1:2]
  )})
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
    req(input$highlight_iter, input$predict_y_iter_surrogate)
    validate(need(storage$check == "ok", ""))
    mbo_models$mbo_fit$set_param_vals(list(highlight_iter = input$highlight_iter,
                                           predict_y_iter_surrogate = as.logical(input$predict_y_iter_surrogate))) #adjust for selection from input
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
    models = list(mbo_input_space = mbo_models$mbo_input_space, mbo_search_space = mbo_models$mbo_search_space,
                  mbo_dist_neighbor = mbo_models$mbo_dist_neighbor, mbo_dependencies = mbo_models$mbo_dependencies)
    names = names(models)
    uis = generateUi(models, names)
    unique_uis = removeDuplicateUi(uis)

    return(unique_uis)
  })

  # create uis for plot param_set for tab 'Diagnostic Tool for Single Iteration'
  output$ui_diagnost = renderUI({
    validate(need(storage$check == "ok", ""))

    models = list(mbo_runtime = mbo_models$mbo_runtime, mbo_fit = mbo_models$mbo_fit, mbo_uncertainty = mbo_models$mbo_uncertainty,
                  mbo_opt_path = mbo_models$mbo_opt_path)
    names = names(models)
    uis = generateUi(models, names) # calls 'MboShiny()' for various 'models'
    unique_uis = removeDuplicateUi(uis) # removes uis which are present in several plots (e.g. 'hihlight_iter')
    return(unique_uis)
  })

# Export plots png
  #read directory from input
  shinyDirChoose(input, 'inputDir', roots = c(home = '~'))

  observeEvent(input$inputDir, {
    req(input$inputDir)
    output$directorySuccess = renderUI({
      return(p("Valid path", style = "color:green"))
    })
  })
  #create png export based on directory
  observeEvent(input$exportPlot, {
    req(input$inputDir)
    errorPath = FALSE
    tryCatch({
      localDir = paste0("~", paste(unlist(input$inputDir$path), collapse = "/"))
      ggsave(path = localDir, filename = "plot.png", plot = storage$CurrPlot, width = 30,
        height = 12, units = "cm", dpi = 600)
    },
    error = function(contd) {
      errorPath = TRUE
    })
    if (errorPath) {
      output$saveSuccess = renderUI({
        return(p("File path does not exist. Please make sure path is correctly specified.", style = "color:red"))
      })
    } else {
      output$saveSuccess = renderUI({
        return(p("File saved", style = "color:green"))
      })
    }
  })
}
