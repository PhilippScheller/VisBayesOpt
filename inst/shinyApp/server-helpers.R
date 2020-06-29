
createTabPanels = function(titles, mbo_plots){

  t = mapply(function(title, plot) {
    # create ui header
    header = renderText({
      return(paste(h4("Runtime split")))
    })
    tab = tabPanel(
      title,
      fluidRow(
        plotOutput(mbo_plots)
      )
    )
  }, title = titles, plot = mbo_plots)

}

generateUi = function(plots, names) {
  assertList(plots)
  assertCharacter(names)
  uis = sapply(plots, function(x) {
    shiny = MboShiny$new(x)
    shiny_ui = shiny$generatePlotParamUi()
  }, simplify = FALSE)
  names(uis) = names
  return(uis)
}

removeDuplicateUi = function(uis) {
  assertList(uis)

  unlist_uis = unlist(uis, recursive = FALSE)
  names = names(unlist(uis, recursive = FALSE))
  duplicates = duplicated(sub(".*\\.", "", unlist(names, recursive = F)))
  # return(duplicates)
  unique_uis = unlist_uis[!duplicates]
  names(unique_uis) = sub(".*\\.", "", unlist(names(unique_uis), recursive = F))# drop the "."


  return(unique_uis)
}

generateMboClasses = function(model_names, mbo_object) {
  assertList(model_names)
  mbo_models = list()
  mbo_classes = lapply(model_names, function(x){
    mbo_models[[x]] = get(x)$new(mbo_object)
  })
  names(mbo_classes) = names(model_names)
  return(mbo_classes)
}

generateMboPlots = function(mbo_class_objects) {
  assertList(mbo_class_objects)
  plots = list()
  mbo_plots = lapply(mbo_class_objects, function(x){
    plots[["x"]] = x$plot()
  })
  names(mbo_plots) = names(mbo_class_objects)
  return(mbo_plots)
}
