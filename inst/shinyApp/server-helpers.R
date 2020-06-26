
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

  print(t)
}

generateUi = function(models, names) {
  assertList(models)
  assertCharacter(names)
  uis = sapply(models, function(x) {
    shiny = MboShiny$new(x)
    shiny_ui = shiny$generatePlotParamUi()
  }, simplify = FALSE)
  names(uis) = names
  return(uis)
}

removeDuplicateUi = function(uis) {
  assertList(uis)
  names = sapply(uis, function(ui, x) {
    get(x, ui[[1]])
    }, x = "param_name")
  test_unique = !duplicated(names)
  unique_uis = uis[test_unique]

  return(unique_uis)
}
