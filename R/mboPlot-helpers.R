#### Helpers for 'generateParamUiShiny()'

getParamUi = function(par.set) {
  expect_class(par.set, "ParamSet")

  par_types = as.list(getParamTypes(par.set))
  pars = par.set$pars
  par_names = names(pars)

  uiFeat = mapply(
    getParamUiFeatures,
    param = pars,
    param.type = par_types,
    param.name = par_names,
    SIMPLIFY = FALSE
  )
  ui = lapply(uiFeat, function(uiFeat)
    makeParamUi(
      uiFeat$lower,
      uiFeat$upper,
      uiFeat$value,
      uiFeat$par_type,
      uiFeat$par_id
    ))

  return(ui)
}

getParamUiFeatures = function(param,
                              param.type,
                              param.name,
                              steps.numeric = 100L) {
  if (param.type %in% c("integer", "numeric")) {
    lower = param$lower
    upper = param$upper
    if (param.type == "integer") {
      value = 1L
    } else {
      value = 1 / steps.numeric
    }
  } else {
    lower = NA
    upper = NA
    value = c(unlist(getValues(param)))
  }
  par_id = paste0("ui.id.", param.name)

  uiFeat = list(
    lower = lower,
    upper = upper,
    value = value,
    par_type = param.type,
    par_id = par_id
  )
  return(uiFeat)
}

makeParamUi = function(lower, upper, value, par.type, par.id) {
  check_numeric(lower, null.ok = TRUE)
  check_numeric(upper, null.ok = TRUE)

  if (par.type %in% c("integer", "numeric")) {
    ui = numericInput(
      inputId = par.id,
      value = NULL,
      label = lower,
      min = lower,
      max = upper,
      step = value
    )
  } else {
    ui = radioButtons(inputId = par.id,
                      label = NULL,
                      choices = value)
  }
  return(ui)
}
