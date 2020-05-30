#### Helpers for 'generateParamUiShiny()'

getParamUi = function(par_set) {
  expect_class(par_set, "ParamSet")

  par_types = as.list(getParamTypes(par_set))
  pars = par_set$pars
  par_names = names(pars)

  uiFeat = mapply(
    getParamUiFeatures,
    param = pars,
    param_type = par_types,
    param_name = par_names,
    SIMPLIFY = FALSE
  )
  ui = lapply(uiFeat, function(uiFeat)
    makeParamUi(
      uiFeat$lower,
      uiFeat$upper,
      uiFeat$value,
      uiFeat$par_type,
      uiFeat$par_id,
      uiFeat$par_name
    ))

  return(ui)
}

getParamUiFeatures = function(param,
                              param_type,
                              param_name,
                              steps_numeric = 100L) {
  if (param_type %in% c("integer", "numeric")) {
    lower = param$lower
    upper = param$upper
    if (param_type == "integer") {
      value = 1L
    } else {
      value = 1 / steps_numeric
    }
  } else {
    lower = NA
    upper = NA
    value = c(unlist(getValues(param)))
  }
  par_id = paste0("ui.id.", param_name)

  uiFeat = list(
    lower = lower,
    upper = upper,
    value = value,
    par_type = param_type,
    par_id = par_id,
    par_name = param_name
  )
  return(uiFeat)
}

makeParamUi = function(lower, upper, value, par_type, par_id, par_name) {
  check_numeric(lower, null.ok = TRUE)
  check_numeric(upper, null.ok = TRUE)

  if (par_type %in% c("integer", "numeric")) {
    ui = numericInput(
      inputId = par_id,
      value = NULL,
      label = par_name,
      min = lower,
      max = upper,
      step = value
    )
  } else {
    ui = radioButtons(inputId = par.id,
                      label = par_name,
                      choices = value)
  }
  return(ui)
}
