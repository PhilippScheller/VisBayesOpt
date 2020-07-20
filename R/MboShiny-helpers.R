# helpers for 'MboShiny'
# helpers for 'generateParamUiShiny()'
generateParamUi = function(param_set, param_vals) {
  param_types = getParamTypes(param_set)
  param_names = getParamIds(param_set)

  params_uis = mapply(function(param, param_val, param_type, param_name){
    param_choice = get(param_name, param)$values # get value for the respective parameter which should be displayed in sidebar panel of app.
    input_label = paste("Select value for", param_name) # create standardized labelling which shows up in sidebar panel of app.

    # For numeric and integer parameters we create a 'numericInput'
    if (param_type %in% c("numeric", "integer")) {
      if (param_type == "integer") {
        step = 1L
      } else {
        step = NA
      }
      if (is.null(param$lower)) param$lower = NA
      if (is.null(param$upper)) param$upper = NA
      input = numericInput(param_name, value = param_val, min = param$lower,
                         max = param$upper, step = step, label = input_label)
    } else {
      # For discrete and logical parameters we create a 'selectInput'
      if (param_type %in% c("logical", "discrete")) {
        input = selectInput(param_name, label = input_label, choices = param_choice)
      } else {
      # For everything else we create a 'textInput'
        input = textInput(param_name, param_val, label = input_label)
      }
    }
    # store all parameter uis in a list.
    input_list = list()
    input_list[[param_name]] = input

    return(input_list)
  }, param = list(param_set$pars), param_val = param_vals, param_type = param_types, param_name = param_names)
  return(params_uis)
}

# function for generating table of summary.
getParamTableFromMboSummary = function(mboSummary, names = c("Characteristic", "Value")) {
  assertClass(names, "character")
  if (length(names) != 2L) stop("Names vector has to much/less elements")

  # call mboSummary on all objects from the list, i.e. all params.
  text_values = lapply(mboSummary, function(x) {
    values = get("value", x)
    values_combined = paste(values, collapse = ", ")
    return(values_combined)
  })
  text_names = lapply(mboSummary, function(x) {
    names = get("name", x)
    names_combined = paste(names,  ":")
    return(names_combined)
  })

  text_mat = matrix(c(unlist(text_names), unlist(text_values)), nrow = length(text_names))
  text_df = data.frame(text_mat, stringsAsFactors = FALSE)
  names(text_df) = names

  return(text_df)
}


# #backup old functions
# #######################################################################################################################
#
# paramBox = function(title, input, fill = TRUE) {
#   content = div(class = "param-box",
#                 span(class = "param-box-title", class = if (fill) "param-box-filled",
#                      title),
#                 span(class = "param-box-inp", input),
#                 # span(class = "param-box-desc", desc)
#   )
#   content = div(class = "col-sm-12", content)
#   return(content)
# }
# getParamUi = function(par_set) {
#   expect_class(par_set, "ParamSet")
#
#   par_types = as.list(getParamTypes(par_set))
#   pars = par_set$pars
#   par_names = names(pars)
#
#   uiFeat = mapply(
#     getParamUiFeatures,
#     param = pars,
#     param_type = par_types,
#     param_name = par_names,
#     SIMPLIFY = FALSE
#   )
#   ui = lapply(uiFeat, function(uiFeat) {
#     makeParamUi(
#       uiFeat$lower,
#       uiFeat$upper,
#       uiFeat$value,
#       uiFeat$par_type,
#       uiFeat$par_id,
#       uiFeat$par_name
#     )
#   })
#
#   return(ui)
# }
#
# getParamUiFeatures = function(param,
#                               param_type,
#                               param_name,
#                               steps_numeric = 100L) {
#   if (param_type %in% c("integer", "numeric")) {
#     lower = param$lower
#     upper = param$upper
#     if (param_type == "integer") {
#       value = 1L
#     } else {
#       value = 1 / steps_numeric
#     }
#   } else {
#     lower = NA
#     upper = NA
#     value = c(unlist(getValues(param)))
#   }
#   par_id = paste0("ui.id.", param_name)
#
#   uiFeat = list(
#     lower = lower,
#     upper = upper,
#     value = value,
#     par_type = param_type,
#     par_id = par_id,
#     par_name = param_name
#   )
#   return(uiFeat)
# }
#
# makeParamUi = function(lower, upper, value, par_type, par_id, par_name) {
#   check_numeric(lower, null.ok = TRUE)
#   check_numeric(upper, null.ok = TRUE)
#
#   if (par_type %in% c("integer", "numeric")) {
#     ui = numericInput(
#       inputId = par_id,
#       value = NULL,
#       label = par_name,
#       min = lower,
#       max = upper,
#       step = value
#     )
#   } else {
#     ui = radioButtons(inputId = par_id,
#                       label = par_name,
#                       choices = value)
#   }
#   return(ui)
# }
#
#
#
#
#
