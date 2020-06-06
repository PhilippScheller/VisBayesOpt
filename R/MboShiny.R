#' @title MboShiny
#'
#' @include MboShiny-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import shiny
#'
#' @importFrom R6 R6Class
#' @importFrom textutils toHTML
#'
#' @description
#' This class transforms inputs into shiny uis.
#'
#' @export
MboShiny = R6Class(
  "MboShiny",
  public = list(
    #' @field summary (`character()`)\cr
    summary = NULL,
    #' @field opt_state (`OptState`)\cr
    #'   Environment containing necessary information needed during optimization in MBO.
    opt_state = NULL,
    #' @field param_set ([ParamSet])\cr
    #'   Object describing the parameter space of the mbo run.
    param_set = NULL,
    #' @field shiny_uis ('html') \cr
    #'   Shiny ui elements.
    shiny_uis = NULL,
    #' @description defines the parameters to be converted to the ui.
    #'
    #' @param mbo_plot ([MboPlot]).
    initialize = function(mbo_plot) {
      self$opt_state = assert_class(mbo_plot$opt_state, "OptState")
      self$param_set = assert_class(mbo_plot$param_set, "ParamSet")
    },
    #' @description
    #' Generates ui elements of all parameters in the parameter set. The html elements are attached
    #' to the object which calls the function.
    #'
    #' @param silent (`logical()`)
    #'   If TRUE, the output will only be saved in the object and not be returned.
    #'
    #' @return (`html`).
    generateParamUiShiny = function(silent = TRUE) {
      if (length(self$param_set$pars) == 0L) {
        self$shiny_uis = h4("No hyperparameters found in the object provided.")
      } else {
        self$shiny_uis = getParamUi(self$param_set)
      }
      if (!silent) return(self$shiny_uis)
    },
    #' @description
    #' Generates ui text elements of the MboSummary. The html elements are attached to the object
    #'  which calls the function.
    #'
    #' @param silent (`logical()`)
    #'   If TRUE, the output will only be saved in the object and not be returned.
    #'
    #' @return (`html`).
    generateSummaryTextUiShiny = function(silent = TRUE) {
      summary_mbo = MboSummary$new(self$opt_state)
      summary_text = summary_mbo$getMboSummary(silent = FALSE)

      if (length(summary) == 0L) {
        self$summary = h4("No summary found in the object provided.")
      } else {
        self$summary = getParamTextFromMboSummary(summary_text)
      }
      if (!silent) return(self$summary)
    }
  )
)
