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
    #' @field mbo_plot ([MboPlot])\cr
    #'   Object of class `MboPlot`.
    mbo_plot = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param mbo_plot ([MboPlot]).
    initialize = function(mbo_plot) {
      self$mbo_plot = mbo_plot
    },
    #' @description
    #' Generates ui elements of all parameters in the parameter set. The html elements are attached
    #' to the object which calls the function.
    #'
    #' @param silent (`logical()`)
    #'   If TRUE, the output will only be saved in the object and not be returned.
    #'
    #' @return (`html`).
    generatePlotParamUi = function() {
      if (isEmpty(self$mbo_plot$param_set)) {
        shiny_uis = h4("There is no param_set for this plot type.")
      } else {
        shiny_uis = generateParamUi(self$mbo_plot$param_set, self$mbo_plot$param_vals)
      }
      return(shiny_uis)
    },
    #' @description
    #' Generates a table of the MboSummary. The html elements are attached to the object
    #'  which calls the function.
    #'
    #' @return (`html`).
    generateSummaryTable = function() {
      summary_mbo = MboSummary$new(self$mbo_plot$opt_state)
      summary_text = summary_mbo$getMboSummary()

      if (length(summary) == 0L) {
        summary = h4("No summary found in the plot object provided.")
      } else {
        summary = getParamTableFromMboSummary(summary_text)
      }
      return(summary)
    }
  )
)

