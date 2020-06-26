# library(R6)
# library(checkmate)
# library(mlrMBO)
# library(ggplot2)
# library(shiny)
# library(ParamHelpers)

#' @title MboPlot
#'
#' @import checkmate
#' @import ggplot2
#' @import mlrMBO
#' @import ParamHelpers
#'
#' @importFrom R6 R6Class
#'
#' @description
#' This is the base class for plotting in this package.
#'
#' @export
MboPlot = R6Class(
  "MboPlot",
  public = list(
    #' @field opt_state ([OptState])\cr
    #'   Environment containing necessary information needed during optimization in MBO.
    opt_state = NULL,
    #' @field param_set ([ParamSet])\cr
    #'   Environment containing necessary parameters for the plot. NOT to be confused with the mbo parameter set.
    param_set = NULL,
    #' @field param_vals ([Param])\cr
    #'   Object containing the parameter values.
    param_vals = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state, param_set, param_vals) {
      self$opt_state = assert_class(opt_state, "OptState")
      self$param_set = assert_class(param_set, "ParamSet")
      self$param_vals = assert_list(param_vals)
    },
    #' @description
    #' Sets parameter values
    #'
    #' @param x ([Param][ParamSet]).
    set_param_vals = function(x) {
      assert_list(x, names = "named")
      is_feasible = isFeasible(par = self$param_set, x)
      if (!is_feasible) {
        stop(attr(is_feasible, "warning"))
      }
      self$param_vals = x
    }
  )
)
