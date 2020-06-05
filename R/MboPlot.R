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
#' @import shiny
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
    #' @field opt_state (`OptState`)\cr
    #'   Environment containing necessary information needed during optimization in MBO.
    opt_state = NULL,
    #' @field opt_path ([OptPath])\cr
    #'   Optimization path of the mbo run.
    opt_path = NULL,
    #' @field param_set ([ParamSet])\cr
    #'   Object describing the parameter space of the search.
    param_set = NULL,
    #' @field param_vals ([Param])\cr
    #'   Object containing the parameter values.
    param_vals = NULL,
    #' @field mbo_control ([MBOControl])\cr
    #'   Control object for mbo.
    mbo_control = NULL,
    #' @description
    #' Initializes the parameters of the object.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      self$opt_state = assert_class(opt_state, "OptState")
      self$opt_path = assert_class(opt_state$opt.path, "OptPath")
      self$param_set = assert_class(opt_state$opt.path$par.set, "ParamSet")
      self$mbo_control = assert_class(opt_state$opt.problem$mbo.control, "MBOControl")
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
