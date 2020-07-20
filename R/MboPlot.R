library(R6)
library(checkmate)
library(mlrMBO)
library(ggplot2)
library(shiny)
library(ParamHelpers)
library(tidyr)
library(dplyr)
library(ggpubr)
library(magrittr)
library(reshape2)

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
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState])\cr
    #'   Environment containing necessary information needed during optimization in MBO.
    opt_state = NULL,
    #' @param param_set ([ParamSet])\cr
    #'   Environment containing necessary parameters for the plot. NOT to be confused with the mbo parameter set.
    param_set = NULL,
    #' @param param_vals ([list])\cr
    #'   Named list containing the parameter values.
    param_vals = NULL,
    #'
    initialize = function(opt_state, param_set, param_vals) {
      self$opt_state = assert_class(opt_state, "OptState")
      self$param_set = assert_class(param_set, "ParamSet")
      self$param_vals = assert_list(param_vals)

      # Do general check of multi-point-proposal and multi-objective here, then we do not need to put it in all function which inherit from 'MboPlot'
      # An adjustment (if e.g. multi-point-proposal is implemented later) is thus only needed here.
      if (opt_state$opt.problem$control$propose.points > 1 | opt_state$opt.problem$control$n.objectives > 1) {
        stop("This plot is not available for multi-point proposals and multi-objective functions.")
      }
    },
    #' @description
    #' Sets parameter values
    #'
    #' @param x (\code{list()})\cr
    set_param_vals = function(x) {
      assertList(x, names = "named")
      is_feasible = isFeasible(par = self$param_set, x)
      if (!is_feasible) {
        stop(attr(is_feasible, "warning"))
      }
      self$param_vals = x
    }
  )
)
