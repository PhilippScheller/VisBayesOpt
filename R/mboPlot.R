# library(R6)
# library(checkmate)
# library(mlrMBO)
# library(ggplot2)
# library(shiny)
# library(ParamHelpers)

#' @title mboPlot
#'
#' @include mboPlot-helpers.R
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
mboPlot = R6Class(
  "mboPlot",
  public = list(
    #' @field opt_state (`OptState`)\cr
    #' Environment containing necessary information needed during optimization in MBO.
    opt_state = NULL,
    #' @field param_set ([ParamSet])\cr
    #' Object describing the parameter space of the search.
    param_set = NULL,
    #' @field param_vals ([Param])\cr
    #' Object containing the parameter values.
    param_vals = NULL,
    #' @field param_types ('char()')\cr
    #' Object containing the parameter values.
    param_types = NULL,
    #' @field control ([MBOControl])\cr
    #' Control object for mbo.
    control = NULL,
    #' @field shiny_uis ('html') \cr
    #' Shiny ui elements.
    shiny_uis = NULL,
    #' @description
    #' Pins the parameters to the object.
    #'
    #' @param opt_state ([OptState])
    #' @param param_set ([ParamSet])
    #' @param param_vals ([Param])
    #' @param control ([MBOControl])
    initialize = function(opt_state, param_set, param_vals, control) {
      self$opt_state = assert_class(opt_state, "OptState")
      self$param_set = assert_class(param_set, "ParamSet")
      self$param_vals = assert_list(param_vals)
      self$control = assert_class(control, "MBOControl")
    },
    #' @description
    #' Sets parameter values
    #'
    #' @param x ([Param][ParamSet])
    set_param_vals = function(x) {
      assert_list(x, names = "named")
      is_feasible = isFeasible(par = self$param_set, x)
      if (!is_feasible) {
        stop(attr(is_feasible, "warning"))
      }
      self$param_vals = x
    },
    #' @description
    #' Some plot function
    plot = function() {
      stop("abstract")
    },
    #' @description
    #' Generates ui elements of all parameters in the parameter set. The html elements are attached
    #' to the object which calls the function.
    #'
    #' @return html of ui for each parameter of the param_set
    generateParamUiShiny = function() {
      if (length(self$param_set$pars) == 0L) {
        self$shiny_uis = h4("No hyperparameters found in the object provided.")
        return(self$shiny_uis)
      } else {
        self$shiny_uis = getParamUi(self$param_set)
        return(self$shiny_uis)
      }
    }
  )
)


#' @export
mboPlotProgress = R6Class(
  "mboPlotProgress",
  inherit = mboPlot,
  public = list(
    initialize = function(opt_state) {
      param_set = makeParamSet(makeIntegerParam("max_iter", lower = 0, upper = opt_state$loop))
      param_vals = list(max_iter = opt_state$loop)
      control = opt_state$opt.problem$control
      param_shiny_ui = super$generateParamUiShiny()
      super$initialize(opt_state, param_set, param_vals, control)
    },
    plot = function() {
      opdf = as.data.frame(self$opt_state$opt.path)
      opdf$cumy = cummin(opdf$y) #cummax if max problem
      opdf = opdf[opdf$dob <= self$param_vals$max_iter, ]

      g = ggplot(opdf, aes(x = dob, y = cumy))
      g = g + geom_line()
      return(g)
    }

  )
)
#######
# Example:
# mboObj1 = readRDS("../test-data/mboObj.RData")
#
# plot_progress = mboPlotProgress$new(mboObj1$final.opt.state)
# plot_progress$plot()
# plot_progress$generateParamUiShiny()




