#' @title MboPlotProgress
#'
#' @include MboPlot-helpers-general.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#'
#' @importFrom R6 R6Class
#' @importFrom grDevices atop
#'
#' @description
#' This class plots the minimum value (y) over the iterations of a mlrMbo run.
#'
#' @export
MboPlotProgress = R6Class(
  "MboPlotProgress",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet()
      param_vals = list()
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots minimal value of model (y) of mbo run over the iterations.
    #'
    #' @return ([ggplot]).
    plot = function() {
      opt_path_df = data.frame(self$opt_state$opt.path)

      if (self$opt_state$opt.problem$control$minimize) {
        opt_path_df$cumy = cummin(opt_path_df$y)
      } else {
        pt_path_df$cumy = cummax(opt_path_df$y)
      }
      opt_path_df = opt_path_df[opt_path_df$dob <= self$opt_state$loop,]

      gg = ggplot(opt_path_df, aes(x = dob, y = cumy))
      gg = gg + geom_line()
      gg = gg + xlab(expression("Iteration " *italic(n)))
      gg = gg + ylab(expression("Min " *italic(f(x)) *" after " *italic(n) *" iterations"))
      gg = gg + ggtitle(expression("Progress of optimization"))
      return(gg)
    }
  )
)
