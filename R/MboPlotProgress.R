#' @title MboPlotProgress
#'
#' @include MboPlot-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#'
#' @importFrom R6 R6Class
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
    #' Plots minimal value of model (y) of mbo run over the iterations.
    #'
    #' @return ([ggplot]).
    plot = function() {
      opdf = as.data.frame(self$opt_state$opt.path)
      opdf$cumy = cummin(opdf$y) #cummax if max problem
      opdf = opdf[opdf$dob <= self$param_vals$max_iter,]

      g = ggplot(opdf, aes(x = dob, y = cumy))
      g = g + geom_line()
      return(g)
    }
  )
)
