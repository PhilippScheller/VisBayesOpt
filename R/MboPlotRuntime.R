#' @title MboPlotRuntime
#'
#' @include MboPlot-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#'
#' @importFrom R6 R6Class
#' @importFrom tidyr gather
#'
#' @description
#' This class plots the runtime of a mlrMbo run.
#'
#' @export
MboPlotRuntime = R6Class(
  "MboPlotRuntime",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Plots minimal value of model (y) of mbo run over the iterations.
    #'
    #' @return ([ggplot]).
    plot = function() {
      df_extra = convertListOfRowsToDataFrame(self$opt_state$opt.path$env$extra)
      df_time = df_extra[colnames(df_extra) %in% c("train.time", "propose.time")]
      df_time$total.time = df_time$train.time + df_time$propose.time

      df_time_long = wideToLong(df_time, 0)

      gg = ggplot(df_time_long, aes(x = rep(seq(1, nrow(df_time)), nrow(df_time_long)/nrow(df_time))
                                    , y = Value, color = Param))
      gg = gg + geom_ribbon(aes(ymin = 0, ymax = Value, fill = Param), alpha = .2)
      gg = gg + geom_line(na.rm = TRUE)
      gg = gg + theme(legend.title=element_blank())
      gg = gg + xlab("Iteration")
      gg = gg + ylab("Time [seconds]")

      return(gg)
    }
  )
)
