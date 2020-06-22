#' @title MboPlotRuntime
#'
#' @include MboPlot-helpers-general.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import BBmisc
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
    #' @param highlight_iter (\code{integer(1) | NULL})\cr
    #' Specifies the iteration to be highlighted. The default \code{NULL} does not highlight any iteration.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = NULL) {
      if (!is.null(highlight_iter)) highlight_iter = assertMultiClass(highlight_iter, c("integer", "numeric"))
      df_extra = convertListOfRowsToDataFrame(self$opt_state$opt.path$env$extra)
      df_time = df_extra[colnames(df_extra) %in% c("train.time", "propose.time")] %>%
        drop_na()
      df_time$total.time = df_time$train.time + df_time$propose.time
      opt_path_df = as.data.frame(self$opt_state$opt.path)

      if (!is.null(highlight_iter)) {
        if(highlight_iter < 0 | highlight_iter > nrow(df_time)) {
          stop("`highlight_iter` exceeds number of iterations from mbo run (n=", nrow(df_time), ")")
        }
      }

      df_time_long = wideToLong(df_time, 0)



      gg = ggplot(df_time_long, aes(x = rep(seq(1, max(opt_path_df$dob)), nrow(df_time_long)/nrow(df_time))
                                    , y = Value, color = Param))
      gg = gg + geom_ribbon(aes(ymin = 0, ymax = Value, fill = Param), alpha = .2)
      if (!is.null(highlight_iter)) {
        gg = gg + geom_line(data = data.frame(x = c(highlight_iter, highlight_iter), y = c(0, Inf)),
                               mapping = aes(x = x, y = y), col = "black", lty = "dashed")
      }
      gg = gg + geom_line(na.rm = TRUE)
      gg = gg + theme(legend.title=element_blank())
      gg = gg + xlab("Iteration")
      gg = gg + ylab("Time [seconds]")

      return(gg)
    }
  )
)
