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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeIntegerParam("highlight_iter"))
      param_vals = list(highlight_iter = 1L) # default value, else set with function `set_param_vals()`
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots minimal value of model (y) of mbo run over the iterations.
    #'
    #' @param highlight_iter (\code{integer(1) | NULL})\cr
    #' Specifies the iteration to be highlighted. The default \code{NULL} does not highlight any iteration.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = self$param_vals$highlight_iter) {
      opt_path_df = as.data.frame(self$opt_state$opt.path)
      n_iters = opt_path_df[nrow(opt_path_df), "dob"]

      # read different times from the opt.path of the mbo object.
      df_time = opt_path_df[colnames(opt_path_df) %in% c("train.time", "propose.time", "exec.time")] %>%
        drop_na()
      df_time$total.time = df_time$train.time + df_time$propose.time

      if (!is.null(highlight_iter)) {
        assertMultiClass(highlight_iter, c("integer", "numeric")) #problem since iterations are sometimes of class 'numeric' even though they are (full) integers. Fix by assertMulticlass
        # prevent crash of function if user specifies iteration which is beyond the number of iterations in the object of the mbo run.
        # simply set it then to the highest iteration possible.
        if (n_iters < highlight_iter) {
          messagef("highlight_iter = %i > n_iters= %i: highlight_iter automatically set to n_iters",
                 highlight_iter, n_iters)
          highlight_iter = n_iters
        }
      }

      df_time_long = wideToLong(df_time, 0)
      df_time_long$Plot = as.factor(ifelse(df_time_long$Param == "exec.time", "Execution", "Train and Propose"))

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
      gg = gg + facet_wrap(Plot ~ ., scales = "free")

      return(gg)
    }
  )
)
