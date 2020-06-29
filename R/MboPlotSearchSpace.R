#' @title MboPlotSearchSpace
#'
#' @include MboPlot-helpers-general.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import dplyr
#' @import BBmisc
#'
#' @importFrom R6 R6Class
#' @importFrom reshape2 melt
#' @importFrom magrittr %T>%
#' @importFrom tidyr gather
#' @importFrom ggpubr ggarrange
#'
#' @description
#' This class generates plots for the visualization of the input space given
#' prior and posterior distributions of the evaluated parameters in the mbo run.
#'
#' @export
MboPlotSearchSpace = R6Class(
  "MboPlotSearchSpace",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeLogicalParam("include_y"))
      param_vals = list(include_y = TRUE)
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param include_y (`boolean(1)`)
    #' Specifies if points should be coloured with y values.
    #'
    #' @return ([ggplot]).
    plot = function(include_y = self$param_vals$include_y) {
      df = getOptPathX(self$opt_state$opt.path)
      y = data.frame(y = getOptPathY(self$opt_state$opt.path))
      n = nrow(df)
      key_num = 0
      key_disc = 0

      df_wide_num = df %>%
        select_if(is.numeric)
      df_wide_disc = df %>%
        select_if(is.factor)

      if (include_y) {
        df_wide_num = cbind(df_wide_num, y)
        df_wide_disc = cbind(df_wide_disc, y)
        key_num = ncol(df_wide_num)
        key_disc = ncol(df_wide_disc)
      }
      ncols_df = c(ncol(df_wide_num), ncol(df_wide_disc))

      df_long_num = wideToLong(df_wide_num, key_num)
      df_long_disc = wideToLong(df_wide_disc, key_disc)

      gg_num = NULL
      gg_disc = NULL
      if (ncols_df[1] > 0) {
        gg_num = ggplot(df_long_num, aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value))
        if (include_y) {
          gg_num = ggplot(df_long_num, aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value, col = y))
        }
        gg_num = gg_num + geom_point(size = 0.5)
        gg_num = gg_num + geom_smooth(aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value),
                              method = "lm", formula = y~x, size=0.5)
        gg_num = gg_num + facet_wrap(Param ~ ., scales = "free")
        gg_num = gg_num + ggtitle("Mbo search space: evaluated numeric parameters")
        gg_num = gg_num + xlab("Iteration")
        gg_num = gg_num + theme(plot.title = element_text(face = "bold"))
      }
      if (ncols_df[2] > 0) {
        gg_disc = ggplot(df_long_disc, aes(x = rep(seq(1:n), times = nrow(df_long_disc)/n), y = Value))
        if (include_y) {
          gg_disc = ggplot(df_long_disc, aes(x = rep(seq(1:n), times = nrow(df_long_disc)/n), y = Value, col = y))
        }
        gg_disc = gg_disc + geom_point()
        gg_disc = gg_disc + facet_wrap(Param ~ ., scales = "free")
        gg_disc = gg_disc + ggtitle("Mbo search space: evaluated discrete parameters")
        gg_disc = gg_disc + xlab("Iteration")
        gg_disc = gg_disc + theme(plot.title = element_text(face = "bold"))
      }
      gg = ggarrange(gg_num, gg_disc, nrow = 2, heights = c(2,1))

      return(gg)
    }
  )
)
