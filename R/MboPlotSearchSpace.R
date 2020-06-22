#' @title MboPlotSearchSpace
#'
#' @include MboPlot-helpers.R
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
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @return ([ggplot]).
    plot = function() {
      df = getOptPathX(self$opt_state$opt.path)
      n = nrow(df)

      df_wide_num = df %>%
        select_if(is.numeric)
      df_wide_disc = df %>%
        select_if(is.factor)
      ncols_df = c(ncol(df_wide_num), ncol(df_wide_disc))

      df_long_num = wideToLong(df_wide_num, 0)
      df_long_disc = wideToLong(df_wide_disc, 0)

      gg_num = NULL
      gg_disc = NULL
      if (ncols_df[1] > 0) {
        gg_num = ggplot(df_long_num, aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value))
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
