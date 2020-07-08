#' @title MboPlotInputSpace
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
#' @importFrom dplyr select_if
#' @importFrom scales number_format
#' @importFrom ggpubr ggarrange
#'
#' @description
#' This class generates plots for the visualization of the input space given
#' prior and posterior distributions of the evaluated parameters in the mbo run.
#'
#' @export
MboPlotInputSpace = R6Class(
  "MboPlotInputSpace",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeLogicalParam("include_prior"))
      param_vals = list(include_prior = TRUE) # default value, else set with function `set_param_vals()`
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param include_prior (`boolean(1)`)
    #'   Specifies if bar chart over sampled prior should also be included in plot.
    #'
    #' @return ([ggplot]).
    plot = function(include_prior = self$param_vals$include_prior) {
      df = getOptPathX(self$opt_state$opt.path)
      n = 1000L
      df_wide_post_num = df %>%
        select_if(is.numeric)
      df_wide_post_num = cbind(type = "posterior", df_wide_post_num)

      df_wide_post_disc = df %>%
        select_if(is.factor)
      df_wide_post_disc = cbind(type = "posterior", df_wide_post_disc)

      df_wide_prior_num = generateRandomDesign(n, self$opt_state$opt.path$par.set) %>%
        select_if(is.numeric)
      df_wide_prior_num = cbind(type = "prior", df_wide_prior_num)
      df_wide_prior_disc = generateRandomDesign(n, self$opt_state$opt.path$par.set) %>%
        select_if(is.factor)
      df_wide_prior_disc = cbind(type = "prior", df_wide_prior_disc)

      df_long_num = rbind(wideToLong(df_wide_post_num), wideToLong(df_wide_prior_num))
      df_long_disc = rbind(wideToLong(df_wide_post_disc), wideToLong(df_wide_prior_disc))

      ncols_df = c(ncol(df_wide_post_num), ncol(df_wide_post_disc))

      gg_num = NULL
      gg_disc = NULL
      if (ncols_df[1] > 1) {
        gg_num = ggplot(filter(df_long_num, type == "posterior"), aes(x = Value, fill = type))
        gg_num = gg_num + geom_bar(aes(y = ..prop.., group = 1), alpha = .4)
        gg_num = gg_num + scale_x_binned(n.breaks = 20, labels = scales::number_format(accuracy = .1))
       if (include_prior) {
         gg_num = gg_num + geom_bar(data = filter(df_long_num, type == "prior"),
                                        mapping = aes(y = ..prop.., group = 1, fill = type),
                                    alpha = .4)
       }
        gg_num = gg_num + facet_wrap(Param ~ ., scales = "free")
        gg_num = gg_num + ggtitle("MBO search space: evaluated numeric parameters")
        gg_num = gg_num + xlab("Param value")
        gg_num = gg_num + theme(plot.title = element_text(face = "bold"))
      }
      if (ncols_df[2] > 1) {
        gg_disc = ggplot(filter(df_long_disc, type == "posterior"), aes(x = Value))
        gg_disc = gg_disc + geom_bar(aes(y = ..prop.., group = 2, fill = type), alpha = .4)
        if (include_prior) {
        gg_disc = gg_disc + geom_bar(data = filter(df_long_disc, type == "prior"),
                                     mapping = aes(y = ..prop.., group = 1, fill = type),
                                     alpha = .4)
        }
        gg_disc = gg_disc + facet_wrap(Param ~ ., scales = "free")
        gg_disc = gg_disc + ggtitle("MBO search space: evaluated discrete parameters")
        gg_disc = gg_disc + xlab("Param value")
        gg_disc = gg_disc + theme(plot.title = element_text(face = "bold"))
      }
      gg = ggarrange(gg_num, gg_disc, nrow = 2, heights = c(2,1))

      return(gg)
    }
  )
)

