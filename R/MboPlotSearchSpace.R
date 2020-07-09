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
      param_set = makeParamSet(makeLogicalParam("include_init_design"), makeLogicalParam("include_y"))
      param_vals = list(include_init_design = TRUE, include_y = TRUE)
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param include_y (`boolean(1)`)
    #' Specifies if points should be coloured with y values.
    #'
    #' @return ([ggplot]).
    plot = function(include_y = self$param_vals$include_y,  include_init_design = self$param_vals$include_init_design,
                    search_space_components = getParamIds(self$opt_state$opt.path$par.set)[1:2]) {
      df = data.frame(self$opt_state$opt.path)
      df_x = getOptPathX(self$opt_state$opt.path)
      df_x_comp =  df_x[, which(colnames(df_x) %in% search_space_components), drop = FALSE]
      y = data.frame(y = getOptPathY(self$opt_state$opt.path))
      n_init = nrow(df) - df[nrow(df), "dob"]
      if (!include_init_design) {
        df_x_comp = df_x_comp[(n_init+1):nrow(df_x_comp),, drop = FALSE]
        y = y[(n_init+1):nrow(y),, drop = FALSE]
      }

      length_num = sum(sapply(df_x_comp, is.numeric))
      length_disc = sum(sapply(df_x_comp, is.factor))

      n = nrow(df_x_comp)
      key_num = 0
      key_disc = 0

      df_wide_num = df_x_comp %>%
        select_if(is.numeric)
      df_wide_disc = df_x_comp %>%
        select_if(is.factor)

      if (include_y) {
        df_wide_num = cbind(df_wide_num, y)
        df_wide_disc = cbind(df_wide_disc, y)
        key_num = ncol(df_wide_num)
        key_disc = ncol(df_wide_disc)
      }
      df_long_num = wideToLong(df_wide_num, key_num)
      df_long_disc = wideToLong(df_wide_disc, key_disc)

      gg_num = NULL
      gg_disc = NULL
      if (length_num > 0) {
        gg_num = ggplot(df_long_num, aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value))
        if (include_y) {
          gg_num = ggplot(df_long_num, aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value, col = y))
        }
        gg_num = gg_num + geom_point(size = 0.5)
        gg_num = gg_num + geom_smooth(aes(x = rep(seq(1:n), times = nrow(df_long_num)/n), y = Value),
                              method = "lm", formula = y~x, size=0.5)
        gg_num = gg_num + facet_wrap(Param ~ ., scales = "free")
        gg_num = gg_num + ggtitle("MBO search space: evaluated numeric parameters")
        gg_num = gg_num + xlab(expression("Iteration " *italic(n)))
        gg_num = gg_num + ylab(expression(atop("Value of search", paste("space component")))) # note: just \n does not work since label is then outside of plot area -> use atop()
        gg_num = gg_num + theme(plot.title = element_text(face = "bold"))
      }
      if (length_disc > 0) {
        gg_disc = ggplot(df_long_disc, aes(x = rep(seq(1:n), times = nrow(df_long_disc)/n), y = Value))
        if (include_y) {
          gg_disc = ggplot(df_long_disc, aes(x = rep(seq(1:n), times = nrow(df_long_disc)/n), y = Value, col = y))
        }
        gg_disc = gg_disc + geom_point()
        gg_disc = gg_disc + facet_wrap(Param ~ ., scales = "free")
        gg_disc = gg_disc + ggtitle("MBO search space: evaluated discrete parameters")
        gg_disc = gg_disc + xlab(expression("Iteration " *italic(n)))
        gg_disc = gg_disc + ylab(expression(atop("Value of search", paste("space component")))) # note: just \n does not work since label is then outside of plot area -> use atop()
        gg_disc = gg_disc + theme(plot.title = element_text(hjust = 0.5))
      }
      print(gg_disc)
      if (length_disc < 1) {
        gg = gg_num
      } else {
        if (length_num < 1) {
          gg = gg_disc
        } else {
          gg = ggarrange(gg_num, gg_disc, nrow = ifelse(length_disc >0, 2, 1),
                         heights = ifelse(c(length_disc >0,length_disc >0) , c(round(log(length_num/length_disc)+0.51), 1)))
        }}
      return(gg)
    }
  )
)
