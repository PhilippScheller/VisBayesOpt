#' @title MboPlotDependencies
#'
#' @include MboPlot-helpers-general.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import ggplot
#' @import BBmisc
#'
#' @importFrom R6 R6Class
#' @importFrom GGally ggpairs
#' @importFrom ggpubr ggarrange
#'
#' @description
#' This class generates plots for the pairwise dependencies of the features in the mbo run.
#' It only covers the numeric search space (i.e. discrete params not considered).
#'
#' @export
MboPlotDependencies = R6Class(
  "MboPlotDependencies",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeLogicalParam("color_y"))
      param_vals = list(color_y = TRUE)
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param color_y (\code{logical(1) | TRUE})\cr
    #' Specifies which dimension the color of the scatter represents. If TRUE then color corresponds to value of 'y'.
    #' If FALSE color corresponds to 'iteration'. Default is TRUE.
    #'
    #' @return ([ggplot]).
    plot = function(color_y = self$param_vals$color_y) {
      df_x = getOptPathX(self$opt_state$opt.path)
      y = data.frame(y = getOptPathY(self$opt_state$opt.path))
      iter = data.frame(self$opt_state$opt.path)$dob
      n = nrow(df_x)
      is_num = sum(sapply(df_x, is.numeric))

      df_wide_num = df_x %>%
        select_if(is.numeric)

      if (color_y) {
        df_3d = data.frame(fill_col = y)
      } else {
        df_3d = data.frame(fill_col = iter)
      }
      df = cbind.data.frame(df_3d, df_wide_num)
      df_long = wideToLong(df, 1)


      print(df_long)


      gg = ggplot2  (df_gg, aes())
      #return(gg)
    }
  )
)
