#' @title MboPlotDependencies
#'
#' @include MboPlot-helpers-general.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import ggplot2
#' @import BBmisc
#'
#' @importFrom R6 R6Class
#' @importFrom GGally ggpairs
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
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
      param_set = makeParamSet(makeLogicalParam("color_y"))#,
                               #makeCharacterVectorParam("search_space_components", values = getParamIds(opt_state$opt.path$par.set)))
      param_vals = list(color_y = TRUE)#, search_space_components = getParamIds(opt_state$opt.path$par.set)[1:2]) # we choose only the first 2 params by default to save time when plot is called first
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param color_y (\code{logical(1) | TRUE})\cr
    #' Specifies which dimension the color of the scatter represents. If TRUE then color corresponds to value of 'y'.
    #' If FALSE color corresponds to 'iteration'. Default is TRUE.
    #' @param search_space_components (\code{list()})\cr
    #' Specifies the search space components which should be plotted.
    #'
    #' @return ([ggplot]).
    plot = function(color_y = self$param_vals$color_y, search_space_components = getParamIds(self$opt_state$opt.path$par.set)[1:2]) { #, search_space_components = self$search_space_components)
      df_x = getOptPathX(self$opt_state$opt.path)
      df_x_comp =  df_x[, which(colnames(df_x) %in% search_space_components), drop = FALSE]
      y = getOptPathY(self$opt_state$opt.path)
      if (self$opt_state$opt.problem$control$minimize) {
        y_best_index = which.min(y)
      } else {
        y_best_index = which.max(y)
      }

      iter = data.frame(self$opt_state$opt.path)$dob #'iter' is number of columns- initial points.
      n = nrow(df_x_comp)
      is_num = sum(sapply(df_x_comp, is.numeric)) #if any numeric in x-space then 'is_num' != 0.

      if (color_y) {
        df_3d = data.frame(fill_col = y) # do not change 'fill_col' as name or also change in helpers file
        legend_title = "y"
      } else {
        df_3d = data.frame(fill_col = iter) # do not change 'fill_col' as name or also change in helpers file
        legend_title = "Iteration"
      }
      df = cbind.data.frame(df_3d, df_x_comp)
      gg_list = create_gg_combinations_scatter(df, legend_title, y_best_index)

      # plot all objects and set some layout options
      gg = ggarrange(plotlist = gg_list, common.legend = TRUE, legend = "right")
      gg = annotate_figure(gg, top = text_grob("Pairwise Dependencies of Search Space Components"))

      return(gg)
    }
  )
)
