#' @title MboPlotDistToNeighbor
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
#' @importFrom magrittr %T>%
#' @importFrom tidyr gather
#' @importFrom ggpubr ggarrange
#'
#' @description
#' This class generates plots for the visualization of the explore-exploit trade-pff.
#' Therefore the distance of `f(x)`
#'
#' @export


MboPlotDistToNeighbor = R6Class(
  "MboPlotDistToNeighbor",
  inherit = MboPlot,
  public = list(

    initialize = function(opt_state) {
      self$opt_path = assert_class(opt_state$opt.path, "OptPath")
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param type (`character(1)`)
    #'   Defines the information to be used for the plot and can be either `prior`, `posterior` or òverlay.
    #'   Prior uses a random desgin of the parameter set.\cr
    #'   Posterior uses the values the optimizer searched over during the mbo run.\cr
    #'   Overlay plots a combination of both, `prior` and `posterior` in a combined plot.
    #' @param plot (`chatacter(1)`)
    #'   Defines plot type. `distribution` plots the distributions of each variable in the design.
    #'   `iteration` plots the value of each variable in the design over the single iterations.
    #' @param theme ([theme|gg])
    #'   A theme to specify the ggplot default.
    #'
    #' @return ([ggplot]).
    PlotDistToNeighbor = function(desgin = FALSE) {
      if (design) {
        df = getOptPathX(self$opt_path)
      } else {
        df = data.frame(y= getOptPathY(self$opt_path))
      }
      df_colnames = colnames(df)
      df_num = extractFromDf(df, extr = c(is.numeric), keepColumNo = 0)

      list_dist = lapply(df_num, lag, n = 5L)
      df_dist = as.data.frame(list_dist, stringsAsFactors = FALSE) %>%
        drop_na()

      df_dist_long = wideToLong(df_dist, 0)
      n = nrow(df_dist_long)/length(unique(df_dist_long$Param))

      gg_dist = ggplot(df_dist_long,
                       aes(x = rep(seq(1:n), times = nrow(df_dist_long)/n),
                           y = Value))
      gg_dist = gg_dist + geom_point()
      gg_dist = gg_dist + facet_wrap(Param ~ ., scales = "free")

      return(gg_dist)
    }
  )
)

