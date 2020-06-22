#' @title MboPlotDistToNeighbor
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
#' @importFrom cluster daisy
#' @importFrom stats dist
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
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param dist_measure (`character(1)`)
    #'   Defines the distance measure which is considered for the determination of the distance between the
    #'   difference between the values of `f(x)`. `min` takes the minimum of the `k` calculated distances to
    #'   the neighbors while `max` uses maximum distance and `mean` calculates arithmetic mean distance.
    #'
    #' @return ([ggplot]).
    plot = function(dist_measure = c("min", "max", "mean")) {
      if (length(dist_measure) != 1L) stop("Only 1 distance measure can be calculated.")
      dist_measure = assert_class(dist_measure, "character")
      if (!check_function(get(dist_measure))) stop("Chosen `dist_measure` cannot be evaluated as a function")

      df = as.data.frame(getOptPathX(self$opt_state$opt.path))
      df_colnames = colnames(df)
      df_num = as.matrix(extractFromDf(df, extr = c(is.numeric), keepColumNo = 0))
      df_disc = extractFromDf(df, extr = c(is.factor), keepColumNo = 0)

      num_present = ifelse(ncol(df_num) > 0, TRUE, FALSE)
      disc_present = ifelse(ncol(df_disc) > 0, TRUE, FALSE)

      # create matrix with euclidean/gower distances, extract lower triangular matrix
      # and calculate the distance over each row (iteration)
      if (num_present) {
        mat_dist_num = as.matrix(dist(df_num))
        mat_dist_num_lower = mat_dist_num * lower.tri(mat_dist_num)
        mat_dist_num_measure = apply(mat_dist_num_lower[2:nrow(mat_dist_num_lower), ], 1, function(x) {
          suppressWarnings(get(dist_measure)(x[x > 0]))
        })
        df_dist_num_measure = as.data.frame(mat_dist_num_measure)
          names(df_dist_num_measure) = "Value"
      }
      if (disc_present) {
        mat_dist_disc = as.matrix(daisy(df_disc, metric = "gower"))
        mat_dist_disc_lower = mat_dist_disc * lower.tri(mat_dist_disc)
        mat_dist_disc_measure = apply(mat_dist_disc_lower[2:nrow(mat_dist_disc_lower), ], 1, function(x) {
          suppressWarnings(get(dist_measure)(x[x > 0]))
        })
        df_dist_disc_measure = as.data.frame(mat_dist_disc_measure)
        names(df_dist_disc_measure) = "Value"
      }
# TODO: combine gower and euclidean distances.

      gg_dist = ggplot(df_dist_num_measure, aes(x = seq(1:nrow(df_dist_num_measure)), y = Value))
      gg_dist = gg_dist + geom_point(shape = 4)
      gg_dist = gg_dist + geom_line()
      gg_dist = gg_dist + ggtitle(paste0(dist_measure, " distance"))
      gg_dist = gg_dist + xlab("Iteration")
      gg_dist = gg_dist + ylab("Euclidean distance")

      return(gg_dist)
    }
  )
)
