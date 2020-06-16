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
    #' @param k (`integer(1)`)
    #'   Defines the number of neighbors conisdered for the difference calculation. For e.g. `k=1` only the
    #'   difference to the previous value is considered (i.e. lagged value) while `k>1` enables to consider
    #'   the distance to the next `k` neighbors.
    #' @param theme ([theme|gg])
    #'   A theme to specify the ggplot default.
    #'
    #' @return ([ggplot]).
    plotDistToNeighbor = function(dist_measure = c("min", "max", "mean"), theme = NULL) {
      if (length(dist_measure) != 1L) stop("Only 1 distance measure can be calculated.")
      dist_measure = assert_class(dist_measure, "character")
      if (!check_function(get(dist_measure))) stop("Chosen `dist_measure` cannot be evaluated as a function")
      if (!is.null(theme)) {
        theme = assert_class(theme, "theme")
      }

      df = as.data.frame(getOptPathX(self$opt_state$opt.path))
      df_colnames = colnames(df)
      df_num = as.matrix(extractFromDf(df, extr = c(is.numeric), keepColumNo = 0))
      df_disc = extractFromDf(df, extr = c(is.factor), keepColumNo = 0)
      # create matrix with euclidean/gower distances
      mat_dist_num = dist(df_num)
      mat_dist_disc = daisy(df_disc, metric = "gower")
      # extract lower triangular matrix
      mat_dist_num_lower = mat_dist_num * lower.tri(mat_dist_num)
      mat_dist_disc_lower = df * lower.tri(mat_dist_disc)

      # calculate the distance over each row (iteration)
      mat_dist_num_measure = apply(mat_dist_num_lower[2:nrow(mat_dist_num_lower), ], 1, function(x) {
        suppressWarnings(get(dist_measure)(x[x > 0]))
      })
      mat_dist_num_measure = apply(mat_dist_num_lower[2:nrow(mat_dist_num_lower), ], 1, function(x) {
        suppressWarnings(get(dist_measure)(x[x > 0]))
      })
      df_dist_num_measure = as.data.frame(mat_dist_num_measure)
      names(df_dist_num_measure) = "Value"

      gg_dist = ggplot(df_dist_num_measure, aes(x = seq(1:nrow(df_dist_num_measure)), y = Value))
      gg_dist = gg_dist + geom_point(shape = 4)
      gg_dist = gg_dist + geom_line()
      gg_dist = gg_dist + ggtitle(paste0(dist_measure, " distance"))
      gg_dist = gg_dist + xlab("Iteration")
      gg_dist = gg_dist + ylab("Euclidean distance")
      gg_dist = gg_dist + theme

      return(gg_dist)
    }
  )
)
