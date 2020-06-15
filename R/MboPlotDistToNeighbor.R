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
    plotDistToNeighbor = function(dist_measure = c("min", "max", "mean"), k = 1L, theme = NULL) {
      if (length(dist_measure) != 1L) stop("Only 1 distance measure can be calculated.")
      dist_measure = assert_class(dist_measure, "character")
      if (!check_function(get(dist_measure))) stop("Chosen `dist_measure` cannot be evaluated as a function")
      if (!is.null(theme)) {
        theme = assert_class(theme, "theme")
      }

      df = as.data.frame(getOptPathY(self$opt_path))
      df_colnames = colnames(df)
      df_num = extractFromDf(df, extr = c(is.numeric), keepColumNo = 0)
      # create matrix with euclidean distances
      dist = dist(df_num)
      mat_dist = as.matrix(dist)
      # extract lower triangular matrix and take the `k` rows below the diagonal
      mat_dist_lower = mat_dist * lower.tri(mat_dist, FALSE)
      mat_k_general = row(mat_dist_lower) - col(mat_dist_lower)
      mat_k = (mat_k_general > 0) & (mat_k_general <= k)
      mat_dist_lower_k = mat_dist_lower * mat_k
      # calculate the distance over each row (iteration) considering the `k` previous iterations
      mat_dist_measure = apply(mat_dist_lower_k[2:nrow(mat_dist_lower_k), ], 1, function(x) {
        suppressWarnings(get(dist_measure)(x[x > 0]))
      })
      df_dist_measure = as.data.frame(mat_dist_measure) %>%
        drop_na
      names(df_dist_measure) = "Value"

      gg_dist = ggplot(df_dist_measure, aes(x = seq(1:nrow(df_dist_measure)), y = Value))
      gg_dist = gg_dist + geom_point(shape = 4)
      gg_dist = gg_dist + geom_line()
      gg_dist = gg_dist + ggtitle(paste0(dist_measure, " distance to ", k, " nearest neigbors"))
      gg_dist = gg_dist + xlab("Iteration")
      gg_dist = gg_dist + ylab("Euclidean distance")
      gg_dist = gg_dist + theme

      return(gg_dist)
    }
  )
)
