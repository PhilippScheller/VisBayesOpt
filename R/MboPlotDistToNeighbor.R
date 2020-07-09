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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeDiscreteParam("dist_measure", c("min", "max", "mean")),
                               makeLogicalParam("include_init_design"))
      param_vals = list(dist_measure = "min", include_init_design = TRUE) # default value, else set with function `set_param_vals()`
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param dist_measure (\code{character(1) | "min"})\cr
    #'   Defines the distance measure which is considered for the determination of the distance between
    #'   the search space componentens. `min` takes the minimum of the `p` componentents, while `max` uses
    #'   maximum distance and `mean` calculates arithmetic mean distance. The default is `min`.
    #'   @param include_init_design (\code{logical(1) | TRUE})\cr
    #'   Decide whether to include to points from the initial design. If `TRUE` the initial design points
    #'   are separated by a vertical line. If `FALSE` they are not plotted. Default is `TRUE`.
    #'
    #' @return ([ggplot]).
    plot = function(dist_measure = self$param_vals$dist_measure, include_init_design = self$param_vals$include_init_design) {
      if (length(dist_measure) != 1L) stop("Only 1 distance measure can be calculated.")
      dist_measure = assert_class(dist_measure, "character")
      if (!check_function(get(dist_measure))) stop("Chosen `dist_measure` cannot be evaluated as a function")

      df = data.frame(self$opt_state$opt.path)
      n_init = nrow(df) - df[nrow(df), "dob"]
      df_x = as.data.frame(getOptPathX(self$opt_state$opt.path))
      df_colnames = colnames(df_x)

      # create matrix with gower distances, extract lower triangular matrix
      # and calculate the distance over each row (iteration)
        mat_dist = as.matrix(daisy(df_x, metric = "gower"))
        mat_dist_lower = mat_dist * lower.tri(mat_dist)
        mat_dist_measure = apply(mat_dist_lower[2:nrow(mat_dist_lower), ], 1, function(x) {
          suppressWarnings(get(dist_measure)(x[x > 0]))
        })
        df_dist_measure = as.data.frame(mat_dist_measure)
        names(df_dist_measure) = "Value"

      if (!include_init_design) {
        df_dist_measure = df_dist_measure[(n_init+1):nrow(df_dist_measure),, drop = FALSE]
      }

      gg_dist = ggplot(df_dist_measure, aes(x = seq(1:nrow(df_dist_measure)), y = Value))
      if (include_init_design) {
        gg_dist = gg_dist + geom_vline(xintercept = n_init, col = "black", lty = "dashed")
      }
      gg_dist = gg_dist + geom_point(shape = 4)
      gg_dist = gg_dist + geom_line()
      gg_dist = gg_dist + ggtitle(paste0(dist_measure, " distance of search space"))
      gg_dist = gg_dist + xlab("Iteration")
      gg_dist = gg_dist + ylab("Gower distance")
      gg_dist = gg_dist + theme(plot.title = element_text(face = "bold"))

      return(gg_dist)
    }
  )
)
