#' @title MboPlotInputSpace
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
MboPlotInputSpace = R6Class(
  "MboPlotInputSpace",
  inherit = MboPlot,
  public = list(
    #' @field opt_path ([OptPath])\cr
    #'   Optimization path of the mbo run.
    opt_path = NULL,
    #' @field param_set ([ParamSet])\cr
    #'   Object describing the parameter space of the search.
    param_set = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      self$opt_path = assert_class(opt_state$opt.path, "OptPath")
      self$param_set = assert_class(opt_state$opt.path$par.set, "ParamSet")
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
    plotInputSpace = function(type = c("prior", "posterior", "overlay"), plot = c("distribution", "iteration"),
                              theme = NULL) {
      if (!is.null(theme)) {
        theme = assert_class(theme, "theme")
      }
      if (type %in% c("prior", "posterior", "overlay") & length(type) == 1) {
        type = assert_class(type, "character")
      } else {
        stop("`type` must be of length 1 and can only take values `prior`, `posterior` or òverlay`")
      }
      if (plot %in% c("distribution", "iteration") & length(plot) == 1) {
        plot = assert_class(plot, "character")
      } else {
        stop("`plot` must be of length 1 and can only take values `distribution` or `iteration`")
      }

      n = nrow(getOptPathX(self$opt_path))
      df_prior = cbind(data.frame("prior", stringsAsFactors = FALSE),
                       generateRandomDesign(n, self$param_set))
      df_posterior = cbind(data.frame("posterior", stringsAsFactors = FALSE),
                           getOptPathX(self$opt_path))
      df = list(prior = df_prior, posterior = df_posterior)

      df_wide_num = lapply(df, extractFromDf, extr = c(is.numeric))
      df_wide_disc = lapply(df, extractFromDf, extr = c(is.factor))
      ncols_df = c(ncol(df_wide_num[[1]]), ncol(df_wide_disc[[1]]))

      df_long_num = lapply(df_wide_num, wideToLong)
      df_long_disc = lapply(df_wide_disc, wideToLong)

      ggnum = NULL
      ggdisc = NULL
      if (ncols_df[1] > 1) {
        if (plot %in% c("distribution")) {
          ggnum = wrappedPlot(df_long_num, "Input space: numeric priors as density",
                              "numeric", type, plot, n, theme)
        } else {
          ggnum = wrappedPlot(df_long_num, "Input space: numeric priors over iterations",
                              "numeric", type, plot, n, theme)
        }
      }
      if (ncols_df[2] > 1) {
        if (plot %in% c("distribution")) {
          ggdisc = wrappedPlot(df_long_disc, "Input space: discrete priors as density",
                             "discrete", type, n, plot, theme)
        } else {
          ggdisc = wrappedPlot(df_long_disc, "Input space: discrete priors over iterations",
                             "discrete", type, n, plot, theme)
        }
      }
      gg = ggarrange(ggnum, ggdisc, nrow = 2, heights = c(2,1))

      return(gg)
    }
  )
)

