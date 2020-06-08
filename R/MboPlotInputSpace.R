#' @title MboPlotInputSpace
#'
#' @include MboPlot-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import dplyr
#'
#' @importFrom R6 R6Class
#' @importFrom reshape2 melt
#' @importFrom magrittr %T>%
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
    #' Initializes the parameters of the object.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      self$opt_path = assert_class(opt_state$opt.path, "OptPath")
      self$param_set = assert_class(opt_state$opt.path$par.set, "ParamSet")
    },
    #' @description
    #' Plots prior distributions of mbo run specified in the set of parameters.
    #'
    #' @param n (`integer()`)
    #'   Defines the number of random samples generated from each parameter in the
    #'   parameter set.
    #' @param theme ([theme|gg])
    #'   A theme to specify the ggplot default.
    #'
    #' @return ([ggplot]).
    plotInputSpace = function(type = c("prior", "posterior", "overlay"), theme = NULL) {
      if (!is.null(theme)) {
        theme = assert_class(theme, "theme")
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
      if (ncols_df[1] != 0) {
        ggnum = plotWrappedDens(df_long_num, "Input space: numeric priors", "numeric", type)
      }
      if (ncols_df[2] != 0) {
        ggdisc = plotWrappedDens(df_long_disc, "Input space: discrete priors", "discrete", type)
      }
      gg = ggarrange(ggnum, ggdisc, nrow = 2, heights = c(2,1))

      return(gg)
    }

    # plotPosterior = function(theme = NULL) {
    #   if (!is.null(theme)) {
    #     theme = assert_class(theme, "theme")
    #   }
    #   df = getOptPathX(self$opt_path)
    #
    #   df_wide_num = extractFromDf(df, extr = c(is.numeric))
    #   df_wide_disc = extractFromDf(df, extr = c(is.factor))
    #   ncols_df = c(ncol(df_wide_num), ncol(df_wide_disc))
    #
    #   df_long_num = wideToLong(df_wide_num)
    #   df_long_disc = wideToLong(df_wide_disc)
    #
    #   ggnum = NULL
    #   ggdisc = NULL
    #   if (ncols_df[1] != 0) {
    #     ggnum = plotWrappedDens(df_long_num, "Input space: numeric posteriors", "numeric")
    #   }
    #   if (ncols_df[2] != 0) {
    #     ggdisc = plotWrappedDens(df_long_disc, "Input space: discrete posteriors", "discrete")
    #   }
    #   gg = ggarrange(ggnum, ggdisc, nrow = 2, heights = c(2,1))
    #
    #   return(gg)
    # },
    #
    # plotOverlay = function(theme = NULL) {
    #
    # }
  )
)

