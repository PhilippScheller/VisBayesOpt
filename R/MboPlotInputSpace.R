#' @title MboPlotInputSpace
#'
#' @include MboPlot-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import shiny
#'
#' @importFrom R6 R6Class
#' @importFrom reshape2 melt
#' @importFrom magrittr %T>%
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
    #' @field param_set ([ParamSet])\cr
    #'   Object describing the parameter space of the search.
    param_set = NULL,
    #' @description
    #' Initializes the parameters of the object.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
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
    plotPrior = function(n = 10L, theme = NULL) {
      if (!is.null(theme)) theme = assert_class(theme, "theme")

      rand_df = generateRandomDesign(n, self$param_set)

      # turn of warning in pipe since 'gather()' throws warning loosing attributes of data.frame
      long_df = rand_df %T>%
        {options(warn=-1)} %>%
        gather("Param", "value", convert = TRUE, factor_key = TRUE) %T>%
        {options(warn=0)}

      g = ggplot(long_df, aes(x = value))
      g = g + geom_density()
      g = g + facet_wrap(Param ~ ., scales = "free")
      g = g + theme

      return(g)
    }
  )
)

