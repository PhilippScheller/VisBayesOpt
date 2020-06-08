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
      if (!is.null(theme)) {
        theme = assert_class(theme, "theme")
      }
      rand_df = generateRandomDesign(n, self$param_set)

      rand_df_num = extractFromDf(rand_df, extr = c(is.numeric))
      rand_df_disc = extractFromDf(rand_df, extr = c(is.factor))
      ncols_df = c(ncol(rand_df_num), ncol(rand_df_disc))

      # turn of warning in pipe since 'gather()' throws warning loosing attributes of data.frame
      long_df_num = rand_df_num %T>%
          {options(warn = -1)} %>%
          gather("Param", "value", convert = TRUE, factor_key = TRUE) %T>%
        {options(warn = 0)}
      long_df_disc =  rand_df_disc %T>%
        {options(warn = -1)} %>%
          gather("Param", "value", convert = TRUE, factor_key = TRUE) %T>%
        {options(warn = 0)}

      ggnum = NULL
      ggdisc = NULL
      if (ncols_df[1] != 0) {
        ggnum = ggplot(long_df_num, aes(x = value))
        ggnum = ggnum + geom_density()
        ggnum = ggnum + facet_wrap(Param ~ ., scales = "free")
        ggnum = ggnum + ggtitle("Input space: numeric priors")
        ggnum = ggnum + theme(plot.title = element_text(face="bold"))
        ggnum = ggnum + theme
      }
      if (ncols_df[2] != 0) {
        ggdisc = ggplot(long_df_disc, aes(x = value))
        ggdisc = ggdisc + geom_bar()
        ggdisc = ggdisc + facet_wrap(Param ~ ., scales = "free")
        ggdisc = ggdisc + ggtitle("Input space: discrete priors")
        ggdisc = ggdisc + theme(plot.title = element_text(face="bold"))
        ggdisc = ggdisc + theme
      }
      gg = ggarrange(ggnum, ggdisc, nrow = 2, heights = c(2,1))

      return(gg)
    }
  )
)

