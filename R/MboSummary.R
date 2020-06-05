#' @title MboSummary
#'
#' @include MboSummary-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import shiny
#'
#' @importFrom R6 R6Class
#'
#' @description
#' This class generates a summary of all relevvant features of a mbo run
#'
#' @export
MboSummary = R6Class(
  "MboSummary",
  public = list(
    #' @field opt_state (`OptState`)\cr
    #'   Environment containing necessary information needed during optimization in MBO.
    opt_state = NULL,
    #' @field mbo_control ([MBOControl])\cr
    #'   Control object for mbo.
    mbo_control = NULL,
    #' @field opt_path ([OptPath])\cr
    #'   Optimization path of the mbo run.
    opt_path = NULL,
    #' @description
    #' Initializes the parameters to of object.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      self$opt_state = assert_class(opt_state, c("OptState"))
      self$mbo_control = assert_class(self$opt_state$opt.problem$control, c("MBOControl"))
      self$opt_path = assert_multi_class(self$opt_state$opt.path, c("OptPathDF", "OptPath"))
    },
    #' @description
    #' Summarizes all relevant characterisitcs of the mbo run.
    #'
    #' @param silent (`logical()`)
    #'   If TRUE, the output will only be saved in the object and not be returned.
    #'
    #' @return (`list()`).
    getMboSummary = function(silent = TRUE) {
      infill_crit_name = assert_class(getMBOInfillCritName(self$mbo_control$infill.crit),
                                 "character")
      infill_crit_dir = assert_class(self$mbo_control$infill_crit$opt.direction, "character")
      hyperparam_names = assert_class(names(self$opt_path$par.set$pars), "character")

      infillCrit = list(name = "Infill crit",
                        value = infill_crit_name,
                        group = 1)
      infillCritOpt = list(name = "Infill crit optim direction",
                           value = infill_crit_dir,
                           group = 1)
      hyperParams = list(name = "Hyperparameters",
                         value = hyperparam_names,
                         group = 2)

      self$mboSummary = list(
        infillCrit = infillCrit,
        infillCritOpt = infillCritOpt,
        hyperParams = hyperParams
      )
      if (!silent)
        return(self$mboSummary)
    }
  )
)
