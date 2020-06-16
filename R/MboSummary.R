#' @title MboSummary
#'
#' @include MboSummary-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#'
#' @importFrom R6 R6Class
#'
#' @description
#' This class generates a summary of all relevvant features of a mbo run
#'
#' @export
MboSummary = R6Class(
  "MboSummary",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Summarizes all relevant characterisitcs of the mbo run.
    #'
    #' @return (`list()`).
    getMboSummary = function() {
      infill_crit_name = assert_class(getMBOInfillCritName(self$opt_state$opt.problem$control$infill.crit),
                                 "character")
      infill_crit_dir = assert_class(self$opt_state$opt.problem$control$infill.crit$opt.direction, "character")
      param_names = assert_class(names(self$opt_state$opt.path$par.set$pars), "character")

      infillCrit = list(name = "Infill crit",
                        value = infill_crit_name,
                        group = 1)
      infillCritOpt = list(name = "Infill crit optim direction",
                           value = infill_crit_dir,
                           group = 1)
      hyperParams = list(name = "Search space",
                         value = param_names,
                         group = 2)

      mboSummary = list(
        infillCrit = infillCrit,
        infillCritOpt = infillCritOpt,
        hyperParams = hyperParams
      )
        return(mboSummary)
    }
  )
)
