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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet()
      param_vals = list()
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Summarizes all relevant characterisitcs of the mbo run.
    #'
    #' @return (`list()`).
    getMboSummary = function() {
      infill_crit_name = assert_class(getMBOInfillCritName(self$opt_state$opt.problem$control$infill.crit),
                                 "character")
      # assert all values we want to extract from the mbo run.
      infill_crit_dir = assert_class(self$opt_state$opt.problem$control$infill.crit$opt.direction, "character")
      param_names = assert_class(names(self$opt_state$opt.path$par.set$pars), "character")
      n_objectives = assert_multi_class(self$opt_state$opt.problem$control$n.objectives, c("numeric", "integer"))
      surrogate = assert_class(self$opt_state$opt.problem$learner$name, "character")
      infill_crit_param_val = self$opt_state$opt.problem$control$infill.crit$params
      infill_crit_param_name = names(self$opt_state$opt.problem$control$infill.crit$params)
      n_iter = assert_multi_class(self$opt_state$opt.problem$control$max.evals, c("numeric", "integer"))
      time = assert_multi_class(round(as.numeric(self$opt_state$time.used)/60, digits = 2), c("numeric", "integer"))
      p_proposed = assert_multi_class(self$opt_state$opt.problem$control$propose.points, c("numeric", "integer"))
      y_minimum = round(min(data.frame(self$opt_state$opt.path)$y), digits = 3)

      # save all values with a 'name' (which is equal to the displayed name in the app summary table), the 'value' and the 'group' (for arranging table)
      infillCritOpt = list(name = "Optimization Direction",
                           value = infill_crit_dir,
                           group = 1)
      hyperParams = list(name = "Search Space",
                         value = param_names,
                         group = 2)
      noObjectives = list(name = "Number of Objectives",
                         value = n_objectives,
                         group = 2)
      surrogate = list(name = "Surrogate-Model",
                          value = surrogate,
                          group = 2)
      infillCrit = list(name = "Infill Criterion",
                        value = infill_crit_name,
                        group = 1)
      infillCrit_param_name = list(name = "Infill Criterion Parameter Name",
                                  value = infill_crit_param_name,
                                  group = 2)
      infillCrit_param_val = list(name = "Infill Criterion Parameter Value",
                       value = infill_crit_param_val,
                       group = 2)
      max_iter = list(name = "Maximum Number of Evaluations",
                      value = n_iter,
                      group = 2)
      runtime = list(name = "Runtime [Minutes]",
                      value = time,
                      group = 2)
      multi_prop = list(name = "Multi-Point Proposal",
                     value = p_proposed,
                     group = 2)
      min_y = list(name = "Minimum y",
                   value = y_minimum,
                   gorup = 2)
      # create list with the entire information
      mboSummary = list(
        infillCrit = infillCrit,
        infillCrit_param_name = infillCrit_param_name,
        infillCrit_param_val = infillCrit_param_val,
        infillCritOpt = infillCritOpt,
        surrogate = surrogate,
        hyperParams = hyperParams,
        noObjectives = noObjectives,
        multi_prop = multi_prop,
        max_iter = max_iter,
        runtime = runtime,
        min_y = min_y
      )
        return(mboSummary)
    }
  )
)
