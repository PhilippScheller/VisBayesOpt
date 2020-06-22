#' @title MboPlotFit
#'
#' @include MboPlot-helpers.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import BBmisc
#' @import ggplot2
#'
#' @importFrom R6 R6Class
#'
#' @description
#' This class plots the quality of the single models fit in the mbo run.
#'
#' @export
MboPlotFit = R6Class(
  "MboPlotFit",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Plots the fit of the model at a specified iteration in the mbo run.
    #'
    #' @return ([ggplot]).
    plot = function() {

      opt_path = self$opt_state$opt.path
      opt_fun = self$opt_state$opt.problem$fun
      par_set = opt_state$opt.path$par.set
      mean_fun = smoof::getMeanFunction(opt_fun) # NULL if not noisy
      control = self$opt_state$opt.problem$control

      if (!is.null(mean_fun)) {
        y_true = vnapply(convertRowsToList(getOptPathX(opt_path), name.list = TRUE, name.vector = TRUE), mean_fun)
      }
      if (is.null(name_y)) {
        name_y = "y"
      }
      if (is.null(mean_fun)) {
        evals = evaluate(opt_fun, par_set, n_param, par_types, noisy, noisy.evals = 20, points.per.dim = 50,
                         names_x, name_y)
      } else {
        evals = evaluate(mean_fun, par_set, n_param, par_types, noisy = FALSE, noisy.evals = 1, points.per.dim = 50,
                         names_x, name_y)
      }
      evals_x = evals[, getParamIds(par_set) , drop = FALSE]

      infill.mean = makeMBOInfillCritMeanResponse()$fun
      y_hat = ifelse(control$minimize, 1, -1) * infill.mean(evals_x, list(model), control)


      print(y_true)













      ###########################################################################################################################

      # opt_path_df = as.data.frame(self$opt_state$opt.path)
      # n_iters = max(opt_path_df$dob)
      # n_init = nrow(opt_path_df) - n_iters
      #
      # if (n_iters < highlight_iter) {
      #   messagef("highlight_iter = %i > n_iters= %i: highlight_iter automatically set to n_iters",
      #            highlight_iter, n_iters)
      #   highlight_iter = n_iters
      # }
      #
      # assertFlag(densregion)
      # assertNumber(se_factor, lower = 0)
      # par_set = self$opt_state$opt.path$par.set
      # names_x = names(par_set$pars)
      # par_types = getParamTypes(par_set)
      # n_param = sum(getParamLengths(par_set))
      # n_obj = self$opt_state$opt.problem$control$n.objectives


    }
  )
)
