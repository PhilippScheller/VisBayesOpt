#' @title MboPlotOptPath
#'
#' @include MboPlot-helpers.R
#' @include MboPlot-helpers-RenderVisualizeOptPath1d.R
#' @include MboPlot-helpers-RenderVisualizeOptPathNd.R
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import BBmisc
#' @import ggplot2
#' @import data.table
#'
#' @importFrom R6 R6Class
#' @importFrom parallelMap parallelMap
#'
#' @description
#' This class plots the opt state for a specified iteration of the mbo run.
#'
#' @export
MboPlotOptPath = R6Class(
  "MboPlotOptPath",
  inherit = MboPlot,
  public = list(
    #' @field stored_pdp (`list()`)\cr
    #'   List for storing already calculated feature effect object for PDPs. Helps to avoid re-calculation in case
    #'   it was already done.
    stored_pdp = NULL,
    #' @description
    #' Plots the opt state for a specified iteration of the mbo run.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = 1L, feature = NULL, densregion = TRUE, parallel = TRUE, se_factor = 1L,
                    trafo = NULL, ...) {

      opt_path_df = as.data.frame(self$opt_state$opt.path)
      n_iters = max(opt_path_df$dob)
      n_init = nrow(opt_path_df) - n_iters

      if (n_iters < highlight_iter) {
        messagef("highlight_iter = %i > n_iters= %i: highlight_iter automatically set to n_iters",
                 highlight_iter, n_iters)
        highlight_iter = n_iters
      }

      assertFlag(densregion)
      assertNumber(se_factor, lower = 0)
      par_set = self$opt_state$opt.path$par.set
      names_x = names(par_set$pars)
      par_types = getParamTypes(par_set)
      n_param = sum(getParamLengths(par_set))
      n_obj = self$opt_state$opt.problem$control$n.objectives

      if (n_param > 1L) {
        if (is.null(feature)) stop("The mbo run has more than 1 feature: please specify 'feature' for partial dependance plot (PDP)")
        assertCharacter(feature)
        checkChoice(feature, names_x)
      }

      if (n_obj > 1L) stop("Opt path can only be visualized for single objective function")

      if (n_param == 1L) {
        if (par_types %nin% c("numeric", "numericvector", "discrete", "discretevector")) {
          stopf("For 1D function only plotting of numeric or discrete functions possible, but your function is '%s'.", par_types)
        }
        return(renderVisualizeOptPath1d(opt_state = self$opt_state, highlight_iter = highlight_iter, se_factor = se_factor,
                                        densregion = densregion, trafo = NULL, ...))
      }
      if (n_param == 2L) {
        if (!hasNumeric(par_set)) {
          stopf("At least one parameter of the target function must be numeric!")
        }
        return(renderVisualizeOptPath2d(object, iter = iter, se.factor = se_factor,
                                        densregion = densregion, trafo = NULL, ...))
      } else {
        if (!is.null(self$stored_pdp[[feature]])) {
          plot(self$stored_pdp[[feature]][[highlight_iter]])
        } else {
          # if not yet calculated (i.e. attached to object) we need re-calculate feature effects for PDP
          pdp_interest = renderVisualizeOptPathNd(opt_state = self$opt_state, interest = "surrogate", feature = feature,
                                                      method = "pdp", grid.size = 50, center.at = NULL, batch.size = 1000,
                                                      parallel = parallel)
          # Bind pdp_interest to R6 class object (therefore we do not need to recalculate once the same feature
          # is chosen with just another iteration)
          self$stored_pdp[[feature]] = pdp_interest
          # generate and return plot
          p_feat = plot(pdp_interest[[highlight_iter]])
          return(p_feat)
        }
      }
    }
  )
)
