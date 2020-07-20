#' @title MboPlotOptPath
#'
#' @include MboPlot-helpers-general.R
#' @include MboPlot-helpers-renderVisualizeOptPath1d.R
#' @include MboPlot-helpers-renderVisualizeOptPathNd.R
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
#' @importFrom tidyr drop_na
#'
#' @description
#' This class plots the opt state for a specified iteration of the mbo run.
#'
#' @export
MboPlotOptPath = R6Class(
  "MboPlotOptPath",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeIntegerParam("highlight_iter"),
                               makeDiscreteParam("search_space_component", values = getParamIds(opt_state$opt.path$par.set)))
      param_vals = list(highlight_iter = 1L, search_space_component = NULL) # default value, else set with function `set_param_vals()`
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @field stored_pdp (`list()`)\cr
    #'   List for storing already calculated search_space_component effect object for PDPs. Helps to avoid re-calculation in case
    #'   it was already done.
    stored_pdp = NULL,
    #' @description
    #' Plots the opt state for a specified iteration of the mbo run.
    #'
    #' @param highlight_iter (\code{integer(1) | NULL})\cr
    #' Specifies the iteration to be highlighted. The default \code{NULL} does not highlight any iteration.
    #' @param search_space_component (\code{character(1) | NULL})\cr
    #' Specifies the search_space_component to be calculated in PDP for higher dimensions, i.e. number of features > 2.
    #' The default \code{NULL} is only a valid input for the 1 dimensional case.
    #' @param parallel (\code{boolean(1) | TRUE})\cr
    #' Specifies if computation of PDP is done parallel, i.e. on multiple cores. The default states that multiple
    #' cores are used.
    #' @param se_factor (\code{numeric(1) | 1})\cr
    #' Specifies the scaling factor for the uncertainty (standard error) estimate for the 1 dimensional case.
    #' The default value is that no up/or downscaling is applied.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = self$param_vals$highlight_iter, search_space_component = self$param_vals$search_space_component,
                    parallel = TRUE, se_factor = 1L) {
      trafo = NULL
      densregion = TRUE
      opt_path_df = as.data.frame(self$opt_state$opt.path)
      n_iters = max(opt_path_df$dob)
      n_init = nrow(opt_path_df) - n_iters

      # prevent crash of function if user specifies iteration which is beyond the number of iterations in the object of the mbo run.
      # simply set it then to the highest iteration possible.
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
        if (is.null(search_space_component)) stop("The mbo run has more than 1 search_space_component: please specify 'search_space_component' for partial dependance plot (PDP)")
        assertCharacter(search_space_component)
        checkChoice(search_space_component, names_x)
      }

      if (n_obj > 1L) stop("Opt path can only be visualized for single objective function")

      if (n_param == 1L) {
        if (par_types %nin% c("numeric", "numericvector", "discrete", "discretevector")) {
          stopf("For 1D function only plotting of numeric or discrete functions possible, but your function is '%s'.", par_types)
        }
        return(renderVisualizeOptPath1d(opt_state = self$opt_state, highlight_iter = highlight_iter, se_factor = se_factor,
                                        densregion = densregion, trafo = NULL))
      }
      if (n_param == 2L) {
        if (!hasNumeric(par_set)) {
          stopf("At least one parameter of the target function must be numeric!")
        }
        return(renderVisualizeOptPath2d(object, iter = iter, se.factor = se_factor,
                                        densregion = densregion, trafo = NULL))
      } else {
        if (!is.null(self$stored_pdp[[search_space_component]])) {
          plot(self$stored_pdp[[search_space_component]][[highlight_iter]])
        } else {
          # if not yet calculated (i.e. attached to object) we need re-calculate search_space_component effects for PDP
          pdp_interest = renderVisualizeOptPathNd(opt_state = self$opt_state, interest = "surrogate", feature = search_space_component,
                                                  method = "pdp", grid.size = 50, center.at = NULL, batch.size = 1000,
                                                  parallel = parallel)
          # Bind pdp_interest to R6 class object (therefore we do not need to recalculate once the same search_space_component
          # is chosen with just another iteration)
          self$stored_pdp[[search_space_component]] = pdp_interest
          # generate and return plot
          p_feat = plot(pdp_interest[[highlight_iter]])
          return(p_feat)
        }
      }
    }
  )
)
