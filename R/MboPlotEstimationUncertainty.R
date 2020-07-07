#' @title MboPlotEstimationUncertainty
#'
#' @import checkmate
#' @import mlrMBO
#' @import ParamHelpers
#' @import BBmisc
#' @import ggplot2
#' @import grDevices
#'
#' @importFrom R6 R6Class
#'
#' @description
#' This class plots the uncertainty for one model in the mbo run.
#'
#' @export
MboPlotEstimationUncertainty = R6Class(
  "MboPlotEstimationUncertainty",
  inherit = MboPlot,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeIntegerParam("highlight_iter"))
      param_vals = list(highlight_iter = 1L) # default value, else set with function `set_param_vals()`
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots the estimation uncertainty (y_evaluated vs y_estimated) for all previous outcome values at an iteration.
    #'
    #' @param highlight_iter (\code{integer(1) | 1})\cr
    #' Specifies the iteration at which the uncertainty estimation is calculated.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = self$param_vals$highlight_iter) {
      opt_path = self$opt_state$opt.path
      control = self$opt_state$opt.problem$control
      models = self$opt_state$opt.result$stored.models
      models = if (inherits(models, "WrappedModel")) list(models) else models
      opt_path_df = as.data.frame(opt_path)
      n_iters = opt_path_df[nrow(opt_path_df), "dob"]
      names_x = names(opt_path$par.set$pars)

      if (!is.null(highlight_iter)) {
        highlight_iter = assertMultiClass(highlight_iter, c("integer", "numeric"))
        if (n_iters < highlight_iter) {
          messagef("highlight_iter = %i > n_iters= %i: highlight_iter automatically set to n_iters",
                 highlight_iter, n_iters)
          highlight_iter = n_iters
        }
      }
      # generate opt_path for the iteration "highlight_iter" with the seen points until "highlight_iter".
      opt_path_iter = opt_path_df[opt_path_df$dob != 0, ][1:highlight_iter,]
      model_iter = models[highlight_iter]
      opt_path_x_iter = getOptPathX(opt_path, 1:highlight_iter)
      infill.mean = makeMBOInfillCritMeanResponse()$fun
      infill.std = makeMBOInfillCritStandardError()$fun

      # calculate estimation of target
      y_hat = ifelse(control$minimize, 1, -1) * infill.mean(opt_path_x_iter, model_iter, control)
      y_hat_se = abs(infill.std(opt_path_x_iter, model_iter, control))
      y_min = y_hat - y_hat_se
      y_max = y_hat + y_hat_se

      y_eval = opt_path_iter$y
      y_df = data.frame(y.hat = y_hat, y.eval = y_eval, y.absdiff = abs(y_hat - y_eval), iters = seq(1:highlight_iter),
                        y.min = y_min, y.max = y_max)


      if (highlight_iter > 1) {
      gg_iter = ggplot(y_df, aes(x = iters, y = y.absdiff))
      gg_iter = gg_iter + geom_line(na.rm = TRUE)
      gg_iter = gg_iter + ylab(expression("| " * hat(y)-y *" |"))
      gg_iter = gg_iter + xlab("Iteration")
      gg_iter = gg_iter + ggtitle(paste("Uncertainty of Estimation in Iteration", highlight_iter))

      gg_dens = ggplot(y_df, aes(x = y.absdiff))
      gg_dens = gg_dens + geom_bar(na.rm = TRUE)
      gg_dens = gg_dens + scale_x_binned(n.breaks = 20, labels = scales::number_format(accuracy = .01))
      gg_dens = gg_dens + ylab(expression("count(| " * hat(y)-y *" |)"))
      gg_dens = gg_dens + xlab(expression("| " * hat(y)-y *" |"))
      gg_dens = gg_dens + ggtitle(paste("Frequency of Uncertainty-estimation in Iteration", highlight_iter))

      gg_cross = ggplot(y_df, aes(x = y.eval, y = y.hat, col = iters))
      gg_cross = gg_cross + geom_pointrange(aes(ymin = y.min, ymax = y.max), data = y_df)
      gg_cross = gg_cross + geom_abline(slope = 1)
      gg_cross = gg_cross + ylab(expression(hat(y)))
      gg_cross = gg_cross + xlab(expression(y))
      gg_cross = gg_cross + labs(color = "Iteration")
      gg_cross = gg_cross + ggtitle(paste("Predicted vs. True Target in Iteration", highlight_iter))



      gg = ggarrange(gg_iter, gg_dens, gg_cross,  ncol = 3)

      } else {
        gg = paste("Plot only available for highlight_iter > 1")
      }
      return(gg)
    }
  )
)

