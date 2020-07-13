#' @title MboPlotFit
#'
#' @include MboPlotFit-helpers.R
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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param opt_state ([OptState]).
    initialize = function(opt_state) {
      param_set = makeParamSet(makeIntegerParam("highlight_iter"), makeLogicalParam("predict_y_iter_surrogate"))
      param_vals = list(highlight_iter = 1L, predict_y_iter_surrogate = TRUE) # default value, else set with function `set_param_vals()`
      super$initialize(opt_state, param_set, param_vals)
    },
    #' @description
    #' Plots the fit of the model using R-squared for each iteration of the mbo run.
    #'
    #' @param highlight_iter (\code{integer(1) | NULL})\cr
    #' Specifies the iteration to be highlighted. The default \code{NULL} does not highlight any iteration.
    #' @param predict_y_iter_surrogate (\code{logical(1) | FALSE})\cr
    #' Specifies if y_hat is predicted with the surrogate from the chosen iteration. If FALSE y_hat is taken from the optimization
    #' path, i.e. predicted based on surrogate of the respective iteration. If TRUE we use the surrogate of 'highlight_iter' iteration
    #' to predict all points based on the search space x again.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = self$param_vals$highlight_iter, predict_y_iter_surrogate = self$param_vals$predict_y_iter_surrogate) {
      opt_path = self$opt_state$opt.path
      control = self$opt_state$opt.problem$control
      models = self$opt_state$opt.result$stored.models
      models = if (inherits(models, "WrappedModel")) list(models) else models
      opt_path_df = as.data.frame(opt_path)
      n_iters = opt_path_df[nrow(opt_path_df), "dob"]
      names_x = names(opt_path$par.set$pars)

      if (!is.null(highlight_iter)) {
        assertMultiClass(highlight_iter, c("integer", "numeric"))
        if (n_iters < highlight_iter) {
          messagef("highlight_iter = %i > n_iters= %i: highlight_iter automatically set to n_iters",
                 highlight_iter, n_iters)
          highlight_iter = n_iters
        }
      }
      # generate opt_path for each iteration "i" with the seen points until "i", i.e. for plot R2
      opt_path_iters = lapply(as.list(seq(1:n_iters)), function(row) opt_path_df[opt_path_df$dob != 0, ][1:row,])
      model_iters = models[1:n_iters]
      # calculate r squared
      R2 = mapply(function(model, opt_path) {
          RSQOverIterations(model, opt_path, control, names_x)
        },opt_path = opt_path_iters, model = model_iters)

      df_r2 = data.frame(R2 = R2, iter = seq(1:n_iters))

      # create data for plot y vs. yhat
      opt_path_iter = opt_path_df[opt_path_df$dob != 0, ][1:highlight_iter,]
      model_iter = models[highlight_iter]
      opt_path_x_iter = getOptPathX(opt_path, 1:highlight_iter)
      infill.mean = makeMBOInfillCritMeanResponse()$fun
      infill.std = makeMBOInfillCritStandardError()$fun

      if (predict_y_iter_surrogate) {
        y_hat = ifelse(control$minimize, 1, -1) * infill.mean(opt_path_x_iter, model_iter, control)
        y_hat_se = abs(infill.std(opt_path_x_iter, model_iter, control))
      } else {
        y_hat = opt_path_df[opt_path_df$dob != 0, "mean"] [1:highlight_iter]
        y_hat_se = opt_path_df[opt_path_df$dob != 0, "se"] [1:highlight_iter]
      }
      y_min = y_hat - y_hat_se
      y_max = y_hat + y_hat_se
      y_eval = opt_path_iter$y
      y_df = data.frame(y.hat = y_hat, y.eval = y_eval, y.absdiff = abs(y_hat - y_eval), iters = seq(1:highlight_iter),
                        y.min = y_min, y.max = y_max)

      # plot r2
      gg_r2 = ggplot(df_r2, aes(x = iter, y = R2))
      gg_r2 = gg_r2 + geom_line(na.rm = TRUE)
      gg_r2 = gg_r2 + geom_point(shape = 4, na.rm = TRUE)
      if (!is.null(highlight_iter)) {
        df_line = data.frame(x = c(highlight_iter, highlight_iter), y = c(0, Inf))
        gg_r2 = gg_r2 + geom_line(data = df_line, mapping = aes(x = x, y = y),
                                col = "black", lty = "dashed")
      }
      gg_r2 = gg_r2 + xlab("Iteration")
      gg_r2 = gg_r2 + ylab(bquote(R^2))
      gg_r2 = gg_r2 + ggtitle(bquote("In-Sample"~R^2))

      # plot y vs. yhat
      gg_cross = ggplot(y_df, aes(x = y.eval, y = y.hat, col = iters))
      gg_cross = gg_cross + geom_pointrange(aes(ymin = y.min, ymax = y.max), data = y_df)
      gg_cross = gg_cross + geom_abline(slope = 1)
      gg_cross = gg_cross + ylab(expression(hat(y)))
      gg_cross = gg_cross + xlab(expression(y))
      gg_cross = gg_cross + labs(color = "Iteration")
      gg_cross = gg_cross + ggtitle(paste("Predicted vs. True Target in Iteration", highlight_iter))

      gg = ggarrange(gg_r2, gg_cross, ncol = 2)
      return(gg)
    }
  )
)

