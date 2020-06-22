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
    #' Plots the fit of the model using R-squared for each iteration of the mbo run.
    #'
    #' @param highlight_iter (\code{integer(1) | NULL})\cr
    #' Specifies the iteration to be highlighted. The default \code{NULL} does not highlight any iteration.
    #'
    #' @return ([ggplot]).
    plot = function(highlight_iter = NULL) {
      opt_path = self$opt_state$opt.path
      control = self$opt_state$opt.problem$control
      models = self$opt_state$opt.result$stored.models
      models = if (inherits(models, "WrappedModel")) list(models) else models
      opt_path_df = as.data.frame(opt_path)
      niter = opt_path_df[nrow(opt_path_df), "dob"]
      names_x = names(opt_path$par.set$pars)

      if (!is.null(highlight_iter)) highlight_iter = assertMultiClass(highlight_iter, c("integer", "numeric"))
      if (!is.null(highlight_iter)) {
        if(highlight_iter < 0 | highlight_iter > niter) {
          stop("`highlight_iter` exceeds number of iterations from mbo run (n=", niter, ")")
        }
      }
      # generate opt_path for each iteration "i" with the seen points until "i". models are
      opt_path_iters = lapply(as.list(seq(1:niter)), function(row) opt_path_df[opt_path_df$dob != 0, ][1:row,])
      model_iters = models[1:niter]
      # calculate r squared
      R2 = mapply(function(model, opt_path) {
          RSQOverIterations(model, opt_path, control, names_x)
        },opt_path = opt_path_iters, model = model_iters)

      df_r2 = data.frame(R2 = R2, iter = seq(1:niter))

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
      return(gg_r2)
    }
  )
)

