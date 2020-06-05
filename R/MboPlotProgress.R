#' @export
MboPlotProgress = R6Class(
  "MboPlotProgress",
  inherit = MboPlot,
  public = list(
    initialize = function(opt_state) {
      param_set = makeParamSet(makeIntegerParam("max_iter", lower = 0, upper = opt_state$loop))
      self$param_vals = list(max_iter = opt_state$loop)
      control = super$control
      super$initialize(opt_state)
    },
    plot = function() {
      opdf = as.data.frame(self$opt_path)
      opdf$cumy = cummin(opdf$y) #cummax if max problem
      opdf = opdf[opdf$dob <= self$param_vals$max_iter,]

      g = ggplot(opdf, aes(x = dob, y = cumy))
      g = g + geom_line()
      return(g)
    }
  )
)
#######
# Example:
# mboObj1 = readRDS("../test-data/mboObj.RData")
#
# plot_progress = mboPlotProgress$new(mboObj1$final.opt.state)
# plot_progress$plot()
# plot_progress$generateParamUiShiny()




