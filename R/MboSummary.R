


MboSummary = R6Class(
  "MboSummary",
  public = list(
    opt_state = NULL,
    mbo_control = NULL,
    infill_crit = NULL,
    opt_path = NULL,
    mboSummary = NULL,

    initialize = function(opt_state) {
      self$opt_state = assert_class(opt_state, c("OptState"))
      self$mbo_control = assert_class(self$opt_state$opt.problem$control, c("MBOControl"))
      self$infill_crit = assert_class(self$mbo_control$infill.crit, c("MBOInfillCrit"))
      self$opt_path = assert_multi_class(self$opt_state$opt.path, c("OptPathDF", "OptPath"))

      # self$mbo_control = assert_multi_class(mbo_control, c("MBOControl"))
      # self$opt_path = assert_multi_class(opt_path, c("OptPath", "OptPathDF"))
    },
    getMboSummary = function(silent = TRUE) {
      infill_crit_name = assert_class(getMBOInfillCritName(self$infill_crit),
                                 "character")
      infill_crit_dir = assert_class(self$infill_crit$opt.direction, "character")
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
