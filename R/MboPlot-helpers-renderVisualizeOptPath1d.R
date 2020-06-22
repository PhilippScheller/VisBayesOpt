renderVisualizeOptPath1d = function(opt_state, highlight_iter, densregion = TRUE, se_factor = 1L,
                                    trafo = NULL, ...) {
  par_set = opt_state$opt.path$par.set
  par_types = getParamTypes(par_set)
  n_param = sum(getParamLengths(par_set))
  names_x = names(opt_state$opt.path$par.set$pars)
  name_y = opt_state$opt.path$y.names
  control = opt_state$opt.problem$control
  noisy = control$noisy
  models = opt_state$opt.result$stored.models
  models = if (inherits(models, "WrappedModel")) list(models) else models
  design = convertToDesign(opt_state$opt.path, control)

  opt_fun = opt_state$opt.problem$fun
  mean_fun = smoof::getMeanFunction(opt_fun) # NULL if not noisy

  y_true = NA
  if (!is.null(mean_fun)) {
    y_true = vnapply(convertRowsToList(getOptPathX(opt_path), name.list = TRUE, name.vector = TRUE), mean_fun)
  }
  if (is.null(name_y)) {
    name_y = "y"
  }

  # evalauate function at 'points.per.dim' points on the given interval defined by 'par.set'.
  # returns data.frame with column 'x' and associated 'f(x)=y' values. nrows=points.per.dim, ncol = y+nx.
  if (is.null(mean_fun)) {
    evals = evaluate(opt_fun, par_set, n_param, par_types, noisy, noisy.evals = 20, points.per.dim = 50,
                     names_x, name_y)
  } else {
    evals = evaluate(mean_fun, par_set, n_param, par_types, noisy = FALSE, noisy.evals = 1, points.per.dim = 50,
                     names_x, name_y)
  }

  # determine global optimum of objective function
  if (smoof::hasGlobalOptimum(opt_fun)) {
    global_opt = smoof::getGlobalOptimum(opt_fun)$value # to be changed for noisy evals
  } else {
    global_opt = NA_real_
  }



  propose.points = control$propose.points
  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  critfun = control$infill.crit$fun

  # we need to maximize expected improvement
  opt.direction = getMBOInfillCritMultiplier(control$infill.crit)
  se = (opt_state$opt.problem$learner$predict.type == "se")
  # if no iterations provided take the total number of iterations in optimization process
  #iter = asInt(iter, lower = 0, upper = length(models))

  # if (!is.na(global_opt)) {
  #   global_opt = getGlobalOptimum(opt.fun)
  # } else {
  #   global_opt = object$global.opt.estim # to be defined
  # }

  #evals = design
  evals_x = evals[, getParamIds(par_set) , drop = FALSE]

  plots = list()

  infill.mean = makeMBOInfillCritMeanResponse()$fun
  infill.ei = makeMBOInfillCritEI()$fun
  infill.se = makeMBOInfillCritStandardError()$fun

  model = models[[highlight_iter]]
  type = vcapply(getOptPathDOB(opt_state$opt.path), getType, iter = highlight_iter)
  idx.past = type %in% c("init", "seq")
  idx.pastpresent = type %in% c("init", "seq", "prop")

  # compute model prediction for highlight_iter
  if (!inherits(model, "FailureModel")) {
    evals$yhat = ifelse(control$minimize, 1, -1) * infill.mean(evals_x, list(model), control)

    if (propose.points == 1L) {
      evals[[infill.crit.id]] = opt.direction *
        critfun(evals_x, list(model), control, par_set, list(design[idx.past, , drop = FALSE]))
    }
    # else {
    #   objective = control$multipoint.moimbo.objective
    #   if (objective == "mean.dist") {
    #     evals[[infill.crit.id]] = opt.direction * infill.mean(evals_x, list(model),
    #                                                           control, par_set, list(design[idx.past,, drop = FALSE]))
    #   } else if (objective == "ei.dist") {
    #     evals[[infill.crit.id]] = opt.direction * infill.ei(evals_x, list(model),
    #                                                         control, par_set, list(design[idx.past,, drop = FALSE]))
    #   } else if (objective %in% c("mean.se", "mean.se.dist")) {
    #     evals[[infill.crit.id]] = opt.direction * infill.mean(evals_x, list(model),
    #                                                           control, par_set, list(design[idx.past,, drop = FALSE]))
    #   }
    # }

    # prepare drawing of standard error (confidence interval)
    if (se) {
      evals$se = -infill.se(evals_x, list(model), control, par_set, list(design[idx.past,, drop = FALSE]))
    }
  }

  # create plot for numeric space
  if (isNumeric(par_set, include.int = FALSE)) {
    evals = data.table::setDT(evals)
    gg.fun = data.table::melt(evals, id.vars = c(getParamIds(par_set), if (se) "se" else NULL))
    gg.fun = data.table::setDF(gg.fun)

    if (se) gg.fun$se = gg.fun$se * se_factor

    # if trafo for y is provided, indicate transformation on the y-axis
    ylab = name_y
    # if (!is.null(trafo$y)) {
    #   ylab = paste0(name.y, " (", attr(trafo$y, "name"), "-transformed)")
    # }

    #determine in wich pane (facet_grid) the points belong to
    pane_names = c(ylab, infill.crit.id)
    gg.fun$pane = factor(pane_names[ifelse(gg.fun$variable %in% c(name_y, "yhat"), 1, 2)], levels = pane_names)


    # data frame with points of different type (initial design points, infill points, proposed points)

    gg.points = buildPointsData(opt_state$opt.path, highlight_iter, design)
    gg.points$pane = factor(pane_names[1], levels = pane_names)

    #transform y and yhat values according to trafo function
    if (!is.null(trafo$y)) {
      tr = trafo$y
      gg.fun[[name.y]] = tr(gg.fun[[name.y]])
      gg.points[[name.y]] = tr(gg.points[[name.y]])
    } else {
      tr = identity
    }

    # Build ggplot objects
    plotNumeric = ggplot(data = gg.fun)
    next.aes = aes_string(x = names_x, y = "value", linetype = "variable")
    plotNumeric = plotNumeric + geom_line(next.aes)
    plotNumeric = plotNumeric + facet_grid(pane~., scales = "free")

    if (se && densregion) {
      #FIXME: We might lose transformation information here tr()
      next.aes = aes_string(x = names_x, ymin = "value-se", ymax = "value+se")
      plotNumeric = plotNumeric + geom_ribbon(data = gg.fun[gg.fun$variable == "yhat", ], next.aes, alpha = 0.2,
                                              fill = "skyblue1")
    }
    plotNumeric = plotNumeric + geom_point(data = dplyr::filter(gg.points, !grepl("prop", type)),
                                           aes_string(x = names_x, y = name_y, colour = "type", shape = "type"))
    plotNumeric = plotNumeric + geom_vline(aes_string(xintercept =  gg.points[get("type", gg.points) == "prop", names_x]),
                                           linetype = "dashed", col = "red")
    #plotNumeric = plotNumeric + scale_colour_manual(values = colors, name = "type")
    plotNumeric = plotNumeric + scale_linetype(name = "type")
    plotNumeric = plotNumeric + theme_bw()

    # noisy should be plotted with bars (see fb post)
    if (noisy) {
      if (!anyMissing(y.true)) {
        source = data.frame(y.true)
        names(source) = name.y
        gap = calculateGap(source[idx.pastpresent, , drop = FALSE], global_opt, control)
      } else {
        gap = NA
      }
    } else {
      gap = calculateGap(design[idx.pastpresent,, drop = FALSE], global_opt, control)
    }
    plotNumeric = plotNumeric + ggtitle(sprintf("Iter = %i, Gap = %.4e", highlight_iter, gap))
    plotNumeric = plotNumeric + ylab(NULL)
    # plotNumeric = plotNumeric + theme(
    #   plot.title = element_text(size = 11, face = "bold")
    # )

    plots = list(pl.fun = plotNumeric)

  }
  if (isDiscrete(par_set)) {


    # create plot for mixed space
    if (!noisy) {
      stopf("Deterministic 1d function with a single factor parameter are not supported.")
    }

    gg.points = buildPointsData(opt_path, highlight_iter)

    if (se && densregion) {
      gg.points$se = -infill.se(gg.points[, names_x, drop = FALSE],
                                models, control, par_set, opt.path[idx.past, , drop = FALSE])
      gg.points$se.min = gg.points[[name_y]] - se_factor * gg.points$se
      gg.points$se.max = gg.points[[name_y]] + se_factor * gg.points$se
    }

    plotMixed = ggplot(data = gg.points, aes_string(x = names_x, y = name_y,
                                                    colour = "type", shape = "type"))
    plotMixed = plotMixed + geom_point()
    if (se && densregion) {
      plotMixed = plotMixed + geom_errorbar(aes_string(ymin = "se.min", ymax = "se.max"),
                                            width = .1, alpha = .5)
    }

    plotMixed = plotMixed + xlab(names_x)
    plotMixed = plotMixed + scale_colour_discrete(name = "type")
    plotMixed = plotMixed + ggtitle(
      sprintf("Iter = %i, Gap = %.4e", highlight_iter,
              calculateGap(design[idx.pastpresent,, drop = FALSE], global_opt, control))
    )

    plotMixed = plotMixed + theme_bw()
    #   theme(
    #   legend.position = "top",
    #   legend.box = "horizontal",
    #   plot.title = element_text(size = 11, face = "bold")
    # )

    plots = list(pl.fun = plotMixed)
  }

  return(plots)

}




















################################################################################################################

evaluate = function(fun, par.set, n.params, par.types, noisy, noisy.evals, points.per.dim, names.x, name.y) {
  if (!noisy && n.params == 1L && par.types == "discrete")
    stopf("ExampleRun does not make sense with a single deterministic discrete parameter.")
  if (n.params %in% c(1L, 2L) && all(par.types %in% c("numeric", "numericvector", "discrete"))) {
    return(getEvals(fun, par.set, noisy, noisy.evals, points.per.dim, names.x, name.y))
  }
}


getEvals = function(fun, par.set, noisy, noisy.evals, points.per.dim, names.x, name.y) {
  xs.trafo = generateGridDesign(par.set, points.per.dim, trafo = TRUE)
  xs.trafo.list = convertRowsToList(xs.trafo, name.list = TRUE, name.vector = TRUE)
  ys = parallelMap(function(x) {
    if (noisy) {
      mean(replicate(noisy.evals, fun(x)))
    } else {
      fun(x)
    }
  }, xs.trafo.list, level = "mlrMBO.feval", simplify = TRUE)
  evals = cbind(xs.trafo, y = ys)
  colnames(evals) = c(names.x, name.y)
  return(evals)
}

getMBOInfillCritMultiplier = function(x) {
  assertClass(x, "MBOInfillCrit")
  if (x$opt.direction == "minimize")
    return(1)
  else if (x$opt.direction == "maximize")
    return(-1)
  else
    stopf("The direction of the infill criterion is '%s' but should be 'minimize' or 'maximize' at this point.", x$opt.direction)
}

getType = function(x, iter) {
  if (x == 0)
    return("init")
  else if (x > 0 && x < iter)
    return("seq")
  else if (x == iter)
    return("prop")
  else
    return ("future")
}
calculateGap = function(design, global.opt, control) {
  best.y = if (control$minimize) min(design[, control$y.name]) else max(design[, control$y.name])
  abs(best.y - global.opt)
}

buildPointsData = function(opt.path, iter, design) {
  type = vcapply(getOptPathDOB(opt.path), getType, iter = iter)
  res = cbind.data.frame(
    design,
    type = type,
    stringsAsFactors = TRUE
  )
  res[res$type %nin% "future",]
}

convertToDesign = function(opt.path, control) {
  df = as.data.frame(opt.path, include.rest = FALSE)
  convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
}
