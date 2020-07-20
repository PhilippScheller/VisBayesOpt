# Function for plotting the surrogate of higher dimensional search spaces as PDP
renderVisualizeOptPathNd = function(opt_state, interest = "surrogate", feature, method = "pdp", grid.size = 20,
                                    center.at = NULL, batch.size = 1000, parallel = TRUE) {

  # general setup
  opt_path_df =  as.data.frame(opt_state$opt.path)
  design = opt_state$opt.problem$design
  sm = opt_state$opt.result$stored.models
  control = opt_state$opt.problem$control
  par_set = opt_state$opt.path$par.set
  critfun = control$infill.crit$fun

  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  name_y = opt_state$opt.path$y.names
  names_x = names(opt_state$opt.path$par.set$pars)
  # iters are the evaluated iterations (i.e. excluding initial design points)
  iters = opt_path_df[nrow(opt_path_df), "dob"]

  # model_seq are the stored models in the object for the single iterations.
  model_seq = sort(as.integer(names(opt_state$opt.result$stored.models)))

  assertChoice(interest, c("surrogate", "acquisition"))
  assertChoice(method, c("ale", "pdp", "ice", "pdp+ice"))
  checkmate::assertLogical(parallel, len = 1, any.missing = FALSE)
  if (is.character(feature)) {
    assertCharacter(feature, max.len = 2, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
    assertSubset(feature, names_x)
  } else {
    assertNumeric(feature, lower = 1, upper = length(names_x), any.missing = FALSE,
                  min.len = 1, max.len = 2)
  }

  assertInteger(model_seq, lower = 1, any.missing = FALSE, min.len = 1, max.len = iters + 1, sorted = TRUE,
                unique = TRUE #mbo automatically saves unique models, e.g. s.m.a = c(1,2,2,5)
                #models 1,2,5 are model_seq. it also rm NAs automatically, e.g. s.m.a = c(1,2,NA)
  )
  # stops if multi point proposal or multiobjective target fun
  if (control$propose.points > 1 | control$n.objectives > 1) {
    stop("This plot is not available for multi-point proposals and multi-objective functions.")
  }
  # stops if infill is not one of the seven built-in infill in mlrMBO
  if (!(infill.crit.id %in% c("mean", "se", "ei", "cb", "eqi", "aei", "adacb")))
    stop("inflInst only implemented for the seven built-in single obj. infill crits")
  # if clauses to guarantee that to stop if
  if (length(model_seq) == 1 & max(model_seq) == iters + 1 & interest == "acquisition") {
    #1.case
    stop(paste0("Only the final model (default) has been model_seq.There are no seen points in iter ", model_seq,
                " because the MBO has terminated after iter ", model_seq - 1, ". Please use interest = surrogate if you want to analyse the final model or run the MBO with others store.model.at")
    )
  }
  if (length(model_seq) > 1 & max(model_seq) == iters + 1 & interest == "acquisition") {
    #2.case
    # we need to remove the last model, since there are no seen.points
    model_seq = model_seq[-length(model_seq)]
  }

  # if clauses to guarantee that it works with "seen.points" if model_seq, but also if not model_seq
  if (interest != "acquisition") seen.points = NULL
  if (interest == "acquisition" & !(opt_state$opt.result$mbo.result$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts"))) {
    stop("Seen points have not been model_seq. Use interest = surrogate or run the MBO again with
         Savepts infill.opt")
  }
  if (interest == "acquisition" & opt_state$opt.result$mbo.result$control$infill.opt %in% c("focussearchSavepts", "cmaesSavepts", "eaSavepts")) {
    # seen points is a list
    #seen.points = opt_state$opt.result$mbo.result$seen.points
    #assertList(seen.points, any.missing = FALSE, len = iters)
    # NOTE:- works if model_seq seen points in res.mbo do not have AF value
    #      - we care about the seen points, for which we model_seq the surrogate models, others are useless
    seen.points <- lapply(model_seq, function(x) {
      assertDataFrame(seen.points[[x]], all.missing = FALSE, any.missing = FALSE)
      ic = critfun(
        points = seen.points[[x]],
        models = list(sm[[as.character(x)]]),
        control = control.mbo,
        par.set = par_set,
        designs = list(opt_path_df[1:(nrow(design) + x - 1), c(names_x, name_y)]),
        iter = x,
        progress = getProgressAdaCB(res.mbo = opt_state$opt.result$mbo.result, iter = x),
        attributes = FALSE
      )
      df = cbind(seen.points[[x]], ic)
      df = data.table::data.table(df)
      data.table::setnames(df, "ic", infill.crit.id)
      df = as.data.frame(df)
    })
    names(seen.points) = model_seq
  }
  # in order to use parLapply and interest we need to use lists. Convert the designs
  # of each iter in a list
  designs = lapply(model_seq, function(x){
    opt_path_df[1:(nrow(design) + x - 1), c(names_x, name_y)]
  })
  names(designs) = model_seq

  if (parallel) {

    # detect the number of cores & initiate cluster
    no.cores = parallel::detectCores() - 1
    cl = parallel::makeCluster(no.cores) # use default type = "PSOCK" since fork does not work on windows which might limit some users, type = "FORK")

    if (interest == "surrogate") {
      result = parallel::parLapply(
        cl,
        model_seq,
        function(x) {
          getFeatureEffectMBO(
            model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = name_y,
            class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # close the cluster
      parallel::stopCluster(cl)
    }

    if (interest == "acquisition") {
      print(seen.points)
      result = parallel::parLapply(
        cl,
        model_seq,
        function(x) {
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.crit.id,
            batch.size.p = batch.size, res.mbo.p = opt_state$opt.result$mbo.result, design.p = designs[[as.character(x)]],
            iter.p = x, feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      # close the cluster
      parallel::stopCluster(cl)
    }
  } else {
    # results, depends on the interest
    if (interest == "surrogate") {
      result = lapply(model_seq, function(x) {
                getFeatureEffectMBO(
                  model.p = sm[[as.character(x)]], data.p = designs[[as.character(x)]], y.p = name_y,
                  class.p = NULL, predict.fun.p = NULL, type.p = NULL, batch.size.p = batch.size,
                  feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
                )
                }
              )
      names(result) = model_seq
    }
    if (interest == "acquisition") {
      result = lapply(
        model_seq,
        function(x) {
          getFeatureEffectAfMBO(
            model.p = sm[[as.character(x)]], data.p = seen.points[[as.character(x)]], y.p = infill.crit.id, batch.size.p = batch.size,
            res.mbo.p = opt_state$opt.result$mbo.result, design.p = designs[[as.character(x)]], iter.p = x,
            feature.fe = feature, method.fe = method, grid.size.fe = grid.size, center.at.fe = center.at
          )
        }
      )
      names(result) = model_seq
    }
  }
  return(result)
}




# helper functions for 'renderVisualizeOptPathNd'
getFeatureEffectMBO = function(model.p, data.p, y.p, class.p, predict.fun.p, type.p, batch.size.p,
                               feature.fe, method.fe, grid.size.fe, center.at.fe) {
  # 1. create a Predictor object
  pred = iml::Predictor$new(
    model = model.p, data = data.p, y = y.p, class = class.p, predict.fun = predict.fun.p,
    type = type.p, batch.size = batch.size.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, feature = feature.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}

getFeatureEffectAfMBO = function(model.p, data.p, y.p, batch.size.p, res.mbo.p, design.p, iter.p,
                                 feature.fe, method.fe, grid.size.fe, center.at.fe) {
  # 1. create a Predictor object
  pred = PredictorAf$new(
    model = model.p, data = data.p, y = y.p, batch.size = batch.size.p,
    res.mbo = res.mbo.p, design = design.p, iter = iter.p
  )
  # 2. estimate the feature effect
  effect = iml::FeatureEffect$new(
    predictor = pred, feature = feature.fe, method = method.fe, grid.size = grid.size.fe,
    center.at = center.at.fe
  )
}

# this function is used to calculate the progress in the given iteration if infill crit AdaCB is used
getProgressAdaCB = function(res.mbo, iter) {
  if (res.mbo$control$infill.crit$id == "adacb") {

    opt_path_df = as.data.frame(res.mbo$opt.path)
    lambda.start = res.mbo$control$infill.crit$params$cb.lambda.start
    lambda.end = res.mbo$control$infill.crit$params$cb.lambda.end
    lambda = opt_path_df[which(opt_path_df$dob == iter), "lambda"]
    progress = (lambda - lambda.start) / (lambda.end - lambda.start)

  } else {
    progress = NULL
  }
}
