renderVisualizeOptPathNd = function(opt_state, interest = "surrogate", feature, method = "pdp", grid.size = 20,
                                    center.at = NULL, batch.size = 1000, parallel = TRUE) {

  # data frames
  opt_path_df =  as.data.frame(opt_state$opt.path)
  design = opt_state$opt.problem$design
  # surrogate models, control object, par.set, und critfun
  sm = opt_state$opt.result$stored.models
  control = opt_state$opt.problem$control
  par_set = opt_state$opt.path$par.set
  critfun = control$infill.crit$fun
  # Tags, Names, Integers
  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  name_y = opt_state$opt.path$y.names
  names_x = names(opt_state$opt.path$par.set$pars)
  # iters are the actually done "iterations" in the process, corresponds to dob in the opt_path_df
  iters = opt_path_df[nrow(opt_path_df), "dob"]

  # model_seq are the model_seq models in the process, not every iter has a model_seq model, only iters with
  # model_seq models can be analyzed
  model_seq = sort(as.integer(names(opt_state$opt.result$stored.models)))

  # a & c
  #checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
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

  assertInteger(model_seq,
                lower = 1,
                any.missing = FALSE,
                min.len = 1,
                max.len = iters + 1,
                sorted = TRUE,
                unique = TRUE #mbo automatically saves unique models, e.g. s.m.a = c(1,2,2,5)
                #models 1,2,5 are model_seq. it also rm NAs automatically, e.g. s.m.a = c(1,2,NA)
  )
  # stops if multi point proposal or multiobjective target fun
  if (control$propose.points > 1 | control$n.objectives > 1)
    stop("FeatureEffectMBO not implemented for Multipoint-Proposal or Multiobjective function")
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
  # in all other cases the analysis can be conducted, note that if length(model_seq) == 1, only 1 iter
  # can be analysed, and eventually only the surrogate model (if model_seq = iters + 1)

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
      #assertDataFrame(seen.points[[x]], all.missing = FALSE, any.missing = FALSE)
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
    cl = parallel::makeCluster(no.cores, type = "FORK")

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




############################ getFeatureEffect ##################################

getFeatureEffectMBO = function(model.p, data.p, y.p, class.p, predict.fun.p, type.p, batch.size.p,
                               feature.fe, method.fe, grid.size.fe, center.at.fe
) {
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

############################## getFeatureEffectAf ##############################
getFeatureEffectAfMBO = function(model.p, data.p, y.p, batch.size.p, res.mbo.p, design.p, iter.p,
                                 feature.fe, method.fe, grid.size.fe, center.at.fe
) {
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

########################## getProgressAdaCB #################################
# this function is used to calculate the progress in the given iteration if
# infill crit AdaCB is used
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

#' PredictorAf = R6::R6Class("PredictorAf",
#'                           inherit = iml::Predictor,
#'                           public = list(
#'                             #' @description Create a PredictorAf object
#'                             #' @param model\cr
#'                             #' A machine learning model from `mlr` package, used as surrogate model in the
#'                             #' MBO process, and needed in order to measure the Infill criteria.
#'                             #' @param data [data.frame]\cr
#'                             #' The data for which we want to measure the Infill value, namely the seen points
#'                             #' within an iteration of the MBO process.
#'                             #' @param y `character(1)` | [numeric] | [factor]\cr
#'                             #' The target vector or (preferably) the name of the target column in the `data` argument.
#'                             #' PredictorAf tries to infer the target automatically from the model. PredictorAf objects
#'                             #' can also be created if y is NULL.
#'                             #' @param batch.size `numeric(1)`\cr
#'                             #' The maximum number of rows to be input the model for prediction at once.
#'                             #' Currently only respected for [FeatureImp], [Partial] and [Interaction].
#'                             #' @param res.mbo [MBOSingleObjResult]\cr
#'                             #' The results of the MBO process.
#'                             #' @param design [data.frame]\cr
#'                             #' The design or training set used to fit the surrogate model. Note, that such observation
#'                             #' have Infill value of 0. The argument must include also a column with the target value.
#'                             #' Note, that this argument corresponds to the data argument of the Predictor Obejct in `iml`.
#'                             #' Allowed column classes are: [numeric], [factor], [integer], [ordered] and [character]. For some models the data can be extracted automatically.
#'                             #' `PredictorAf$new()` throws an error when it can't extract the design automatically.
#'                             #' @param iter `numeric(1)`\cr
#'                             #' The iteration of interest within the MBO process
#'                             initialize = function(
#'                               # some arguments of the iml::Predictor
#'                               model = NULL, data = NULL, y = NULL, batch.size = 1000,
#'                               # arguments needed for the Predictor of the Af
#'                               res.mbo = NULL, design = NULL, iter = NULL
#'                             ) {
#'                               checkmate::assertClass(model, classes = "WrappedModel", null.ok = FALSE)
#'                               checkmate::assertClass(res.mbo, classes = c("MBOSingleObjResult", "MBOResult"))
#'                               opdf =  as.data.frame(res.mbo$opt.path)
#'                               itmax = opdf[nrow(opdf), "dob"]
#'                               checkmate::assertNumber(iter, lower = 1, upper = itmax)
#'
#'                               # this need to be fixed because the data argument in PredictorAf is different then
#'                               # in the normal Predictor obeject -- if NULL it extracts the data from model, which
#'                               # are theoretically our designs
#'                               if (is.null(design)) {
#'                                 tryCatch(
#'                                   {
#'                                     design <- prediction::find_data(model)
#'                                   },
#'                                   error = function(e) stop("Can't extract design from model, please provide via design=")
#'                                 )
#'                               } else {
#'                                 # assertions works only for SingleObejective & SinglePoint Proposals
#'                                 checkmate::assertDataFrame(design,
#'                                                            any.missing = FALSE, all.missing = FALSE,
#'                                                            ncols = length(names(res.mbo$x)) + 1,
#'                                                            nrows = nrow(res.mbo$final.opt.state$opt.problem$design) + iter - 1
#'                                 )
#'                                 checkmate::assertNames(names(design), identical.to = c(names(res.mbo$x), res.mbo$control$y.name))
#'                               }
#'                               #it's ok if y if found in model since the name is the same as in the data
#'                               if (is.null(y)) {
#'                                 y = res.mbo$control$infill.crit$id
#'                                 # y not always needed, so ignore when not in data
#'                                 if (is.character(y) && !(y %in% names(data))) {
#'                                   y = NULL
#'                                 }
#'                               }
#'                               if (is.null(data)) {
#'                                 # notice that it extracts the data, but the value for the Af for those points is missing
#'                                 if (!is.null(res.mbo$seen.points)) data = res.mbo$seen.points[[iter]]
#'                                 else stop("Can't extract seen points. Provide them via data= or make sure to run the MBO with Savepts opt. methods")
#'                               }
#'                               self$data = Data$new(data, y = y)
#'                               #self$class = class
#'                               self$model = list(model)
#'                               self$task = inferTaskFromModel(model)
#'                               self$batch.size = batch.size
#'                               # begin edited:
#'                               self$res.mbo = res.mbo
#'                               self$par.set = res.mbo$opt.path$par.set
#'                               self$control = res.mbo$control
#'                               self$design = list(design)
#'                               # add error if y is not included in design does not include y
#'                               self$iter = iter
#'                               self$progress = getProgressAdaCB(res.mbo, iter)
#'                               # the prdiction function is extracted from res.mbo, do not need an argument
#'                               self$prediction.function = res.mbo$control$infill.crit$fun
#'                               # end edited
#'                             },
#'                             #' @description Predict the value of the Af for newdata. Although the function
#'                             #' works for design points, it theoretically makes no sense to predict the Af
#'                             #' for such points, as their Af value is per definition 0. Resulting Af values
#'                             #' will be very close to 0.
#'                             #' @template newdata
#'                             predict = function(newdata) {
#'                               checkmate::assert_data_frame(newdata)
#'                               # Makes sure it's not a data.table
#'                               newdata = as.data.frame(newdata)
#'                               # make sure only features are used
#'                               newdata = newdata[, intersect(
#'                                 self$data$feature.names,
#'                                 colnames(newdata)
#'                               ), drop = FALSE]
#'                               # begin edited:
#'                               prediction = self$prediction.function(
#'                                 points = newdata,
#'                                 models = self$model,
#'                                 control = self$control,
#'                                 par.set = self$par.set,
#'                                 designs = self$design,
#'                                 iter = self$iter,
#'                                 progress = self$progress,
#'                                 attributes = FALSE
#'                               )
#'                               prediction = data.frame(prediction)
#'                               # end edited:
#'                               if (!private$predictionChecked) {
#'                                 checkPrediction(prediction, newdata)
#'                                 private$predictionChecked = TRUE
#'                               }
#'                               # If S3 or function, we can only infer the task
#'                               # once we see the predictions
#'                               if (is.null(self$task)) {
#'                                 self$task = inferTaskFromPrediction(prediction)
#'                               }
#'                               rownames(prediction) = NULL
#'                               data.frame(prediction, check.names = FALSE)
#'                             },
#'                             #' @description Print the PredictorAf object.
#'                             print = function() {
#'                               cat(
#'                                 "Prediction task of surrogate model:", self$task,
#'                                 ", Infill criteria:", self$res.mbo$control$infill.crit$id, "\n"
#'                               )
#'                             },
#'
#'                             #' @field data [data.frame]\cr
#'                             data = NULL,
#'
#'                             #' @field model (any)\cr
#'                             model = NULL,
#'
#'                             #' @field batch.size `numeric(1)`\cr
#'                             #' The number of rows to be input the model for prediction at once.
#'                             batch.size = NULL,
#'
#'                             #' @field task `character(1)`\cr
#'                             #'   The inferred prediction task: `"classification"` or `"regression"`. So far,
#'                             #'   you will encounter only `"regression"`.
#'                             task = NULL,
#'
#'                             # begin edited:
#'                             #' @field res.mbo [MBOSingleObjResult]
#'                             #' the results of the MBO process
#'                             res.mbo = NULL,
#'
#'                             #' @field prediction.function
#'                             #' The acquisition function extracted from the MBO process. So far, the seven
#'                             #' built in AF for SinglObejctive are implemented. A function which expects the
#'                             #' following parameters in exactly this order and return a numeric vector of
#'                             #' criteria values at the points: points, models, control, par.set, design, iter,
#'                             #' progress, attributes
#'                             prediction.function = NULL,
#'
#'                             #' @field par.set [ParamSet]\cr
#'                             #' the parameter set of the MBO process, describing different aspects of the
#'                             #' objective function parameters
#'                             par.set = NULL,
#'
#'                             #' @field control [MBOControl]\cr
#'                             #' the control object of the MBO process
#'                             control = NULL,
#'
#'                             #' @field design [data.frame]\cr
#'                             #' the design used in the the specific iteration of the MBO process
#'                             design = NULL,
#'
#'                             #' @field  iter `integer(1)`\cr
#'                             #' The iteration of interest, in which we want to analyse the AF. Please make sure that the
#'                             #' corresponding surrogate model has been stored troughout the process (see store.model.at
#'                             #' in makeMBOControl)
#'                             iter = NULL,
#'
#'                             #' @field progress `numeric(1)`\cr
#'                             #' A value between 0 and 1 indicating the progress of the optimization. Only needed in case of AdaCB,
#'                             #' or other custom Adaptive Infill Criteria. The progress is calculated with intenal function
#'                             #' getProgressAdaCB.
#'                             progress = NULL
#'                           ),
#'                           private = list(
#'                             predictionChecked = FALSE
#'                           )
#' )
#'
