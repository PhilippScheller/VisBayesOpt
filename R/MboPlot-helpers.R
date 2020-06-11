# MboPlotInputSpace helpers

extractFromDf = function(df, extr) {
  df = assert_class(df, "data.frame")
  extr = assert_class(extr, "list")
  check_fun = lapply(extr, check_function)
  if (!all(unlist(check_fun))) stop("Make sure that provided extractions are all functions")
  if (length(extr) > 1) stop("Only 1 function can be evaluated, you provided multiple ones")

  df_select = df %>%
    select_if(extr)
  df_sleect_named = cbind(df[1], df_select)

  return(df_sleect_named)
}

wideToLong = function(df_wide) {

  key = colnames(df_wide[1])

  # turn of warning in pipe since 'gather()' throws warning loosing attributes of data.frame
  df_long = df_wide %T>%
    {options(warn = -1)} %>%
    gather("Param", "Value", -key ,convert = TRUE, factor_key = TRUE) %T>%
    {options(warn = 0)}

  return(df_long)
}

wrappedPlot = function(df_long, title, method = c("numeric", "discrete"), type = c("prior", "posterior", "overlay"),
                           plot = c("distribution", "iteration"), n, theme = NULL) {

  if (type %in% c("prior", "posterior")) {
    df = get(type, df_long)
  } else {
    names(df_long[[1]])[1] = "type"
    names(df_long[[2]])[1] = "type"
    df = rbind.data.frame(df_long[[1]], df_long[[2]])
  }
  df_colnames = colnames(df)

  if (ncol(df) != 3) stop("data.frame needs to be in long format (i.e. have exactly 3 columns).")
  if (length(method) != 1) stop("Only 1 method can be selected, choose either 'numeric' or 'discrete'.")
  if (length(type) != 1) stop("Only 1 type can be selected, choose either 'prior', 'posterior' or 'overlay'.")
  if (all(df_colnames %nin% c("Param", "Value"))) stop("Columns must be named with 'Value' and 'Param'.")


  gg = ggplot(df, aes(x = Value, colour = type))
  if (plot == "distribution") {
    if (method == "numeric") {
      gg = gg + geom_density(n = 2^6)
    } else {
      gg = gg + geom_bar()
    }
  }
  if (plot == "iteration") {
    if (method == "numeric") {
      gg = gg + geom_point(aes(x = rep(seq(1:n), times = nrow(df)/n), y = Value))
      gg = gg + geom_smooth(aes(x = rep(seq(1:n), times = nrow(df)/n), y = Value),
                            method = "lm", formula = y~x)
    } else {
      gg = gg + geom_bar()
    }
  }
  gg = gg + facet_wrap(Param ~ ., scales = "free")
  gg = gg + ggtitle(title)
  gg = gg + theme(plot.title = element_text(face = "bold"))
  gg = gg + theme

  return(gg)
}

