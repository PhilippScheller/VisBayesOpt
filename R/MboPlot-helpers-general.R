# General helpers (used in various functions)
# Extract all columns of a data.frame which are of a certain type ('extr'). 'keepColumnNo' preserves columns to be returned anyway.
extractFromDf = function(df, extr, keepColumNo = c(1)) {
  df = as.data.frame(df)
  assertClass(extr, "list")
  check_fun = lapply(extr, check_function)
  if (!all(unlist(check_fun))) stop("Make sure that provided extractions are all functions")
  if (length(extr) > 1) stop("Only 1 function can be evaluated, you provided multiple ones")

  df_select = df %>%
    select_if(extr)
  if (keepColumNo > 0) {
    df_select = cbind.data.frame(df[keepColumNo], cbind(df_select))
  }

  return(df_select)
}
# Convert data.frame 'df_wide' to long format. Long format often needed for plotting with ggplot. 'keyColumn' is a column which is preserved
# when passing the argumnts to the function 'gather()'.
wideToLong = function(df_wide, keyColumn = 1) {

  key = colnames(df_wide[keyColumn])

  # turn of warning in pipe since 'gather()' throws warning loosing attributes of data.frame
  df_long = df_wide %T>% # '%T>%' enables passing argumentw like turning off warnings. Besides that its the same as the usual pipe operator %>%
    {options(warn = -1)} %>%
    gather("Param", "Value", -all_of(key) ,convert = TRUE, factor_key = TRUE) %T>%
    {options(warn = 0)}

  return(df_long)
}


### Backup - currently not needed ###
# Specific helpers (only used in specified function)
# MboPlotInputSpace helpers

# wrappedPlot = function(df_long, title, method = c("numeric", "discrete"), type = c("prior", "posterior", "overlay"),
#                        plot = c("distribution", "iteration"), n, theme = NULL) {
#
#   if (type %in% c("prior", "posterior")) {
#     df = get(type, df_long)
#   } else {
#     names(df_long[[1]])[1] = "type"
#     names(df_long[[2]])[1] = "type"
#     df = rbind.data.frame(df_long[[1]], df_long[[2]])
#   }
#   df_colnames = colnames(df)
#
#   if (ncol(df) != 3) stop("data.frame needs to be in long format (i.e. have exactly 3 columns).")
#   if (length(method) != 1) stop("Only 1 method can be selected, choose either 'numeric' or 'discrete'.")
#   if (length(type) != 1) stop("Only 1 type can be selected, choose either 'prior', 'posterior' or 'overlay'.")
#   if (all(df_colnames %nin% c("Param", "Value"))) stop("Columns must be named with 'Value' and 'Param'.")
#
#
#   gg = ggplot(df, aes(x = Value, colour = type))
#   if (plot == "distribution") {
#     if (method == "numeric") {
#       gg = gg + geom_density(n = 2^6)
#     } else {
#       gg = gg + geom_bar()
#     }
#   }
#   if (plot == "iteration") {
#     if (method == "numeric") {
#       gg = gg + geom_point(aes(x = rep(seq(1:n), times = nrow(df)/n), y = Value),
#                            size = 0.5)
#       gg = gg + geom_smooth(aes(x = rep(seq(1:n), times = nrow(df)/n), y = Value),
#                             method = "lm", formula = y~x, size=0.5)
#     } else {
#       # generate frequency df for factor variables
#       df_frequencies = generateFrequencyDf(df)
#       gg = gg + geom_point(aes(x = rep(seq(1:n), times = nrow(df)/n), y = Value),
#                            size = 0.5)
#     }
#   }
#   gg = gg + facet_wrap(Param ~ ., scales = "free")
#   gg = gg + ggtitle(title)
#   gg = gg + theme(plot.title = element_text(face = "bold"))
#   gg = gg + theme
#
#   return(gg)
# }

# generateFrequencyDf = function(df) {
#   assertClass(df, "data.frame")
#
#   df_n = df %>%
#     group_by(Value) %>%
#     mutate(new = cumany(Value == "dart"))
#
# }
