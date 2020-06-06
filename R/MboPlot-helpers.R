# MboPlotInputSpace helpers

extractFromDf = function(df, extr) {
  df = assert_class(df, "data.frame")
  extr = assert_class(extr, "list")
  check_fun = lapply(extr, check_function)
  if (!all(unlist(check_fun))) stop("Make sure that provided extractions are all functions")
  if (length(extr) > 1) stop("Only 1 function can be evaluated, you provided multiple ones")

  df_select = df %>%
    select_if(extr)

  return(df_select)
}

