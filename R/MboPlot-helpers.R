# MboPlotInputSpace helpers

generateWideFormat = function(df) {
  df = assert_class(df, "data.frame")
  colnames = assert_class(names(df), "character")

  df_colnames = data.frame(matrix(
    rep(colnames(df), each = nrow(df)),ncol = ncol(df)),
    stringsAsFactors = F)

  id = paste("id", colnames, sep = ".")
  names(df_colnames) = id
  df_combined = cbind.data.frame(df, df_colnames)

  out = list(df = df_combined, id.vars = id)
  return(out)
}

