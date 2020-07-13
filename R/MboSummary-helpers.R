generateSummaryRowsUiShiny = function(mboSummary = NULL) {
  assertClass(mboSummary, "mboSummary")

  ifelse(is.null(mboSummary$mboSummary), stop("No summary provided, first run mboSummary() on the object"),
         summary = mboSummary$mboSummary)
  groups = c(unlist(lapply(mboSummary, function(x) get("group", x))))
  maxRow = max(table(groups))

  generateColumnsUi(ncols = 3, nrows = maxRow, mboSummary)

}

generateColumnsUi = function(ncols = 1, nrows = 1, mboSummary) {

  # for (column in 1:n)
  grid = expand.grid(seq(1:nrows), 1:ncols)
  gridSort = grid[order(grid[,1]),]
  par_id = sprintf("%s.%s", gridSort[,1], gridSort[,2])

    generateTextUi()
}

generateTextUi = function(text, par_name, par_id) {

  ui = textOutput(par_id, )

}
