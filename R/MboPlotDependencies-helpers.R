# Helper function for 'MboPlotDependencies'
# Creates the scatter plot combinations on the lower triangular matrix
create_gg_combinations_scatter = function(df, legend_title, y_best_index) {
  assertClass(df, "data.frame")
  assertClass(legend_title, "character")
  assertClass(y_best_index, "integer")
  df_col = df[, which(names(df) %in% c("fill_col")), drop = FALSE]
  df_x = df[, -which(names(df) %in% c("fill_col")), drop = FALSE]
  df_list = create_df_combinations_list(df_x, df_col, y_best_index)
  gg_list = lapply(df_list, create_gg_object, legend_title)

  return(gg_list)
}

# create list object with all pairwise combinations of data and labels (x,y axis)
create_df_combinations_list = function(df_x, df_col, y_best_index) {
  assertClass(df_x, "data.frame")
  n = ncol(df_x)
  list_index = 1
  df_list = list()

  for (i in 1:n) {
    for (j in 1:n) {
      if (i >= j) {
        if (i == j) {# for plotting histogram on diagonal
          hist = TRUE
          gg_df = df_x[, i, drop = FALSE]
        } else {
          hist = FALSE
          gg_df = cbind(df_x[, i, drop = FALSE], df_x[, j, drop = FALSE], df_col)
          }
        if (i == n) { # for dropping axis labels in inner plots of the matrix (i.e. only keep for last plot of row and column)
          x_axis = TRUE # plot x axis label
        } else {
          x_axis = FALSE # do not plot x axis label
        }
        if (j == 1) {
          y_axis = TRUE # plot y axis label
        } else {
          y_axis = FALSE # do not plot y axis label
        }
        df_list[[list_index]] = list(gg_df = gg_df, x_axis = x_axis, y_axis = y_axis, hist = hist, y_best_index = y_best_index)
        list_index = list_index+1
      } else {
        df_list[[list_index]] = list(gg_df = NULL, x_axis = NULL, y_axis = NULL, hist = NULL, y_best_index = NULL)
        list_index = list_index+1
      }
    }
  }
  return(df_list)
}

# based on output of 'create_df_combinations_list' this function creates all pairwise ggplots and the respective axis labels (only on outer axes)
create_gg_object = function(df, legend_title) {
  if (!is.null(df$gg_df)) {
    axis_x = df$x_axis[1] # if TRUE plot x axis, if FALSE do not
    axis_y = df$y_axis[1] # if TRUE plot y axis, if FALSE do not
    hist = df$hist # if TRUR plot historgram (for diagonal)
    df_sel = df$gg_df
    y_best_ind = df$y_best_index

    if (hist) {
      name_x2 = colnames(df_sel[, 1, drop = FALSE])
      name_x1 = "frequency"
    } else {
      name_x1 = colnames(df_sel[, 1, drop = FALSE])
      name_x2 = colnames(df_sel[, 2, drop = FALSE])
      name_col = colnames(df_sel[, 3, drop = FALSE])
    }
    if (hist) {
      gg = ggplot(df_sel, aes(x = get(name_x2)))
      gg = gg + geom_bar(aes(y = ..prop.., group = 1))
      if (!is.factor(df_sel[, paste(name_x2)])) {
        gg = gg + scale_x_binned(n.breaks = 10, labels = scales::number_format(accuracy = .1))
      }
      gg = gg + scale_y_continuous(name = paste(name_x1), position = "right", sec.axis = dup_axis(name = ""))
      gg = gg + theme(axis.text.y.right = element_blank(),
                      axis.title.y.left = element_blank(),
                      axis.ticks.y.right = element_blank())
    } else {
      gg = ggplot(df_sel, aes(y = get(name_x1), x = get(name_x2), col = get(name_col)))
      gg = gg + geom_point()
      gg = gg + geom_point(data = df_sel[y_best_ind, ,drop = FALSE],
                           mapping = aes(y = get(name_x1), x = get(name_x2)), colour = "red", pch = 17, size = 3)
  }
  if (axis_x) {
    gg = gg + xlab(paste(name_x2))
  } else {
  gg = gg + theme(axis.title.x=element_blank(),
                  #axis.ticks.x = element_blank(),
                  axis.text.x = element_blank())
  }
  if (!axis_y & !hist) {
    gg = gg + theme(axis.title.y=element_blank(),
                    #axis.ticks.y = element_blank(),
                    axis.text.y = element_blank())
  } else {
    gg = gg + ylab(paste(name_x1))
  }
  gg = gg + labs(color = legend_title)
  } else {
    gg = NULL
  }
  return(gg)
}
