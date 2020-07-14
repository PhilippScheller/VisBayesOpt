

RSQOverIterations = function(opt_path, names_x) {
  opt_path_x = opt_path[,names_x, drop = FALSE]
  y_hat = opt_path$mean
  y_eval = opt_path$y
  y_df = data.frame(y.hat = y_hat, y.eval = y_eval)
  p = ncol(opt_path_x)
  n = nrow(opt_path_x)

  rss = sum((y_eval-y_hat)^2)
  tss = sum((y_eval-mean(y_eval))^2)
  if (tss == 0) {
    R2 = NA_real_
  } else {
    R2 = ifelse(1-(tss/(rss+tss)) < 0, 0, tss/(rss+tss)) # r2 numerical stable in accordante to r2 from summary.lm (in stats package)
  }
  return(R2)
}


