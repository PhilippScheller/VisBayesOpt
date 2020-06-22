

RSQOverIterations = function(model, opt_path, control, names_x) {
  # assertClass(opt_path, "OptPath")
  # assertClass(control, "MBOControl")
  niter = opt_path[nrow(opt_path), "dob"]

  infill.mean = makeMBOInfillCritMeanResponse()$fun
  opt_path_x = opt_path[,names_x]
  new_x = opt_path_x[(nrow(opt_path)-niter+1):nrow(opt_path),]
  #print(new_x)

  y_hat = ifelse(control$minimize, 1, -1) * infill.mean(new_x, list(model), control)
  y_eval = opt_path$y
  y_df = data.frame(y.hat = y_hat, y.eval = y_eval)

  rss = sum((y_eval-y_hat)^2)
  tss = sum((y_eval-mean(y_eval))^2)
  if (tss == 0) {
    R2 = NA_real_
  } else {
    R2 = ifelse((1-rss/tss) < 0, NA_real_, 1-rss/tss)
  }
  return(R2)
}
