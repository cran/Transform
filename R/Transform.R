Transform <- function(data, method= "dl", lambda = seq(0, 6, 0.01), lambda2 = NULL, plot = TRUE, alpha = 0.05, verbose = TRUE) 

{
  
  if (method == "bc") {(out <- bcTransform(data=data, lambda = lambda, lambda2 = lambda2, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "ls") {(out <- lsTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "bd") {(out <- bdTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "yj") {(out <- yjTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "ss") {(out <- ssTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "mn") {(out <- mnTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "md") {(out <- mdTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "dl") {(out <- dlTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "gp") {(out <- gpTransform(data=data, lambda = lambda, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "lg") {(out <- lgTransform(data=data, lambda2 = lambda2, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "gl") {(out <- glTransform(data=data, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "nl") {(out <- nlTransform(data=data, plot = plot, alpha = alpha, verbose = verbose))}
  else if (method == "rp") {(out <- rpTransform(data=data, plot = plot, alpha = alpha, verbose = verbose))}
  else stop("The incorrect transformation method argument")
  
  invisible(out)
}