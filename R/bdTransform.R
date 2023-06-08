bdTransform <- function(data, lambda = seq(0.01, 6, 0.01), plot = TRUE, alpha = 0.05, verbose = TRUE){
  
  dname<-deparse(substitute(data))
  
  data<-as.numeric(data)
  
  
  if (is.na(min(data))==TRUE) stop("Data include NA")
  if (min(lambda)<=0) stop("Lambda can be positive values.")
  
  
  lambda <- lambda[lambda>0]
  
  store1 <- lapply(1:length(lambda), function(x) lambda[x])
  store2 <- lapply(1:length(lambda), function(x) (abs(data)^store1[[x]]*sign(data)-1)/store1[[x]] )
  store3 <- lapply(1:length(store2), function(x) shapiro.test(store2[[x]])$statistic)
  
  pred.lamb<-store1[[which.max(store3)]]
  method.name<-"Estimating Bickel-Docksum transformation parameter via Shapiro-Wilk test statistic"
  
  
  if (pred.lamb==max(lambda)) warning("Enlarge the range of the lambda")
  if (pred.lamb==min(lambda)) warning("Enlarge the range of the lambda")
  
  
  
  data.transformed<-(abs(data)^pred.lamb*sign(data)-1)/pred.lamb 

  
  
  if(plot){
    
    
    oldpar<-par(mfrow=c(2,2))
    on.exit(par(oldpar))
    hist(data, xlab = dname, prob=TRUE, main = paste("Histogram of", dname))
    lines(density(data))
    hist(data.transformed, xlab = paste("Transformed", dname), prob=TRUE, main = paste("Histogram of tf", dname))
    lines(density(data.transformed))
    qqnorm(data, main = paste("Q-Q plot of", dname))
    qqline(data)
    qqnorm(data.transformed, main = paste("Q-Q plot of tf", dname))
    qqline(data.transformed)
    
    
  }
  
  
  
  statistic<-shapiro.test(data.transformed)$statistic
  pvalue<-shapiro.test(data.transformed)$p.value
  nortest.name<-"Shapiro-Wilk normality test"
  
  
  if (verbose){
    cat("\n"," Bickel-Docksum Power transformation", "\n", sep = " ")
    cat("-------------------------------------------------------------------", "\n\n", sep = " ")
    cat("  lambda.hat :", pred.lamb, "\n\n", sep = " ")
    cat("\n", "  ",nortest.name," for transformed data ", "(alpha = ",alpha,")", "\n", sep = "")
    cat("-------------------------------------------------------------------", "\n\n", sep = " ")
    cat("  statistic  :", statistic, "\n", sep = " ")
    cat("  p.value    :", pvalue, "\n\n", sep = " ")
    cat(if(pvalue > alpha){"  Result     : Transformed data are normal."}
        else {"  Result     : Transformed data are not normal."},"\n")
    cat("-------------------------------------------------------------------", "\n\n", sep = " ")
  }
  
  out<-list()
  out$method <- method.name
  out$lambda.hat <- as.numeric(pred.lamb)
  out$statistic <- as.numeric(statistic)
  out$p.value <- as.numeric(pvalue)
  out$alpha <- as.numeric(alpha)
  out$tf.data <- data.transformed
  out$var.name <- dname
  attr(out, "class") <- "bd"
  invisible(out)
  
  
}

