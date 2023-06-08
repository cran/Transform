nlTransform <- function(data, plot = TRUE, alpha = 0.05, verbose = TRUE){
  
  dname<-deparse(substitute(data))
  
  data<-as.numeric(data)
  
  

  if (is.na(min(data))==TRUE) stop("Data include NA")
  
  

  

  method.name<-"Neglog transformation"
  
  

  
  
  
  data.transformed<-sign(data)*log(abs(data)+1)
 
  
  
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
    cat("\n"," Neglog transformation", "\n", sep = " ")
    cat("-------------------------------------------------------------------", "\n\n", sep = " ")
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
  out$statistic <- as.numeric(statistic)
  out$p.value <- as.numeric(pvalue)
  out$alpha <- as.numeric(alpha)
  out$tf.data <- data.transformed
  out$var.name <- dname
  attr(out, "class") <- "nl"
  invisible(out)
  
  
}
