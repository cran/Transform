bcTransform <- function(data, lambda = seq(-3,3,0.01), lambda2 = NULL, plot = TRUE, alpha = 0.05, verbose = TRUE){
    
    dname<-deparse(substitute(data))
    
    data<-as.numeric(data)
   
    
    if(is.null(lambda2)) lambda2<-0
    
    data <- data+lambda2

    if (is.na(min(data))==TRUE) stop("Data include NA")
    if (min(data)<=0) stop("Data must include positive values. Specify shifting parameter, lambda2")



 
store1<-lapply(1:length(lambda), function(x) lambda[x])
store2<-lapply(1:length(lambda), function(x) if (store1[[x]] != 0) (data^store1[[x]]-1)/store1[[x]] else log(data))
store3<-lapply(1:length(lambda), function(x) shapiro.test(store2[[x]])$statistic)
     
pred.lamb<-store1[[which.max(store3)]]
method.name<-"Estimating Box-Cox transformation parameter via Shapiro-Wilk test statistic"
      

if (pred.lamb==max(lambda)) warning("Enlarge the range of the lambda")
if (pred.lamb==min(lambda)) warning("Enlarge the range of the lambda")
      


if (pred.lamb!=0) data.transformed<-((data^pred.lamb)-1)/pred.lamb
if (pred.lamb==0) data.transformed<-log(data)


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
        cat("\n"," Box-Cox power transformation", "\n", sep = " ")
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
      out$lambda2 <- as.numeric(lambda2)
      out$statistic <- as.numeric(statistic)
      out$p.value <- as.numeric(pvalue)
      out$alpha <- as.numeric(alpha)
      out$tf.data <- data.transformed
      out$var.name <- dname
      attr(out, "class") <- "bc"
      invisible(out)


  }

