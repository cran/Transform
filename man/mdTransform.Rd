\name{mdTransform}
\alias{mdTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modulus Transformation for Normality
}
\description{
\code{mdTransform} performs Modulus transformation for normality of a variable and provides graphical analysis.  
}
\usage{mdTransform(data, lambda = seq(-3,3,0.01), plot = TRUE, alpha = 0.05, 
  verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{lambda}{a vector which includes the sequence of candidate lambda values. Default is set to (-3,3) with increment 0.01.} 
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Modulus power transformation is defined by:
  
  \deqn{y' = \left\{ \begin{array}{ll}
    Sign(y)\frac{(|y|+1)^\lambda-1}{\lambda} \mbox{ ,  if $\lambda \neq 0$} \cr
    Sign(y) \log{(|y|+1)} \mbox{ , if $\lambda = 0$} 
    \end{array} \right.
}
}

\value{
A list with class "md" containing the following elements:

\item{method}{method to estimate Modulus transformation parameter}
\item{lambda.hat}{estimate of Modulus transformation parameter}
\item{statistic}{Shapiro-Wilk test statistic for transformed data}
\item{p.value}{Shapiro-Wilk test p.value for transformed data}
\item{alpha}{level of significance to assess normality} 
\item{tf.data}{transformed data set}
\item{var.name}{variable name}


}
\references{
Asar, O., Ilk, O., Dag, O. (2017). Estimating Box-Cox Power Transformation Parameter via Goodness of Fit Tests. \emph{Communications in Statistics - Simulation and Computation}, \bold{46:1}, 91--105.

John, J., Draper, N.R. (1980). An Alternative Family of Transformations. \emph{Journal of the Royal Statistical Society Series C: Applied Statistics}, \bold{29:2}, 190--7.

}
\author{
Muge Coskun Yildirim, Osman Dag 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{mdTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- mdTransform(data)
out$lambda.hat # the estimate of Modulus parameter based on Shapiro-Wilk test statistic 
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

