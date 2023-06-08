\name{bdTransform}
\alias{bdTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bickel-Docksum Transformation for Normality
}
\description{
\code{bdTransform} performs Bickel-Docksum transformation for normality of a variable and provides graphical analysis.  
}
\usage{bdTransform(data, lambda = seq(0.01,6,0.01), plot = TRUE, alpha = 0.05, 
  verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{lambda}{a vector which includes the sequence of candidate lambda values. Default is set to (0.01,6) with increment 0.01.} 
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}



\details{

Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Bickel-Docksum power transformation is defined by:
  
  \deqn{y' = \frac{|y|^\lambda Sign(y)-1}{\lambda} \mbox{ , if $\lambda > 0$} }



}

\value{
A list with class "bd" containing the following elements:

\item{method}{method to estimate Bickel-Docksum transformation parameter}
\item{lambda.hat}{estimate of Bickel-Docksum transformation parameter}
\item{statistic}{Shapiro-Wilk test statistic for transformed data}
\item{p.value}{Shapiro-Wilk test p.value for transformed data}
\item{alpha}{level of significance to assess normality} 
\item{tf.data}{transformed data set}
\item{var.name}{variable name}


}
\references{
Asar, O., Ilk, O., Dag, O. (2017). Estimating Box-Cox Power Transformation Parameter via Goodness of Fit Tests. \emph{Communications in Statistics - Simulation and Computation}, \bold{46:1}, 91--105.

Bickel, P.J., Doksum, K.A. (1981). An Analysis of Transformations Revisited. \emph{Journal of the American Statistical Association}, \bold{76:374}, 296--311.
}
\author{
Muge Coskun Yildirim, Osman Dag 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{bdTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- bdTransform(data)
out$lambda.hat # the estimate of Bickel-Docksum parameter based on Shapiro-Wilk test statistic 
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

