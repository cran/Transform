\name{Transform}
\alias{Transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical Transformations for Normality
}
\description{
\code{Transform} performs transformations for normality of a variable and provides graphical analysis.  
}
\usage{Transform(data, method = "dl", lambda = seq(0,6,0.01), lambda2 = NULL, plot = TRUE, 
  alpha = 0.05, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{method}{a character string. Different transformation methods can be used for the estimation of the optimal transformation parameter: Box-Cox ("bc"), Log-shift ("ls"), Bickel-Doksum ("bd"), Yeo-Johnson ("yj"), Square Root ("ss"), Manly ("mn"), Modulus ("md"), Dual ("dl"), Gpower ("gp"), Log ("lg"), Glog ("gl"), Neglog ("nl"), Reciprocal ("rp"). Default is set to method = "dl".}
  \item{lambda}{a vector which includes the sequence of candidate lambda values. Please see the corresponding method to learn the lambda range. Default is set to (0,6) with increment 0.01.} 
  \item{lambda2}{a numeric for an additional shifting parameter. Please see the corresponding method to learn the lambda2. Default is set to lambda2 = NULL.} 
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}


\value{
See the corresponding transformation method.

}

%\references{
%}

\author{
Muge Coskun Yildirim, Osman Dag
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{bcTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- Transform(data, method = "bc")
out$lambda.hat # the estimate of Box-Cox parameter based on Shapiro-Wilk test statistic 
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

