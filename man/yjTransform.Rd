\name{yjTransform}
\alias{yjTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Yeo- Johnson Transformation for Normality
}
\description{
\code{yjTransform} performs Yeo- Johnson transformation for normality of a variable and provides graphical analysis.  
}
\usage{yjTransform(data, lambda = seq(-3,3,0.01), plot = TRUE, alpha = 0.05, 
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
Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Yeo-Johnson power transformation is defined by:
  
  \deqn{y' = \left\{ \begin{array}{ll}
    \frac{(y+1)^\lambda-1}{\lambda} \mbox{ ,  if $\lambda \neq 0, y \geq 0$} \cr
    \log(y+1) \mbox{ , if $\lambda = 0, y \geq 0$} \cr
    \frac{(1-y)^{2-\lambda}-1}{\lambda-2} \mbox{ ,  if $\lambda \neq 2, y < 0$} \cr
    -\log(1-y) \mbox{ ,  if $\lambda = 2, y < 0$} 
    \end{array} \right.}

}

\value{
A list with class "yj" containing the following elements:

\item{method}{method to estimate Yeo-Johnson transformation parameter}
\item{lambda.hat}{estimate of Yeo-Johnson transformation parameter}
\item{statistic}{Shapiro-Wilk test statistic for transformed data}
\item{p.value}{Shapiro-Wilk test p.value for transformed data}
\item{alpha}{level of significance to assess normality} 
\item{tf.data}{transformed data set}
\item{var.name}{variable name}


}
\references{
Asar, O., Ilk, O., Dag, O. (2017). Estimating Box-Cox Power Transformation Parameter via Goodness of Fit Tests. \emph{Communications in Statistics - Simulation and Computation}, \bold{46:1}, 91--105.

Yeo, I.K., Johnson, R.A. (2000). A New Family of Power Transformations to Improve Normality or Symmetry. \emph{Biometrika}, \bold{87:4}, 954--9.

}
\author{
Muge Coskun Yildirim, Osman Dag 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{yjTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- yjTransform(data)
out$lambda.hat # the estimate of Yeo- Johnson parameter based on Shapiro-Wilk test statistic 
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

