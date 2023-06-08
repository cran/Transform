\name{dlTransform}
\alias{dlTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dual Transformation for Normality
}
\description{
\code{dlTransform} performs Dual transformation for normality of a variable and provides graphical analysis.  
}
\usage{dlTransform(data, lambda = seq(0,6,0.01), plot = TRUE, alpha = 0.05, 
  verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{lambda}{a vector which includes the sequence of candidate lambda values. Default is set to (0,6) with increment 0.01.} 
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Dual power transformation is defined by:
  
  \deqn{y' = \left\{ \begin{array}{ll}
    \frac{y^\lambda-y^{-\lambda}}{2\lambda} \mbox{ ,  if $\lambda > 0$} \cr
    \log(y) \mbox{ , if $\lambda = 0$} 
    \end{array} \right.
}
}

\value{
A list with class "dl" containing the following elements:

\item{method}{method to estimate Dual transformation parameter}
\item{lambda.hat}{estimate of Dual transformation parameter}
\item{statistic}{Shapiro-Wilk test statistic for transformed data}
\item{p.value}{Shapiro-Wilk test p.value for transformed data}
\item{alpha}{level of significance to assess normality} 
\item{tf.data}{transformed data set}
\item{var.name}{variable name}


}
\references{
Asar, O., Ilk, O., Dag, O. (2017). Estimating Box-Cox Power Transformation Parameter via Goodness of Fit Tests. \emph{Communications in Statistics - Simulation and Computation}, \bold{46:1}, 91--105.

Yang, Z. (2006). A Modified Family of Power Transformations. \emph{Economics Letters}. \bold{92:1}, 14--9.

}
\author{
Muge Coskun Yildirim, Osman Dag 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{dlTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- dlTransform(data)
out$lambda.hat # the estimate of Dual parameter based on Shapiro-Wilk test statistic 
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

