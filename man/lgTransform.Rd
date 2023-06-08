\name{lgTransform}
\alias{lgTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log Transformation for Normality
}
\description{
\code{lgTransform} performs Log transformation for normality of a variable and provides graphical analysis.  
}
\usage{lgTransform(data, lambda2 = NULL, plot = TRUE, alpha = 0.05, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{lambda2}{a numeric for an additional shifting parameter. Default is set to lambda2 = NULL.} 
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Log power transformation is defined by:
  
  \deqn{y' = \log(y)} 

If the data include any nonpositive observations, a shifting parameter \eqn{\lambda_2}{lambda2} can be included in the transformation given by:

  \deqn{y' = \log(y+\lambda_2)} 

}

\value{
A list with class "lg" containing the following elements:

\item{method}{method name}
\item{lambda2}{additional shifting parameter}
\item{statistic}{Shapiro-Wilk test statistic for transformed data}
\item{p.value}{Shapiro-Wilk test p.value for transformed data}
\item{alpha}{level of significance to assess normality} 
\item{tf.data}{transformed data set}
\item{var.name}{variable name}


}
\references{
Asar, O., Ilk, O., Dag, O. (2017). Estimating Box-Cox Power Transformation Parameter via Goodness of Fit Tests. \emph{Communications in Statistics - Simulation and Computation}, \bold{46:1}, 91--105.

Box, G.E., Cox, D.R. (1964). An Analysis of Transformations. \emph{Journal of the Royal Statistical Society: Series B (Methodological)}, \bold{26:2}, 211--43.

}
\author{
Muge Coskun Yildirim, Osman Dag
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{lgTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- lgTransform(data)
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

