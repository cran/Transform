\name{bcTransform}
\alias{bcTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Box-Cox Transformation for Normality
}
\description{
\code{bcTransform} performs Box-Cox transformation for normality of a variable and provides graphical analysis.  
}
\usage{bcTransform(data, lambda = seq(-3,3,0.01), lambda2 = NULL, plot = TRUE, 
  alpha = 0.05, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{lambda}{a vector which includes the sequence of candidate lambda values. Default is set to (-3,3) with increment 0.01.} 
  \item{lambda2}{a numeric for an additional shifting parameter. Default is set to lambda2 = NULL.} 
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{

Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Box-Cox power transformation is defined by:
  
  \deqn{y' = \left\{ \begin{array}{ll}
    \frac{y^\lambda - 1}{\lambda} \mbox{ ,  if $\lambda \neq 0$} \cr
    log(y) \mbox{ , if $\lambda = 0$} 
    \end{array} \right.}

If the data include any non- positive observations, a shifting parameter \eqn{\lambda_2}{lambda2} can be included in the transformation given by:

  \deqn{y' = \left\{ \begin{array}{ll}
    \frac{(y + \lambda_2)^\lambda - 1}{\lambda} \mbox{ ,  if $\lambda \neq 0$} \cr
    log(y + \lambda_2) \mbox{ , if $\lambda = 0$} 
    \end{array} \right.}

}
\value{
A list with class "bc" containing the following elements:

\item{method}{method to estimate Box-Cox transformation parameter}
\item{lambda.hat}{estimate of Box-Cox Power transformation parameter}
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
%\code{\link{bcTransform}}
%\code{\link{powerTransform}}
%}
\examples{

data <- cars$dist

library(Transform)
out <- bcTransform(data)
out$lambda.hat # the estimate of Box-Cox parameter based on Shapiro-Wilk test statistic 
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

