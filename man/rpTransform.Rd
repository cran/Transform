\name{rpTransform}
\alias{rpTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reciprocal Transformation for Normality
}
\description{
\code{rpTransform} performs Reciprocal transformation for normality of a variable and provides graphical analysis.  
}
\usage{rpTransform(data, plot = TRUE, alpha = 0.05, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a numeric vector of data values.}
  \item{plot}{a logical to plot histogram with its density line and qqplot of raw and transformed data. Defaults plot = TRUE.} 
  \item{alpha}{the level of significance to check the normality after transformation. Default is set to alpha = 0.05.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Denote \eqn{y} the variable at the original scale and \eqn{y'} the transformed variable. The Dual power transformation is defined by:
  
  \deqn{y' = \frac{1}{y}}
}

\value{
A list with class "rp" containing the following elements:

\item{method}{method name}
\item{statistic}{Shapiro-Wilk test statistic for transformed data}
\item{p.value}{Shapiro-Wilk test p.value for transformed data}
\item{alpha}{level of significance to assess normality} 
\item{tf.data}{transformed data set}
\item{var.name}{variable name}


}
\references{
Asar, O., Ilk, O., Dag, O. (2017). Estimating Box-Cox Power Transformation Parameter via Goodness of Fit Tests. \emph{Communications in Statistics - Simulation and Computation}, \bold{46:1}, 91--105.

Tukey, J.W. (1957). On the Comparative Anatomy of Transformations. \emph{The Annals of Mathematical Statistics}, 602--32.

}
\author{
Muge Coskun Yildirim, Osman Dag 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{rpTransform}}
%\code{\link{powerTransform}}
%}
\examples{


data <- cars$dist

library(Transform)
out <- rpTransform(data)
out$p.value # p.value of Shapiro-Wilk test for transformed data 
out$tf.data # transformed data set


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{functions}

