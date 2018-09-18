\name{vim.input}
\alias{vim.individual}
\alias{vim.input}

\title{VIM for Inputs}

\description{
  Quantifies the importance of each input variable occuring in at least one
  of the logic regression models found in the application of \code{logic.bagging}.
}

\usage{
vim.input(object, useN = NULL, iter = NULL, prop = TRUE,
   standardize = NULL, mu = 0, addMatImp = FALSE, 
   prob.case = 0.5, rand = NA)
}

\arguments{
  \item{object}{an object of class \code{logicBagg}, i.e.\ the output of \code{logic.bagging}}
  \item{useN}{logical specifying if the number of correctly classified out-of-bag observations should
     be used in the computation of the importance measure. If \code{FALSE}, the proportion of
     correctly classified oob observations is used instead. If \code{NULL} (default), then
     the specification of \code{useN} in \code{object} is used.}
  \item{iter}{integer specifying the number of times the values of the considered variable
    are permuted in the computation of its importance. If \code{NULL} (default), the values
    of the variable are not permuted, but the variable is removed from the model.}
  \item{prop}{should the proportion of logic regression models containing the respective variable
    also be computed?}
  \item{standardize}{should a standardized version of the importance measure for a set of variables
    be returned? By default, \code{standardize = TRUE} is used in the classification and the (multinomial)
    logistic regression case, and \code{standarize} is set to \code{FALSE} in the linear regression case. For details, see \code{mu}.}
  \item{mu}{a non-negative numeric value. Ignored if \code{standardize = FALSE}. Otherwise, a t-statistic
    for testing the null hypothesis that the importance of the respective variable is equal to \code{mu}
    is computed.}
  \item{addMatImp}{should the matrix containing the improvements due to each of the variables in each
    of the logic regression models be added to the output?}
  \item{prob.case}{a numeric value between 0 and 1. If the logistic regression approach of logic
     regression has been used in \code{logic.bagging}, then an observation will be classified as a case (or
     more exactly, as 1), if the class probability of this observation is larger than \code{prob.case}.
     Otherwise, \code{prob.case} is ignored.}
  \item{rand}{an integer for setting the random number generator in a reproducible case.}
}

\value{
An object of class \code{logicFS} containing
  \item{vim}{the importances of the variables,}
  \item{prop}{the proportion of logic regression models containing the respective variable 
    (if \code{prop = TRUE}) or \code{NULL} (if \code{prop = FALSE}),}
  \item{primes}{the names of the variables,}
  \item{type}{the type of model (1: classification, 2:linear regression, 3: logistic regression),}
  \item{param}{further parameters (if \code{addInfo = TRUE} in the previous call of \code{logic.bagging}),}
  \item{mat.imp}{either a matrix containing the improvements due to the variables for each of the models 
    (if \code{addMatImp = TRUE}), or \code{NULL} (if \code{addMatImp = FALSE}),}
  \item{measure}{the name of the used importance measure,}
  \item{useN}{the value of \code{useN},}
  \item{threshold}{\code{NULL} if \code{standardize = FALSE}, otherwise the \eqn{1-0.05/m} quantile
     of the t-distribution with \eqn{B-1} degrees of freedom, where \eqn{m} is the number of variables and
     \eqn{B} is the number of logic regression models composing \code{object},}
  \item{mu}{\code{mu} (if \code{standardize = TRUE}), or \code{NULL} (otherwise),}
  \item{iter}{\code{iter}.}
}

\references{
  Schwender, H., Ruczinski, I., Ickstadt, K. (2011). Testing SNPs and Sets of SNPs for Importance in Association Studies. 
  \emph{Biostatistics}, 12, 18-32.
}

\author{Holger Schwender, \email{holger.schwender@hhu.de}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.set}}, \code{\link{vim.ebam}}, \code{\link{vim.chisq}}
}

\keyword{logic}
\keyword{htest}
