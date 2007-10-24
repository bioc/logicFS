\name{vim.individual}
\alias{vim.individual}

\title{VIM for Individual Variables}

\description{
  Quantifies the importance of each individual variable occuring in at least one
  of the logic regression models found in the application of \code{logic.bagging}.
}

\usage{
vim.individual(object, iter = NULL, prop = TRUE, standardize = FALSE, 
   mu = 0, addMatImp = FALSE, prob.case = 0.5, rand = NA)
}

\arguments{
  \item{object}{an object of class \code{logicBagg}, i.e.\ the output of \code{logic.bagging}}
  \item{iter}{integer specifying the number of times the values of the considered variable
    are permuted in the computation of its importance. If \code{NULL} (default), the values
    of the variable are not permuted, but the variable is removed from the model}
  \item{prop}{should the proportion of logic regression models containing the respective variable
    also be computed?}
  \item{standardize}{should a standardized version of the individual variable importance measure
    be returned? For details, see \code{mu}}
  \item{mu}{a non-negative numeric value. Ignored if \code{standardize = FALSE}. Otherwise, a t-statistic
    for testing the null hypothesis that the importance of the respective variable is equal to \code{mu}
    is computed}
  \item{addMatImp}{should the matrix containing the improvements due to each of the variables in each
    of the logic regression models be added to the output?}
  \item{prob.case}{a numeric value between 0 and 1. If in \code{logic.bagging}
     \code{ntrees} has been set to a value larger than 1, or \code{glm.if.1tree} has been
     set to \code{TRUE}, then an observation will be classified as a case (or
     more exactly, as 1), if the class probability of this observation is larger than \code{case}}
  \item{rand}{an integer for setting the random number generator in a reproducible case}
}

\value{
An object of class \code{logicFS} containing
  \item{vim}{the importances of the variables}
  \item{prop}{the proportion of logic regression models containing the respective variable 
    (if \code{prop = TRUE}) or \code{NULL} (if \code{prop = FALSE})}
  \item{primes}{the names of the variables}
  \item{type}{the type of model (1: classification, 3: logistic regression)}
  \item{param}{further parameters (if \code{addInfo = TRUE} in the previous call of \code{logic.bagging})}
  \item{mat.imp}{either a matrix containing the improvements due to the variables for each of the models 
    (if \code{addMatImp = TRUE}), or \code{NULL} (if \code{addMatImp = FALSE})}
  \item{measure}{the name of the used importance measure}
  \item{threshold}{\code{NULL} if \code{standardize = FALSE}, otherwise the \eqn{1-0.05/m} quantile
     of the t-distribution with \eqn{B-1} degrees of freedom, where \eqn{m} is the number of variables and
     \eqn{B} is the number of logic regression models composing \code{object}}
  \item{mu}{\code{mu} (if \code{standardize = TRUE}), or \code{NULL} (otherwise)}
  \item{iter}{\code{iter}}
}

\references{Holger Schwender (2007).\ Measuring the Importances of Genotypes and Sets of Single Nucleotide
  Polymorphisms.\ Technical Report, SFB 475, Department of Statistics, University of Dortmund.\ Appears soon.}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.set}}, \code{\link{vim.ebam}}, \code{\link{vim.chisq}}
}

\keyword{logic}
\keyword{htest}
