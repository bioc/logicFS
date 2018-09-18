\name{vim.chisq}
\alias{vim.chisq}

\title{ChiSquare Based Importance}
\description{
  Determining the importance of interactions found by \code{logic.bagging} or \code{logicFS}
  by Pearson's ChiSquare Statistic. Only available for the classification and the logistic
  regression approach of logic regression.
}
\usage{
vim.chisq(object, data = NULL, cl = NULL)
}

\arguments{
  \item{object}{either an object of class \code{logicFS} or the output of an application of 
     \code{logic.bagging} with \code{importance = TRUE}.}
  \item{data}{a data frame or matrix consisting of 0's and 1's in which each column corresponds
     to one of the explanatory variables used in the original analysis with \code{logic.bagging} 
     or \code{logicFS}, and each row corresponds to an observation. Must be specified if
     \code{object} is an object of class \code{logicFS}, or \code{cl} is specified. If
     \code{object} is an object of class \code{logicBagg} and neither \code{data} nor \code{cl}
     is specified, \code{data} and \code{cl} stored in \code{object} is used to compute the
     ChiSquare statistics. It is, however, highly recommended to use new \code{data} to test
     the interactions contained in \code{object}, as they have been found using the \code{data}
     stored in \code{object}, and it is very likely that most of them will show up as interesting
     if they are tested on the same data set.}
  \item{cl}{a numeric vector of 0's and 1's specifying the class labels of the observations in \code{data}.
     Must be specified either if \code{object} is an object of class \code{logicFS}, or if \code{data}
     is specified.}
}

\details{
  Currently Pearson's ChiSquare statistic is computed without continuity correction.
  
  Contrary to \code{\link{vim.logicFS}} (and \code{\link{vim.norm}} and \code{\link{vim.signperm}}),
  \code{vim.chisq} does neither take the logic regression models into acount nor uses the out-of-bag
  observations for computing the importances of the identified interactions. It "just" tests each
  of the found interactions on the whole data set by calculating Pearson's ChiSquare statistic for
  each of these interactions. It is, therefore, highly recommended to use an independent data set
  for specifying the importances of these interactions with \code{vim.chisq}.
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants}
  \item{vim}{the values of Pearson's ChiSquare statistic,}
  \item{prop}{NULL,}
  \item{type}{NULL,}
  \item{param}{further parameters (if \code{object} is the output of \code{logicFS} or \code{vim.logicFS} 
     with \code{addInfo = TRUE}),}
  \item{mat.imp}{NULL,}
  \item{measure}{"ChiSquare Based",}
  \item{threshold}{the 1 - 0.05/m quantile of the ChiSquare distribution with one degree of freedom,}
  \item{mu}{NULL.}
}

\author{Holger Schwender, \email{holger.schwender@hhu.de}}

\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.norm}}, \code{\link{vim.ebam}}
}

\keyword{logic}
\keyword{htest}
