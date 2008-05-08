\name{vim.ebam}
\alias{vim.ebam}

\title{EBAM Based Importance}
\description{
  Determines the importance of interactions found by \code{logic.bagging} or \code{logicFS}
  by an Empirical Bayes Analysis of Microarrays (EBAM). Only available for the classification
  and the logistic regression approach of logic regression.
}
\usage{
vim.ebam(object, data = NULL, cl = NULL, nameEBAM = NULL, ...)
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
  \item{nameEBAM}{a character string. If specified, then the output of the EBAM analysis is stored under
     this name in the global environment.}
  \item{\dots}{further arguments of \code{ebam} and \code{cat.ebam}. For details, see the help files
     of these functions from the package \code{siggenes}.}
}

\details{
  For each interaction found by \code{logic.bagging} or \code{logicFS}, the posterior probability
  that this interaction is significant is computed using the Empirical Bayes Analysis of Microarrays (EBAM).
  These posterior probabilities are used as the EBAM based importances of the interactions.
  
  The test statistic underlying this EBAM analysis is Pearson's ChiSquare statistic. Currently,
  the value of this statistic is computed without continuity correction.
    
  Contrary to \code{\link{vim.logicFS}} (and \code{\link{vim.norm}} and \code{\link{vim.perm}}),
  \code{vim.ebam} does neither take the logic regression models into acount nor uses the out-of-bag
  observations for computing the importances of the identified interactions. It "just" tests each
  of the found interactions on the whole data set by calculating Pearson's ChiSquare statistic for
  each of these interactions and performing an EBAM analysis. It is, therefore, highly recommended 
  to use an independent data set for specifying the importances of these interactions with \code{vim.ebam}.
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants,}
  \item{vim}{the posterior probabilities of the interactions,}
  \item{prop}{NULL,}
  \item{type}{NULL,}
  \item{param}{further parameters (if \code{object} is the output of \code{logicFS} or \code{vim.logicFS} 
     with \code{addInfo = TRUE}),}
  \item{mat.imp}{NULL,}
  \item{measure}{"EBAM Based",}
  \item{threshold}{the value of \code{delta} used in the EBAM analysis (see help files for \code{ebam}); 
    by default: 0.9,}
  \item{mu}{NULL.}
}

\references{
  Schwender, H. and Ickstadt, K. (2008). Empirical Bayes Analysis of 
  Single Nucleotide Polymorphisms. \emph{BMC Bioinformatics}, 9:144.
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.norm}}, \code{\link{vim.chisq}}
}

\keyword{logic}
\keyword{htest}
