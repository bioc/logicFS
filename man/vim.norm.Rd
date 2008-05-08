\name{vim.norm}
\alias{vim.norm}
\alias{vim.perm}

\title{Standardized and Permutation Based Importance Measure}

\description{
  Computes a standarized or a permutation based version of either the Single Tree Measure,
  the Quantitative Response Measure, or the Multiple Tree Measure.
}

\usage{
vim.norm(object, mu = 0)

vim.perm(object, mu = 0, n.perm = 10000, n.subset = 1000, 
  adjust = "bonferroni", rand = NA)
}

\arguments{
  \item{object}{either the output of \code{\link{logicFS}} or \code{\link{vim.logicFS}}
     with \code{addMatImp = TRUE}, or the output of \code{\link{logic.bagging}}
     with \code{importance = TRUE} and \code{addMatImp = TRUE}.}
  \item{mu}{a non-negative numeric value. Default is zero. However, \code{mu} should actually
     be set to a value larger than zero. See \code{Details}.}
  \item{n.perm}{the number of (sign) permutations used in \code{vim.perm}.}
  \item{n.subset}{an integer specifying how many permutations should be considered at once.}
  \item{adjust}{character vector naming the method with which the raw permutation based
     p-values are adjusted for multiplicity. If \code{"qvalue"}, the function \code{qvalue.cal}
     from the package \code{siggenes} is used to compute q-values. Otherwise, 
     \code{p.adjust} is used to adjust for multiple comparisons. See \code{p.adjust} for all
     other possible specifications of \code{adjust}. If \code{"none"}, the raw p-values will
     be used. For more details, see \code{Details}.}
  \item{rand}{an integer for setting the random number generator in a reproducible case.}
}
\details{
  In both \code{vim.norm} and \code{vim.perm}, an one-sample t-statistic is computed for each
  prime implicant, where the numerator is given by \eqn{VIM - }\code{mu} with VIM being the
  single or the multiple tree importance, and the denominator is the corresponding standard
  error computed by employing the \code{B} improvements of the considered prime implicant
  in the \code{B} logic regression models. (Note that VIM is the mean over these
  \code{B} improvements.)
  
  As using \code{mu = 0} might lead to calling a prime implicant important, even though it actually
  shows only improvements of 1 or 0, \code{mu} should be set to a value larger than zero. 
  %A rule of thumb might be to set \code{mu} to about one third of \code{diff}, where a prime implicant
  %should explain, i.e.\ be true for, at least \code{diff} more cases than controls to be considered
  %as important.
  
  In \code{vim.norm}, the value of this t-statistic is returned as the standardized importance
  of a prime implicant. The larger this value, the more important is the prime implicant. (This applies
  to all importance measures -- at least for those contained in this package.) Assuming normality,
  a possible threshold for a prime implicant to be considered as important is the \eqn{1-0.05/m} quantile
  of the t-distribution with \eqn{B-1} degrees of freedom, where \eqn{m} is the number of prime implicants.
  
  In \code{vim.perm}, the sign permutation is used to determine \code{n.perm} permuted values of the
  one-sample t-statistic, and to compute the raw p-values for each of the prime implicants. Afterwards,
  these p-values are adjusted for multiple comparisons using the method specified by \code{adjust}.
  The permutation based importance of a prime implicant is then given by \eqn{1-} these adjusted p-values. 
  Here, a possible threshold for calling a prime implicant important is 0.95.
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants,}
  \item{vim}{the respective importance of the prime implicants,}
  \item{prop}{NULL,}
  \item{type}{the type of model (1: classification, 2: linear regression, 3: logistic regression),}
  \item{param}{further parameters (if \code{addInfo = TRUE}),}
  \item{mat.imp}{NULL,}
  \item{measure}{the name of the used importance measure,}
  \item{useN}{the value of \code{useN} from the original analysis with, e.g., \code{\link{logicFS}},}
  \item{threshold}{the threshold suggested in \code{Details},}
  \item{mu}{\code{mu}.}
}

\references{
   Schwender, H. (2007). Statistical Analysis of Genotype and Gene Expression Data.
   \emph{Dissertation}, Department of Statistics, University of Dortmund, Dortmund, Germany.
}
\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.chisq}}, \code{\link{vim.ebam}}
}

\keyword{logic}
\keyword{htest}
