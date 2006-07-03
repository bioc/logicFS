\name{minDNF}
\alias{minDNF}
\alias{prime.implicants}
\alias{print.minDNF}
\alias{print.primeImp}
\title{Minimum Disjunctive Normal Form}
\description{
  Computes the prime implicants or the minimal disjuntive form, respectively,
  of a given truth table.
}
\usage{
  prime.implicants(mat)
  minDNF(mat)
}
\arguments{
  \item{mat}{a matrix containing only 0's and 1's. Each column of \code{mat}
     corresponds to a binary variable and each row to a combination of the
     variables for which the logic expression is TRUE.}
}


\value{
  Either an object of class \code{minDNF} or of class \code{primeImp}. Both contain
  a vector of (a minimum number of) prime implicants. The \code{primeImp} additionally
  contains the prime implicant table.
}

\details{
  \code{minDNF} is a fast implementation of the Quine-McCluskey algorithm using matrix
  algebra. A Tech. Report on this implementation will follow soon.
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{logic.pimp}}
}

\keyword{optimize}
\keyword{logic}
\keyword{print}
