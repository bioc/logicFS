\name{getMatEval}
\alias{getMatEval}

\title{Evaluate Prime Implicants}
\description{
  Computes the values of prime implicants for observations for which the
  values of the variables composing the prime implicants are available.
}
\usage{
getMatEval(data, vec.primes, check = TRUE)
}

\arguments{
  \item{data}{a data frame in which each row corrsponds to an observation,
     and each column to a binary variable.}
  \item{vec.primes}{a character vector naming the prime implicants that
     should be evaluated. Each of the variables composing these prime implicants must
     be represented by one column of \code{data}.}
  \item{check}{ should some checks be done before the evaluation is performed? It is
     highly recommended not to change the default \code{check = TRUE}.}
}

\value{
  a matrix in which each row corresponds to an observation (the same observations 
  in the same order as in \code{data}, and each column to one of the prime implicants.
}

\author{Holger Schwender, \email{holger.schwender@hhu.de}}

\keyword{utilities}
