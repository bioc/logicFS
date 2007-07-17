\name{logic.pimp}
\alias{logic.pimp}

\title{Prime Implicants}
\description{
  Determines the prime implicants contained in the logic regression models
  comprised in an object of class \code{logicBagg}.
}
\usage{
logic.pimp(log.out)
}

\arguments{
  \item{log.out}{an object of class \code{logicBagg}, i.e.\ the output
     of \code{logic.bagging}}
}

\value{
  A list consisting of the prime implicants for
  each of the \code{B} logic regression models of \code{log.out}.
}

\details{
  Since we are interested in all potentially interested interactions and not
  in a minimum set of them, \code{logic.pimp} and returns all
  prime implicants and not a minimum number of them.
}
  
\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{prime.implicants}}
}

\keyword{utilities}
\keyword{logic}
