\name{logic.oob}
\alias{logic.oob}

\title{Prime Implicants}
\description{
  Computes the out-of-bag error of the classification rule comprised by
  a \code{logicBagg} object.
}
\usage{
logic.oob(log.out, prob.case = 0.5)
}

\arguments{
  \item{log.out}{an object of class \code{logicBagg}, i.e.\ the output of
     \code{logic.bagging}}
  \item{prob.case}{a numeric value between 0 and 1. If the logic regression models
     are logistic regression models, i.e.\ if in \code{logic.bagging} \code{ntree}
     is set to a value larger than 1, or \code{glm.if.1tree} is set to \code{TRUE},
     then an observation will be classified as case (or more exactly, as 1)
     if the class probability is larger than \code{prob.case}}
}

\value{
  The out-of-bag error estimate.
}

  
\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{logic.bagging}}
}

\keyword{utilities}
\keyword{htest}
