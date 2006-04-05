\name{logic.vim}
\alias{logic.vim}
\alias{logic.oob}
\alias{logic.pimp}

\title{Prime Implicants}
\description{
  \code{logic.pimp} computes the prime implicants of an object of class 
  \code{logicBagg}.
  
  \code{logic.vim} additionally computes the importance of the prime
  implicants.
  
  \code{logic.oob} computes the out-of-bag error of a \code{logicBagg}
  object.
}
\usage{
logic.pimp(log.out)
logic.oob(log.out, prob.case = 0.5)
logic.vim(log.out, prob.case = 0.5, addInfo = FALSE)
}

\arguments{
  \item{log.out}{an object of class \code{logicBagg}.}
  \item{prob.case}{a numeric value between 0 and 1. If the outcome of the
     logistic regression, i.e.\ the predicted probability, for an observation is
     larger than \code{prob.case} this observations will be classified as case 
     (or 1).}
  \item{addInfo}{should further information on the logic regression models
     be added?}
}

\value{
  \code{logic.pimp} returns a list consisting of the prime implicants for
  each of the \code{B} logic regression models of \code{log.out}.
  
  \code{logic.oob} returns the out-of-bag error.
  
  \code{logic.vim} returns an object of class \code{logicFS} containing
  \item{primes}{the prime implicants}
  \item{vim}{the importance of the prime implicants}
  \item{prop}{the proportion of logic regression models that contain the prime 
     implicants}
  \item{type}{the type of model (1: classification, 3: logistic regression)}
  \item{param}{further parameters (if \code{addInfo=TRUE}).}
}

\details{
  Since we are interested in all potentially interested SNP interactions and not
  in a minimum set of them both \code{logic.pimp} and \code{logic.vim} return all
  prime implicants and not a minimum number of them.
}
  
\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\note{ 
  Tech. Report on the feature selection using logic regression will be
  available soon.
}

\seealso{
   \code{\link{logic.bagging}}, \code{\link{logic.fs}},
   \code{\link{prime.implicants}}
}

\keyword{utilities}
\keyword{logic}
\keyword{htest}
