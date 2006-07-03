\name{predict.logicBagg}
\alias{predict.logicBagg}
\title{Predict Method for logicBagg objects}
\description{
  Prediction of test data Using an object of class \code{logicBagg}.
}
\usage{
\method{predict}{logicBagg}(object, newData, prob.case = 0.5, ...)
}

\arguments{
  \item{object}{an object of class \code{logicBagg}.}
  \item{newData}{a matrix containing new data. If omitted
     \code{object\$data}, i.e.\ the original training data, are used. 
     Each row of \code{newData} must correspond
     to a new observation. \code{newData} must have the same number
     of columns as \code{object\$data}, and the order of the variables
     must be same in both matrices. Warning: Currently, there will be
     no checking if the latter is \code{TRUE}.}
  \item{prob.case}{a numeric value between 0 and 1. If the outcome of the
     logistic regression, i.e.\ the predicted probability, for an observation is
     larger than \code{prob.case} this observations will be classified as case 
     (or 1).}
  \item{...}{Ignored.}
}

\value{
  A numeric vector containing the predicted classes of the new observations.
}
   
\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{logic.bagging}}
}

\keyword{regression}
