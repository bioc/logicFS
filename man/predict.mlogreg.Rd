\name{predict.mlogreg}
\alias{predict.mlogreg}

\title{Predict Method for mlogreg Objects}
\description{
   Prediction for test data using an object of class \code{mlogreg}.
}
\usage{
 \method{predict}{mlogreg}(object, newData, type = c("class", "prob"), ...)
}

\arguments{
  \item{object}{an object of class \code{mlogreg}, i.e.\ the output of the
     function \code{mlogreg}.}
  \item{newData}{a matrix or data frame containing new data. If omitted
     \code{object\$data}, i.e.\ the original training data, are used. 
     Each row of \code{newData} must correspond
     to a new observation. Each row of \code{newData} must contain the same
     variable as the corresponding column of the data matrix used in \code{\link{mlogreg}},
     i.e.\ \code{x} if the default method of \code{\link{mlogreg}} has been used, or
     \code{data} \emph{without} the column containing the response if the \code{formula} method
     has been used.}
  \item{type}{character vector indicating the type of output. If \code{"class"}, a
     vector containing the predicted classes of the observations will be returned. 
     If \code{"prob"}, the class probabilities for each level and all observations are returned.}
  \item{...}{Ignored.}
}

\value{
  A numeric vector containing the predicted classes (if \code{type = "class"}), or a matrix composed of the
  class probabilities (if \code{type = "prob"}).
}
   
\author{Holger Schwender, \email{holger.schwender@hhu.de}}

\seealso{
   \code{\link{mlogreg}}
}

\keyword{regression}
