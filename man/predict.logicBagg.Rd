\name{predict.logicBagg}
\alias{predict.logicBagg}
\title{Predict Method for logicBagg objects}
\description{
  Prediction for test data using an object of class \code{logicBagg}.
}
\usage{
\method{predict}{logicBagg}(object, newData, prob.case = 0.5, 
    type = c("class", "prob"), score = c("DPO", "Conc", "Brier"), ...)
}

\arguments{
  \item{object}{an object of class \code{logicBagg}.}
  \item{newData}{a matrix or data frame containing new data. If omitted
     \code{object\$data}, i.e.\ the original training data, are used. 
     Each row of \code{newData} must correspond
     to a new observation. Each row of \code{newData} must contain the same
     variable as the corresponding column of the data matrix used in \code{\link{logic.bagging}},
     i.e.\ \code{x} if the default method of \code{\link{logic.bagging}} has been used, or
     \code{data} \emph{without} the column containing the response if the \code{formula} method
     has been used.}
  \item{prob.case}{a numeric value between 0 and 1. A new observation will be
     classified as case (or more exactly, as 1) if the class probability, i.e.\
     the average of the predicted probabilities of the models (if the logistic
     regression approach of logic regression has been used), or the percentage of
     votes for class 1 (if the classification approach of logic regression has been used)
     is larger than \code{prob.case}. Ignored if \code{type = "prob"} or the response is either quantitative or an object of class \code{Surv}.}
  \item{type}{character vector indicating the type of output. If \code{"class"}, a numeric
     vector of zeros and ones containing the predicted classes of the observations (using the specification
     of \code{prob.case}) will be returned. If \code{"prob"}, the class probabilities or
     percentages of votes for class 1, respectively, for all observations are returned. Ignored
     if the response is quantitative or an object of class \code{Surv}.}
  \item{score}{a character string naming the score that should be used to assess the performance of the prediction model in the survival case. By default, the distance between predicted outcomes (\code{score = "DPO"}) proposed by Tietz et al.\ (2018) is used in the assessment of the prediction performance. Alternatively, Harrell's C-Index (\code{"Conc"}), or the Brier score (\code{"Brier"}) can be used. Furthermore, \code{score} determines whether a prediction for the cumulative hazard function (\code{score = "DPO"} or \code{score = "Conc"}) or the survival function (\code{score = "Brier"}) of the new observations should be made. Ignored in all other than the survival case.}
  \item{...}{Ignored.}
}

\value{
  A numeric vector containing the predicted classes (if \code{type = "class"}) or the
  class probabilities (if \code{type = "prob"}) of the new observations if the classification
  or the logistic regression approach of logic regression is used. If the response is quantitative,
  the predicted value of the response for all observations in the test data set is returned. If the
  response is of class \code{Surv}, an object of class \code{predict.survivalFS} with either an 
  prediction for the cumulative hazard function or the survival function of the new observations is returned.
}
   
\author{Holger Schwender, \email{holger.schwender@hhu.de}, Tobias Tietz, \email{tobias.tietz@hhu.de}}

\seealso{
   \code{\link{logic.bagging}}
}

\keyword{regression}
