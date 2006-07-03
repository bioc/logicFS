\name{plot.logicFS}
\alias{plot.logicFS}
\alias{plot.logicBagg}

\title{Variable Importance Plot}
\description{
  Generates a dotchart of the importance of the most important
  interactions for an object of class \code{logicFS} or \code{logicBagg}.
}

\usage{
\method{plot}{logicFS}(x, topX = 15, cex = 0.9, pch = 16, col = 1, v0.col = "grey35",
   show.prop = FALSE, force.topX = FALSE, include0 = TRUE, coded = TRUE, ...)

\method{plot}{logicBagg}(x, topX = 15, cex = 0.9, pch = 16, col = 1, v0.col = "grey35",
   show.prop = FALSE, force.topX = FALSE, include0 = TRUE, coded = TRUE, ...)
}

\arguments{
  \item{x}{an object of either class \code{logicFS} or \code{logicBagg}.}
  \item{topX}{integer specifying how many interactions should be shown.
     If \code{topX} is larger than the number of interactions contained in
     \code{x} all the interactions are shown. For further information, 
     see \code{force.topX}.}
  \item{cex}{a numeric value specifying the relative size of the text
     and symbols}
  \item{pch}{specifies the used symbol. See \code{?par} for details.}
  \item{col}{the color of the text and the symbols. See \code{?par} for
     how colors can be specified.}
  \item{v0.col}{the color of the vertical line at \eqn{x = 0}. See
     \code{?par} for how colors can be specified.}
  \item{show.prop}{if \code{TRUE} the proportions of models that contain
      the interactions of interest are shown. If \code{FALSE} (default) the
      importances of the interactions are shown.}
  \item{force.topX}{if \code{TRUE} exactly \code{topX} interactions are
      shown. If \code{FALSE} (default) all interactions up to the \code{topX}th
      most important one and all interactions having the same importance
      as the \code{topX}th most important one} are shown.
  \item{include0}{should \eqn{x=0} be included regardless whether the importances
      of the shown interactions are much higher than 0?}
  \item{coded}{should the coded variable names be displayed? Might be useful
      if the actual variable names are pretty long. The coded variable name of
      the \emph{y}th variable is "Xy".}
  \item{...}{Ignored}
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{logic.fs}}, \code{\link{logic.bagging}}
}

\keyword{hplot}
