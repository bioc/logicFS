\name{plot.logicFS}
\alias{plot.logicFS}
\alias{plot.logicBagg}

\title{Variable Importance Plot}
\description{
  Generates a dotchart of the importance of the most important
  interactions for an object of class \code{logicFS} or \code{logicBagg}.
}

\usage{
\method{plot}{logicFS}(x, topX = 15, cex = 0.9, pch = 16, col = 1, show.prop = FALSE, 
   force.topX = FALSE, coded = TRUE, add.thres = TRUE, thres = NULL, 
   include0 = TRUE, add.v0 = TRUE, v0.col = "grey50", main = NULL, ...)

\method{plot}{logicBagg}(x, topX = 15, cex = 0.9, pch = 16, col = 1, show.prop = FALSE, 
   force.topX = FALSE, coded = TRUE, include0 = TRUE, add.v0 = TRUE,
   v0.col = "grey50", main = NULL, ...)
}

\arguments{
  \item{x}{an object of either class \code{logicFS} or \code{logicBagg}.}
  \item{topX}{integer specifying how many interactions should be shown.
     If \code{topX} is larger than the number of interactions contained in
     \code{x} all the interactions are shown. For further information, 
     see \code{force.topX}.}
  \item{cex}{a numeric value specifying the relative size of the text
     and symbols.}
  \item{pch}{specifies the used symbol. See the help of \code{par} for details.}
  \item{col}{the color of the text and the symbols. See the help of \code{par} for
     how colors can be specified.}
  \item{show.prop}{if \code{TRUE} the proportions of models that contain
      the interactions of interest are shown. If \code{FALSE} (default) the
      importances of the interactions are shown.}
  \item{force.topX}{if \code{TRUE} exactly \code{topX} interactions are
      shown. If \code{FALSE} (default) all interactions up to the \code{topX}th
      most important one and all interactions having the same importance
      as the \code{topX}th most important one are shown.}
  \item{coded}{should the coded variable names be displayed? Might be useful
      if the actual variable names are pretty long. The coded variable name of
      the \emph{j}-th variable is \code{Xj}.}
  \item{add.thres}{should a vertical line marking the threshold for a prime implicant
      to be called important be drawn in the plot? If \code{TRUE}, this vertical
      line will be drawn at \code{NULL}.}
  \item{thres}{non-negative numeric value specifying the threshold for a prime implicant
      to be called important. If \code{NULL} and \code{add.thres = TRUE}, the 
      suggested threshold from \code{x} will be used.}
  \item{include0}{should the x-axis include zero regardless whether the importances
      of the shown interactions are much higher than 0?}
  \item{add.v0}{should a vertical line be drawn at \eqn{x = 0}? Ignored if 
     \code{include0 = FALSE} and all importances are larger than zero.}
  \item{v0.col}{the color of the vertical line at \eqn{x = 0}. See the help page of
     \code{par} for how colors can be specified.}
  \item{main}{character string naming the title of the plot. If \code{NULL}, the name
     of the importance measure is used.}
  \item{...}{Ignored.}
}

\author{Holger Schwender, \email{holger.schwender@hhu.de}}

\seealso{
   \code{\link{logicFS}}, \code{\link{logic.bagging}}
}

\keyword{hplot}
