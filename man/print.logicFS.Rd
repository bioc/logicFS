\name{print.logicFS}
\alias{print.logicFS}

\title{Print an logicFS object}
\description{
  This functions prints an object of class \code{logicFS}.
}
\usage{
\method{print}{logicFS}(x, topX = 5, show.prop = TRUE, coded = FALSE, ...)
}

\arguments{
  \item{x}{an object of class \code{logicFS}}
  \item{topX}{integer indícating how many interactions should
     be shown. Additionally to the \code{topX} most important
     interactions, any interaction having the same importance
     as the \code{topX} most important one is also shown}
  \item{show.prop}{should the proportions of models containing
     the interactions of interests also be shown?}
  \item{coded}{should the coded variable names be displayed? Might be useful
      if the actual variable names are pretty long. The coded variable name of
      the \emph{y}th variable is "Xy".}
  \item{...}{Ignored}
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
   \code{\link{logic.fs}}, \code{\link{logic.vim}}
}
\keyword{print}
