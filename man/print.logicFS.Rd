\name{print.logicFS}
\alias{print.logicFS}

\title{Print a logicFS object}
\description{
  Prints an object of class \code{logicFS}.
}
\usage{
\method{print}{logicFS}(x, topX = 5, show.prop = TRUE, coded = FALSE, digits = 2, ...)
}

\arguments{
  \item{x}{an object of either class \code{logicFS}.}
  \item{topX}{integer indicating how many interactions should be shown.
     Additionally to the \code{topX} most important interactions, any
     interaction having the same importance as the \code{topX} most
     important one are also shown.}
  \item{show.prop}{should the proportions of models containing the interactions
     of interest also be shown?}
  \item{coded}{should the coded variable names be displayed? Might be useful
      if the actual variable names are pretty long. The coded variable name of
      the \emph{j}-th variable is \code{Xj}.}
  \item{digits}{number of digits used in the output.}
  \item{\dots}{Ignored.}
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{
    \code{\link{logicFS}}, \code{\link{vim.logicFS}}
}

\keyword{print}
