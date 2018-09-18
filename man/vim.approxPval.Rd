\name{vim.approxPval}
\alias{vim.approxPval}

\title{
Approximate P-Value Based Importance Measure
}
\description{
Computes the importances based on an approximation to a t- or F-distribution.
}
\usage{
vim.approxPval(object, version = 1, adjust = "bonferroni")
}

\arguments{
  \item{object}{an object of class \code{logicFS} which contains the values of standardized importances.
    Only in the linear regression case, the importances in \code{object} are allowed to be non-standardized.
}
\item{version}{either \code{1} or \code{2}. If \code{1}, then the importance measure is computed
  by 1 - padj, where padj is the adjusted p-value. If \code{2}, the importance measure is determined
  by -log10(padj), where a raw p-value equal to 0 is set to 1 / (10 * \code{n.perm}) to avoid
  infinitive importances.
}
  \item{adjust}{character vector naming the method with which the raw permutation based
    p-values are adjusted for multiplicity. If \code{"qvalue"}, the function \code{qvalue.cal}
    from the package \code{siggenes} is used to compute q-values. Otherwise,
    \code{p.adjust} is used to adjust for multiple comparisons. See \code{p.adjust} for all
    other possible specifications of \code{adjust}. If \code{"none"}, the raw p-values will
    be used.
}
}
\value{
An object of class \code{logicFS} containing the same object as \code{object} except for
  \item{vim}{the values of the importance measure based on an approximation to the t- or F-distribution,}
  \item{measure}{the name of the used importance measure,}
  \item{threshold}{0.95 if \code{version = 1}, and -log10(0.05) if \code{version = 2}.}
}
\references{
  Schwender, H., Ruczinski, I., Ickstadt, K. (2011). Testing SNPs and Sets of SNPs for Importance in Association Studies. 
  \emph{Biostatistics}, 12, 18-32.
}
\author{
   Holger Schwender, \email{holger.schwender@hhu.de}
}

\seealso{
  \code{\link{logic.bagging}}, \code{\link{logicFS}}, \code{\link{vim.input}}, \code{\link{vim.set}}, \code{\link{vim.permSet}}
}

\keyword{logic}
\keyword{htest}
