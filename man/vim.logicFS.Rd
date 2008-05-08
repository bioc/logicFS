\name{vim.logicFS}
\alias{vim.logicFS}

\title{Importance Measures}
\description{
  Computes the value of the single or the multiple tree measure, respectively, for
  each prime implicant contained in a logic bagging model to specify the importance 
  of the prime implicant for classification, if the response is binary. If the response
  is quantitative, the importance is specified by a measure based on the mean square
  prediction error.
}

\usage{
vim.logicFS(log.out, useN = TRUE, prob.case = 0.5, addInfo = FALSE, addMatImp = TRUE)
}

\arguments{
  \item{log.out}{an object of class \code{logicBagg}, i.e.\ the output of
     \code{logic.bagging}.}
  \item{useN}{logical specifying if the number of correctly classified out-of-bag observations should
     be used in the computation of the importance measure. If \code{FALSE}, the proportion of
     correctly classified oob observations is used instead.}
  \item{prob.case}{a numeric value between 0 and 1. If the logistic regression approach
     of logic regression is used (i.e.\ if the response is binary, and in \code{logic.bagging}
     \code{ntrees} is set to a value larger than 1, or \code{glm.if.1tree} is
     set to \code{TRUE}), then an observation will be classified as a case (or
     more exactly as 1), if the class probability of this observation estimated
     by the logic bagging model is larger than \code{prob.case}.}
  \item{addInfo}{should further information on the logic regression models
     be added?}
  \item{addMatImp}{should the matrix containing the improvements due to the prime implicants
     in each of the iterations be added to the output? (For each of the prime implicants,
     the importance is computed by the average over the \code{B} improvements.) Must be
     set to \code{TRUE}, if standardized importances should be computed using 
     \code{\link{vim.norm}}, or if permutation based importances should be computed 
     using \code{\link{vim.perm}}.} 
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants,}
  \item{vim}{the importance of the prime implicants,}
  \item{prop}{the proportion of logic regression models containing the prime implicants,}
  \item{type}{the type of model (1: classification, 2: linear regression, 3: logistic regression),}
  \item{param}{further parameters (if \code{addInfo = TRUE}),}
  \item{mat.imp}{the matrix containing the improvements if \code{addMatImp = TRUE},
    otherwise, \code{NULL},}
  \item{measure}{the name of the used importance measure,}
  \item{useN}{the value of \code{useN},}
  \item{threshold}{NULL,}
  \item{mu}{NULL.}
}

\references{
   Schwender, H., Ickstadt, K. (2007). Identification of SNP Interactions
   Using Logic Regression. \emph{Biostatistics}, doi:10.1093/biostatistics/kxm024.
}

 
\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.norm}}, \code{\link{vim.perm}}
}

\keyword{logic}
\keyword{htest}
