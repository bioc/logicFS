\name{vim.logicFS}
\alias{vim.logicFS}
\alias{logic.vim}

\title{Single and Multiple Tree Measure}
\description{
  Computes the value of the single or the multiple tree measure, respectively, for
  each prime implicant contained in a logic bagging model to specify the importance 
  of the prime implicant for classification.
}

\usage{
vim.logicFS(log.out, prob.case = 0.5, addInfo = FALSE, addMatImp = TRUE)
}

\arguments{
  \item{log.out}{an object of class \code{logicBagg}, i.e.\ the output of
     \code{logic.bagging}}
  \item{prob.case}{a numeric value between 0 and 1. If in \code{logic.bagging}
     \code{ntrees} is set to a value larger than 1, or \code{glm.if.1tree} is
     set to \code{TRUE}, then an observation will be classified as a case (or
     more exactly, as 1), if the class probability of this observation estimated
     by the logic bagging model is larger than \code{case}}
  \item{addInfo}{should further information on the logic regression models
     be added?}
  \item{addMatImp}{should the matrix containing the improvements due to the prime implicants
     in each of the iterations be added to the output? (For each of the prime implicants,
     the importance is computed by the average over the \code{B} improvements.) Must be
     set to \code{TRUE}, if standardized importances should be computed using 
     \code{\link{vim.norm}}, or if permutation based importances should be computed 
     using \code{\link{vim.perm}}} 
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants}
  \item{vim}{the importance of the prime implicants}
  \item{prop}{the proportion of logic regression models that contain the prime 
     implicants}
  \item{type}{the type of model (1: classification, 3: logistic regression)}
  \item{param}{further parameters (if \code{addInfo = TRUE})}
  \item{mat.imp}{the matrix containing the improvements if \code{addMatImp = TRUE},
    otherwise, \code{NULL}}
  \item{measure}{the name of the used importance measure}
  \item{threshold}{NULL}
  \item{mu}{NULL}
}

\references{
   Schwender, H., Ickstadt, K. (2007). Identification of SNP Interactions
   Using Logic Regression. To appear in \emph{Biostatistics}.
}

 
\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.norm}}, \code{\link{vim.perm}}
}

\keyword{logic}
\keyword{htest}
