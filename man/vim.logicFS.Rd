\name{vim.logicFS}
\alias{vim.logicFS}

\title{Importance Measures}
\description{
  Computes the value of the single or the multiple tree measure, respectively, for
  each prime implicant contained in a logic bagging model to specify the importance 
  of the prime implicant for classification, if the response is binary. If the response
  is quantitative, the importance is specified by a measure based on the log2-transformed 
  mean square prediction error. If the response is a time to an event, performance measures for time-to-event models are employed to determine the importance measures. 
}

\usage{
vim.logicFS(log.out, neighbor = NULL, adjusted = FALSE, useN = TRUE, 
   onlyRemove = FALSE, prob.case = 0.5, addInfo = FALSE, 
	 score = c("DPO", "Conc", "Brier", "PL"), ensemble = FALSE, 
	 addMatImp = TRUE)
}

\arguments{
  \item{log.out}{an object of class \code{logicBagg}, i.e.\ the output of
     \code{logic.bagging}.}
	\item{neighbor}{a list consisting of character vectors specifying SNPs that are in LD. If specified, all SNPs need to occur exactly one time in this list. If specified, the importance measures are adjusted for LD by considering the SNPs within a LD block as exchangable.}
	\item{adjusted}{logical specifying whether the measures should be adjusted for noise. Often, the interaction actually associated with the response is not exactly found in some iterations of logic bagging, but an interaction is identified that additionally contains one (or seldomly more) noise SNPs. If \code{adjusted} is set to \code{TRUE}, the values of the importance measure is corrected for this behaviour.}
  \item{useN}{logical specifying if the number of correctly classified out-of-bag observations should
     be used in the computation of the importance measure. If \code{FALSE}, the proportion of
     correctly classified oob observations is used instead. Ignored in the survival case.}
  \item{onlyRemove}{should in the single tree case the multiple tree measure be used? If \code{TRUE},
     the prime implicants are only removed from the trees when determining the importance in the
     single tree case. If \code{FALSE}, the original single tree measure is computed for each prime
     implicant, i.e.\ a prime implicant is not only removed from the trees in which it is contained,
     but also added to the trees that do not contain this interaction. Ignored in all other than the
     classification case.}
  \item{prob.case}{a numeric value between 0 and 1. If the logistic regression approach
     of logic regression is used (i.e.\ if the response is binary, and in \code{logic.bagging}
     \code{ntrees} is set to a value larger than 1, or \code{glm.if.1tree} is
     set to \code{TRUE}), then an observation will be classified as a case (or
     more exactly as 1), if the class probability of this observation estimated
     by the logic bagging model is larger than \code{prob.case}.}
  \item{addInfo}{should further information on the logic regression models
     be added?}
		\item{score}{a character string naming the score that should be used in the computation of the importance measure for a survival time analysis. By default, the distance between predicted outcomes (\code{score = "DPO"}) proposed by Tietz et al.\ (2018) is used in the determination of the importance of the variables. Alternatively, Harrell's C-Index (\code{"Conc"}), the Brier score (\code{"Brier"}), or the predictive partial log-likelihood (\code{"PL"}) can be used.}
		\item{ensemble}{in the case of a survival outcome, should \code{ensemble} importance measures (as, e.g., in \code{randomSurvivalSRC} be used? If \code{FALSE}, importance measures analogous to the ones in the logicFS analysis of other outcomes are used (see Tietz et al., 2018).}
  \item{addMatImp}{should the matrix containing the improvements due to the prime implicants
     in each of the iterations be added to the output? (For each of the prime implicants,
     the importance is computed by the average over the \code{B} improvements.) Must be
     set to \code{TRUE}, if standardized importances should be computed using 
     \code{\link{vim.norm}}, or if permutation based importances should be computed 
     using \code{\link{vim.signperm}}. If \code{ensemble = TRUE} and \code{addMatImp = TRUE} in the survival case, 
     the respective score of the full model is added to the output instead of an improvement matrix.} 
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants,}
  \item{vim}{the importance of the prime implicants,}
  \item{prop}{the proportion of logic regression models containing the prime implicants (or the neighbors of the prime implicants, if \code{neighbor != NULL}; or the extended primes of the prime implicants, if \code{adjusted = TRUE}; or the extended primes of the neighbors of the prime implicants, if \code{neighbor != NULL} and \code{adjusted = TRUE}),}
  \item{type}{the type of model (1: classification, 2: linear regression, 3: logistic regression, 4: Cox regression),}
  \item{param}{further parameters (if \code{addInfo = TRUE}),}
  \item{mat.imp}{either the matrix containing the improvements if \code{addMatImp = TRUE} and \code{ensemble = FALSE}, or the respective score of the full model if \code{addMatImp = TRUE} and \code{ensemble = TRUE}, or \code{NULL} if \code{addMatImp = FALSE},}
  \item{measure}{the name of the used importance measure,}
  \item{neighbor}{\code{neighbor},} 
  \item{useN}{the value of \code{useN},}
  \item{threshold}{NULL,}
  \item{mu}{NULL.}
}

\references{
   Schwender, H., Ickstadt, K. (2007). Identification of SNP Interactions
   Using Logic Regression. \emph{Biostatistics}, 9(1), 187-198.
   
	 Tietz, T., Selinski, S., Golka, K., Hengstler, J.G., Gripp, S., Ickstadt, K.,
	 Ruczinski, I., Schwender, H. (2018). Identification of Interactions of
	 Binary Variables Associated with Survival Time Using survivalFS. Submitted.
}
   
\author{Holger Schwender, \email{holger.schwender@hhu.de}; Tobias Tietz, \email{tobias.tietz@hhu.de}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.norm}}, \code{\link{vim.signperm}}
}

\keyword{logic}
\keyword{htest}
