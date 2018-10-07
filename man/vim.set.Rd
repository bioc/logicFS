\name{vim.set}
\alias{vim.set}
\alias{vim.snp}

\title{VIM for SNPs and Sets of Variables}
\description{
  Quantifies the importances of SNPs or sets of variables, respectively, contained in a logic bagging model.
}

\usage{
  vim.snp(object, useN = NULL, iter = NULL, standardize = NULL, 
     mu = 0, addMatImp = FALSE, prob.case = 0.5, 
     score = c("DPO", "Conc", "Brier", "PL"), ensemble = FALSE, 
     rand = NULL)

  vim.set(object, set = NULL, useN = NULL, iter = NULL, standardize = NULL, 
     mu = 0, addMatImp = FALSE, prob.case = 0.5, 
     score = c("DPO", "Conc", "Brier", "PL"), ensemble = FALSE,
     rand = NULL)
}

\arguments{
  \item{object}{an object of class \code{logicBagg}, i.e.\ the output of \code{logic.bagging}.}
  \item{set}{either a list or a character or numeric vector. 
  
    If \code{NULL} (default), then it
    will be assumed that \code{data}, i.e.\ the data set used in the application of \code{logic.bagging},
    has been generated using \code{\link{make.snp.dummy}} or similar functions for coding variables
    by binary variables, i.e.\ with a function that splits a variable, say SNPx, into the dummy variables
    SNPx.1, SNPx.2, ... (where the ``." can also be any other sign, e.g., an underscore).
    
    If a character or a numeric vector,
    then the length of \code{set} must be equal to the number of variables used in \code{object},
    i.e.\ the number of columns of \code{data} in the \code{logicBagg} object, and must specify
    the set to which a variable belongs either by an integer between 1 and the number of sets, or
    by a set name. If a variable should not be included in any of the sets, set the corresponding 
    entry of \code{set} to \code{NA}. Using this specification of \code{set} it is not possible to
    assign a variable to more than one sets. For such a case, set \code{set} to a list (as follows).
    
    If \code{set} is a list, then each object in this list represents a set of variables. Therefore,
    each object must be either a character or a numeric vector specifying either the names of the variables 
    that belongs to the respective set or the columns of \code{data} that contains these variables.
    If \code{names(set)} is \code{NULL}, generic names will be employed as names for the sets. Otherwise,
    \code{names(set)} are used.}
  \item{useN}{logical specifying if the number of correctly classified out-of-bag observations should
     be used in the computation of the importance measure. If \code{FALSE}, the proportion of
     correctly classified oob observations is used instead. If \code{NULL} (default), then the
     specification of \code{useN} in \code{object} is used. In the survival case, \code{useN} is ignored.}
  \item{iter}{integer specifying the number of times the values of the variables in the respective set
    are permuted in the computation of the importance of this set. If \code{NULL} (default), the values
    of the variables are not permuted, but all variables belonging to the set are removed from the model. Permutation of variables is not available in the survival case, i.e. \code{iter} is set to \code{NULL}.}
  \item{standardize}{should a standardized version of the importance measure for a set of variables
    be returned? By default, \code{standardize = TRUE} is used in the classification and the (multinomial)
    logistic regression case, and \code{standarize} is set to \code{FALSE} in the linear regression case. Standardization is not available in the survival case. For details, see \code{mu}.}
  \item{mu}{a non-negative numeric value. Ignored if \code{standardize = FALSE}. Otherwise, a t-statistic
    for testing the null hypothesis that the importance of the respective set is equal to \code{mu}
    is computed.}
  \item{addMatImp}{should the matrix containing the improvements due to each of the sets in each
    of the logic regression models be added to the output? If \code{ensemble = TRUE} and \code{addMatImp = TRUE} in the survival case, 
     the respective score of the full model is added to the output instead of an improvement matrix.}
  \item{prob.case}{a numeric value between 0 and 1. If the logistic regression approach of logic
     regression has been used in \code{logic.bagging}, then an observation will be classified as a case (or
     more exactly, as 1), if the class probability of this observation is larger than \code{prob.case}.
     Otherwise, \code{prob.case} is ignored.}
	\item{score}{a character string naming the score that should be used in the computation of the importance measure for a survival time analysis. By default, the distance between predicted outcomes (\code{score = "DPO"}) proposed by Tietz et al.\ (2018) is used in the determination of the importance of the variables. Alternatively, Harrell's C-Index (\code{"Conc"}), the Brier score (\code{"Brier"}), or the predictive partial log-likelihood (\code{"PL"}) can be used.}
	\item{ensemble}{in the case of a survival outcome, should \code{ensemble} importance measures (as, e.g., in \code{randomSurvivalSRC} be used? If \code{FALSE}, importance measures analogous to the ones in the logicFS analysis of other outcomes are used (see Tietz et al., 2018).}	
  \item{rand}{an integer for setting the random number generator in a reproducible state.}
}

\value{
An object of class \code{logicFS} containing
  \item{vim}{the importances of the sets of variables,}
  \item{prop}{\code{NULL},}
  \item{primes}{the names of the sets of variables,}
  \item{type}{the type of model (1: classification, 2:linear regression, 3: logistic regression, 4: Cox regression),}
  \item{param}{further parameters (if \code{addInfo = TRUE} in the previous call of \code{logic.bagging}), 
    or \code{NULL} (otherwise),}
  \item{mat.imp}{either a matrix containing the improvements due to the sets of variables for each of the models 
    (if \code{addMatImp = TRUE} and \code{ensemble = FALSE}), or the respective score of the full model (if \code{addMatImp = TRUE} and \code{ensemble = TRUE}, or \code{NULL} (if \code{addMatImp = FALSE}),}
  \item{measure}{the name of the used importance measure,}
  \item{useN}{the value of \code{useN},}
  \item{threshold}{\code{NULL} if \code{standardize = FALSE}, otherwise the \eqn{1-0.05/m} quantile
     of the t-distribution with \eqn{B-1} degrees of freedom, where \eqn{m} is the number of sets and
     \eqn{B} is the number of logic regression models composing \code{object},}
  \item{mu}{\code{mu} (if \code{standardize = TRUE}), or \code{NULL} (otherwise),}
  \item{iter}{\code{iter},}
  \item{name}{\code{"Set"}.}
}

\references{
   Schwender, H., Ruczinski, I., Ickstadt, K. (2011). Testing SNPs and Sets of SNPs 
   for Importance in Association Studies. \emph{Biostatistics}, 12, 18-32.
	
	 Tietz, T., Selinski, S., Golka, K., Hengstler, J.G., Gripp, S., Ickstadt, K.,
	 Ruczinski, I., Schwender, H. (2018). Identification of Interactions of
	 Binary Variables Associated with Survival Time Using survivalFS. Submitted.
}
   
\author{Holger Schwender, \email{holger.schwender@hhu.de}; Tobias Tietz, \email{tobias.tietz@hhu.de}}

\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.input}}, \code{\link{vim.ebam}}, \code{\link{vim.chisq}}
}

\keyword{logic}
\keyword{htest}
