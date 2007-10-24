\name{vim.set}
\alias{vim.set}

\title{VIM for Sets of Variables}
\description{
  Quantifies the importances of sets of variables contained in a logic bagging model.
}

\usage{
  vim.set(object, set = NULL, iter = NULL, standardize = FALSE, 
     mu = 0, addMatImp = FALSE, prob.case = 0.5, rand = NA)
}

\arguments{
  \item{object}{an object of class \code{logicBagg}, i.e.\ the output of \code{logic.bagging}}
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
    \code{names(set)} are used}
  \item{iter}{integer specifying the number of times the values of the variables in the respective set
    are permuted in the computation of the importance of this set. If \code{NULL} (default), the values
    of the variables are not permuted, but all variables belonging to the set are removed from the model}
  \item{standardize}{should a standardized version of the importance measure for a set of variables
    be returned? For details, see \code{mu}}
  \item{mu}{a non-negative numeric value. Ignored if \code{standardize = FALSE}. Otherwise, a t-statistic
    for testing the null hypothesis that the importance of the respective set is equal to \code{mu}
    is computed}
  \item{addMatImp}{should the matrix containing the improvements due to each of the sets in each
    of the logic regression models be added to the output?}
  \item{prob.case}{a numeric value between 0 and 1. If in \code{logic.bagging}
     \code{ntrees} has been set to a value larger than 1, or \code{glm.if.1tree} has been
     set to \code{TRUE}, then an observation will be classified as a case (or
     more exactly, as 1), if the class probability of this observation is larger than \code{case}}
  \item{rand}{an integer for setting the random number generator in a reproducible case}
}

\value{
An object of class \code{logicFS} containing
  \item{vim}{the importances of the sets of variables}
  \item{prop}{NULL}
  \item{primes}{the names of the sets of variables}
  \item{type}{the type of model (1: classification, 3: logistic regression)}
  \item{param}{further parameters (if \code{addInfo = TRUE} in the previous call of \code{logic.bagging}), 
    or \code{NULL} (otherwise)}
  \item{mat.imp}{either a matrix containing the improvements due to the sets of variables for each of the models 
    (if \code{addMatImp = TRUE}), or \code{NULL} (if \code{addMatImp = FALSE})}
  \item{measure}{the name of the used importance measure}
  \item{threshold}{\code{NULL} if \code{standardize = FALSE}, otherwise the \eqn{1-0.05/m} quantile
     of the t-distribution with \eqn{B-1} degrees of freedom, where \eqn{m} is the number of sets and
     \eqn{B} is the number of logic regression models composing \code{object}}
  \item{mu}{\code{mu} (if \code{standardize = TRUE}), or \code{NULL} (otherwise)}
  \item{iter}{\code{iter}}
}

\references{Holger Schwender (2007).\ Measuring the Importances of Genotypes and Sets of Single Nucleotide
  Polymorphisms.\ Technical Report, SFB 475, Department of Statistics, University of Dortmund.\ Appears soon.}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\seealso{
   \code{\link{logic.bagging}}, \code{\link{logicFS}},
   \code{\link{vim.logicFS}}, \code{\link{vim.set}}, \code{\link{vim.ebam}}, \code{\link{vim.chisq}}
}

\keyword{logic}
\keyword{htest}
