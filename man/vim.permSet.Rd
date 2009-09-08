\name{vim.permSNP}
\alias{vim.permSet}
\alias{vim.permSNP}
\alias{vim.permInput}

\title{
Permutation Based Importance Measures
}
\description{
Computes the importances of input variables, SNPs, or sets of SNPs, respectively, based on permutations of the response.
Currently only available for the classification and the logistic regression approach of logic regression.
}
\usage{
  vim.permInput(object, n.perm = NULL, standardize = TRUE, 
    rebuild = FALSE, prob.case = 0.5, useAll = FALSE, 
    adjust = "bonferroni", addMatPerm = FALSE, rand=NA)

  vim.permSNP(object, n.perm = NULL, standardize = TRUE,
     rebuild = FALSE, prob.case = 0.5, useAll = FALSE,
     adjust = "bonferroni", addMatPerm = FALSE, rand = NA)

  vim.permSet(object, set = NULL, n.perm = NULL, standardize = TRUE,
     rebuild = FALSE, prob.case = 0.5, useAll = FALSE,
     adjust = "bonferroni", addMatPerm = FALSE, rand = NA)
}

\arguments{
  \item{object}{
    an object of class \code{logicBagg}, i.e.\ the output of \code{\link{logic.bagging}}.
}
  \item{set}{either a list or a character or numeric vector. 
  
    If \code{NULL} (default), then it
    will be assumed that \code{data}, i.e.\ the data set used in the application of \code{\link{logic.bagging}},
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
    \code{names(set)} are used.
}
  \item{n.perm}{
    number of permutations used in the computation of the importances. By default (i.e.\ if 
    \code{n.perm = NULL}), 100 permutations are used if \code{rebuild = TRUE} and the regression
    approach of logic regression has been used in \code{\link{logic.bagging}} (by setting
    \code{ntrees} to an integer larger than 1, or \code{glm.if.1tree = TRUE}). Otherwise,
    1000 permutation are employed. Note that actually much more permutations should be used.
}
  \item{standardize}{
    should the standardized importance measure be used?
}
  \item{rebuild}{
    logical indicating whether the logic regression models should be rebuild (i.e.\ the parameters
    \eqn{\beta}{beta} of the generalized linear models should be recomputed) after removing a
    variable or a set of variables from the logic trees and for each permutation of the response.
    Note that setting \code{rebuild = TRUE} increases the computation time substantially.
}
  \item{prob.case}{a numeric value between 0 and 1. If the logistic regression approach of logic
    regression has been used in \code{\link{logic.bagging}}, then an observation will be classified as a case (or
    more exactly, as 1), if the class probability of this observation is larger than \code{prob.case}.
    Otherwise, \code{prob.case} is ignored.
}
  \item{useAll}{
    logical indicating whether all \eqn{m *}{m *} \code{n.perm} permuted values should be used
    in the computation of the permutation based p-values, where \eqn{m} is the number of
    variables or sets of variables, respectively. If \code{FALSE}, the \code{n.perm} permuted
    values corresponding to the respective variable (or set of variables) are employed in
    the determination of the p-value of this variable (or set of variables).
}
  \item{adjust}{character vector naming the method with which the raw permutation based
    p-values are adjusted for multiplicity. If \code{"qvalue"}, the function \code{qvalue.cal}
    from the package \code{siggenes} is used to compute q-values. Otherwise,
    \code{p.adjust} is used to adjust for multiple comparisons. See \code{p.adjust} for all
    other possible specifications of \code{adjust}. If \code{"none"}, the raw p-values will
    be used.
}
  \item{addMatPerm}{
    should the (\code{n.perm} + 1) x \eqn{m} matrix containing the original values (first column)
    and the permuted values (the remaining columns) of the importance measure for the \eqn{m}
    variables or \eqn{m} sets of variables be added to the output? 
}
  \item{rand}{
    an integer for setting the random number generator in a reproducible state.
}
}

\value{
An object of class \code{logicFS} containing
  \item{vim}{the values of the importance measure for the input variables, the SNPs, or the sets of SNPs, respectively,}
  \item{prop}{\code{NULL},}
  \item{primes}{the names of the inputs, SNPs, or sets of variables, respectively,}
  \item{type}{the type of model (1: classification, 3: logistic regression),}
  \item{param}{\code{NULL},}
  \item{mat.imp}{\code{NULL},}
  \item{measure}{the name of the used importance measure,}
  \item{threshold}{0.95, i.e.\ the suggested threshold for calling an input, SNP or set of SNPs, respectively, important
     (this is just used as default value when plotting the importances, see argument \code{thres} of \code{\link{plot.logicFS}}),}
  \item{mu}{\code{NULL},}
  \item{useN}{\code{TRUE},}
  \item{name}{either \code{"Variable"}, \code{"SNP"}, or \code{"Set"},}
  \item{mat.perm}{if \code{addMatPerm = FALSE}, \code{NULL}; otherwise, a matrix containing the original and the permuted
     values of the respective importance measure.} 
}
\references{
   Schwender, H. and Ickstadt, K. (2009). Testing SNPs and Sets of SNPs for Importance on the Prediction in Association Studies.
   To appear.
}
\author{
   Holger Schwender, \email{holger.schwender@udo.edu}
}

\seealso{
  \code{\link{logic.bagging}}, \code{\link{vim.input}}, \code{\link{vim.set}}, \code{\link{vim.signperm}}
}

\keyword{logic}
\keyword{htest}

