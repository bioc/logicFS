\name{logic.bagging}
\alias{logic.bagging}
\alias{logic.bagging.default}
\alias{logic.bagging.formula}
\alias{print.logicBagg}
\title{Bagged Logic Regression}
\description{
  A Bagging version of logic regression. Currently available for the 
  classification, the linear regression, and the logistic regression approach 
  of \code{logreg}. 
}
\usage{
\method{logic.bagging}{formula}(formula, data, ...)

\method{logic.bagging}{default}(x, y, B = 100, ntrees = 1, nleaves = 8, 
  glm.if.1tree = FALSE, replace = TRUE, sub.frac = 0.632,
  anneal.control = logreg.anneal.control(), oob = TRUE, 
  prob.case = 0.5, importance = TRUE, addMatImp = FALSE,
  rand = NULL, ...)
}

\arguments{
  \item{formula}{an object of class \code{formula} describing the model that should be
     fitted}
  \item{data}{a data frame containing the variables in the model. Each column of \code{data}
     must correspond to a binary variable (coded by 0 and 1) except for the column comprising
     the response, and each row to an observation. The response must be either binary (coded by
     0 and 1) or continuous. If continuous, a linear model is fitted in each of the \code{B} iterations of
     \code{logic.bagging}. Otherwise, depending on \code{ntrees} (and \code{glm.if.1tree})
     the classification or the logistic regression approach of logic regression is used}
  \item{x}{a matrix consisting of 0's and 1's. Each column must correspond
     to a binary variable and each row to an observation}
  \item{y}{a numeric vector that either contains the class labels (coded by 0 and 1)
     of the observations if the classification or logistic regression approach of logic
     regression should be used, or the values of a continuous response if the linear
     regression approach should be used}
  \item{B}{an integer specifying the number of iterations}
  \item{ntrees}{an integer indicating how many trees should be used. 
  
     For a binary response: If \code{ntrees}
     is larger than 1, the logistic regression approach of logic regreesion
     will be used. If \code{ntrees} is 1, then by default the classification
     approach of logic regression will be used (see \code{glm.if.1tree}.)
     
     For a continuous response: A linear regression model with \code{ntrees} trees
     is fitted in each of the \code{B} iterations}
  \item{nleaves}{a numeric value specifying the maximum number of leaves used
     in all trees combined. See the help page of the function \code{logreg} of
     the package \code{LogicReg} for details}
  \item{glm.if.1tree}{if \code{ntrees} is 1 and \code{glm.if.1tree} is \code{TRUE}
     the logistic regression approach of logic regression is used instead of
     the classification approach. Ignored if \code{ntrees} is not 1 or the response is not binary}
  \item{replace}{should sampling of the cases be done with replacement? If 
     \code{TRUE}, a Bootstrap sample of size \code{length(cl)} is drawn
     from the \code{length(cl)} observations in each of the \code{B} iterations. If
     \code{FALSE}, \code{ceiling(sub.frac * length(cl))} of the observations
     are drawn without replacement in each iteration}
  \item{sub.frac}{a proportion specifying the fraction of the observations that
     are used in each iteration to build a classification rule if \code{replace = FALSE}.
     Ignored if \code{replace = TRUE}}
  \item{anneal.control}{a list containing the parameters for simulated annealing.
     See the help page of \code{logreg.anneal.control} in the \code{LogicReg} package}
  \item{oob}{should the out-of-bag error rate (classification and logistic regression)
    or the out-of-bag root mean square prediction error (linear regression), respectively, be computed?}
  \item{prob.case}{a numeric value between 0 and 1. If the outcome of the
     logistic regression, i.e.\ the class probability, for an observation is
     larger than \code{prob.case}, this observations will be classified as case 
     (or 1)}
  \item{importance}{should the measure of importance be computed?}
  \item{addMatImp}{should the matrix containing the improvements due to the prime implicants
     in each of the iterations be added to the output? (For each of the prime implicants,
     the importance is computed by the average over the \code{B} improvements.) Must be
     set to \code{TRUE}, if standardized importances should be computed using 
     \code{\link{vim.norm}}, or if permutation based importances should be computed 
     using \code{\link{vim.perm}}} 
  \item{rand}{numeric value. If specified, the random number generator will be
     set into a reproducible state}
  \item{...}{for the \code{formula} method, optional parameters to be passed to the low level function
    \code{logic.bagging.default}. Otherwise, ignored}
}

\value{
  \code{logic.bagging} returns an object of class \code{logicBagg} containing
  \item{logreg.model}{a list containing the \code{B} logic regression models}
  \item{inbagg}{a list specifying the \code{B} Bootstrap samples}
  \item{vim}{an object of class \code{logicFS} (if \code{importance = TRUE})}
  \item{oob.error}{the out-of-bag error (if \code{oob = TRUE})}
  \item{...}{further parameters of the logic regression}
  
}

\references{
   Ruczinski, I., Kooperberg, C., LeBlanc M.L. (2003). Logic Regression.
   \emph{Journal of Computational and Graphical Statistics}, 12, 475-511.
   
   Schwender, H., Ickstadt, K. (2007). Identification of SNP Interactions
   Using Logic Regression. \emph{Biostatistics}, doi:10.1093/biostatistics/kxm024
}
   
\author{Holger Schwender, \email{holger.schwender@udo.edu}}



\seealso{
   \code{\link{predict.logicBagg}}, \code{\link{plot.logicBagg}},
   \code{\link{logicFS}}
}


\examples{\dontrun{
 # Load data.
   data(data.logicfs)
   
   # For logic regression and hence logic.bagging, the variables must
   # be binary. data.logicfs, however, contains categorical data 
   # with realizations 1, 2 and 3. Such data can be transformed 
   # into binary data by
   bin.snps<-make.snp.dummy(data.logicfs)
   
   # To speed up the search for the best logic regression models
   # only a small number of iterations is used in simulated annealing.
   my.anneal<-logreg.anneal.control(start=2,end=-2,iter=10000)
   
   # Bagged logic regression is then performed by
   bagg.out<-logic.bagging(bin.snps,cl.logicfs,B=20,nleaves=10,
       rand=123,anneal.control=my.anneal)
   
   # The output of logic.bagging can be printed
   bagg.out
   
   # By default, also the importances of the interactions are 
   # computed
   bagg.out$vim
   
   # and can be plotted.
   plot(bagg.out)
   
   # The original variable names are displayed in
   plot(bagg.out,coded=FALSE)
   
   # New observations (here we assume that these observations are
   # in data.logicfs) are assigned to one of the classes by
   predict(bagg.out,data.logicfs)
}}

\keyword{tree}
\keyword{regression}
