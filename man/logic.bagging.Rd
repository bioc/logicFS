\name{logic.bagging}
\alias{logic.bagging}
\alias{print.logicBagg}
\title{Bagged Logic Regression}
\description{
  A first basic Bagging version of logic regression. Currently only the 
  classification and the logistic regression approach of \code{logreg} are available. 
}
\usage{
logic.bagging(data, cl, B = 100, ntrees = 1, nleaves = 8, 
  glm.if.1tree = FALSE, anneal.control = logreg.anneal.control(),
  oob = TRUE, prob.case = 0.5, importance = TRUE, rand = NULL)
}

\arguments{
  \item{data}{a matrix consisting of 0's and 1's. Each column must correspond
     to a binary variable and each row to an observation.}
  \item{cl}{a vector of 0's and 1's containing the class labels of the
     observations.}
  \item{B}{an integer specifying the number of iterations.}
  \item{ntrees}{an integer indicating how many trees should be used. If \code{ntrees}
     is larger than 1, the logistic regression approach of logic regreesion
     will be used. If \code{ntrees} is 1, then by default the classification
     approach of logic regression will be used (see \code{glm.if.1tree}).}
  \item{nleaves}{a numeric value specifying the maximum number of leaves used
     in all trees combined. See the help page of the function \code{logreg} of
     the package \code{LogicReg} for details.}
  \item{glm.if.1tree}{if \code{ntrees} is 1 and \code{glm.if.1tree} is TRUE
     the logistic regression approach of logic regression is used instead of
     the classification approach. Ignored if \code{ntrees} is not 1.}
  \item{anneal.control}{a list containing the parameters for simulated annealing.
     See \code{?logreg.anneal.control} of the \code{LogicReg} package.}
  \item{oob}{should the out-of-bag error rate be computed?}
  \item{prob.case}{a numeric value between 0 and 1. If the outcome of the
     logistic regression, i.e.\ the predicted probability, for an observation is
     larger than \code{prob.case} this observations will be classified as case 
     (or 1).}
  \item{importance}{should the measure of importance be computed?}
  \item{rand}{numeric value. If specified, the random number generator will be
     set into a reproducible state.}
}

\value{
  \code{logic.bagging} returns an object of class \code{logicBagg} containing
  \item{logreg.model}{a list containing the \code{B} logic regression models}
  \item{inbagg}{a list specifying the \code{B} Bootstrap samples}
  \item{vim}{an object of class \code{logicFS} (if \code{importance=TRUE})}
  \item{oob.error}{the out-of-bag error (if \code{oob=TRUE})}
  \item{...}{further parameters of the logic regression}
  
}

\references{
   Ruczinski, I., Kooperberg, C., LeBlanc M.L. (2003). Logic Regression.
   \emph{Journal of Computational and Graphical Statistics}, 12, 475-511.
   }
   
\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\note{ 
  Tech. Report on the feature selection using logic regression will be
  available soon.
}

\seealso{
   \code{\link{predict.logicBagg}}, \code{\link{plot.logicBagg}},
   \code{\link{logic.fs}}
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
