\name{logic.fs}
\alias{logic.fs}
\title{Feature Selection with Logic Regression}
\description{
  Identification of interesting interactions between binary variables
  using logic regression. Currently only the classification and the 
  logistic regression approach of \code{logreg} are available. 
}
\usage{
logic.fs(data, cl, B = 100, ntrees = 1, nleaves = 8, glm.if.1tree = FALSE, 
  anneal.control = logreg.anneal.control(), prob.case = 0.5, rand = NULL)
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
  \item{prob.case}{a numeric value between 0 and 1. If the outcome of the
     logistic regression, i.e.\ the predicted probability, for an observation is
     larger than \code{prob.case} this observations will be classified as case 
     (or 1).}
  \item{rand}{numeric value. If specified, the random number generator will be
     set into a reproducible state.}
}

\value{
  \code{logic.fs} returns an object of class \code{logicFS} containing
  \item{primes}{the prime implicants}
  \item{vim}{the importance of the prime implicants}
  \item{prop}{the proportions of logic regression models that contain the prime 
     implicants}
  \item{type}{the type of model (1: classification, 3: logistic regression)}
  \item{param}{further parameters}  
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
   \code{\link{plot.logicFS}}, \code{\link{logic.bagging}}
}


\examples{\dontrun{
   # Load data.
   data(data.logicfs)
   
   # For logic regression and hence logic.fs, the variables must
   # be binary. data.logicfs, however, contains categorical data 
   # with realizations 1, 2 and 3. Such data can be transformed 
   # into binary data by
   bin.snps<-make.snp.dummy(data.logicfs)
   
   # To speed up the search for the best logic regression models
   # only a small number of iterations is used in simulated annealing.
   my.anneal<-logreg.anneal.control(start=2,end=-2,iter=10000)
   
   # Feature selection using logic regression is then done by
   log.out<-logic.fs(bin.snps,cl.logicfs,B=20,nleaves=10,
       rand=123,anneal.control=my.anneal)
   
   # The output of logic.fs can be printed
   log.out
   
   # One can specify another number of interactions that should be
   # printed, here, e.g., 15.
   print(log.out,topX=15)
   
   # The variable importance can also be plotted.
   plot(log.out)
   
   # And the original variable names are displayed in
   plot(log.out,coded=FALSE)
}}

\keyword{tree}
\keyword{multivariate}
\keyword{regression}
