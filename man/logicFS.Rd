\name{logicFS}
\alias{logicFS}
\alias{logicFS.default}
\alias{logicFS.formula}
\title{Feature Selection with Logic Regression}
\description{
  Identification of interesting interactions between binary variables
  using logic regression. Currently available for the classification, the linear
  regression and the logistic regression approach of \code{logreg} and for
  a multinomial logic regression as implemented in \code{mlogreg}. 
}
\usage{
\method{logicFS}{formula}(formula, data, recdom = TRUE, ...)

\method{logicFS}{default}(x, y, B = 100, useN = TRUE, ntrees = 1, nleaves = 8, 
  glm.if.1tree = FALSE, replace = TRUE, sub.frac = 0.632, 
  anneal.control = logreg.anneal.control(), onlyRemove = FALSE,
  prob.case = 0.5, addMatImp = TRUE, rand = NULL, ...)
}

\arguments{
  \item{formula}{an object of class \code{formula} describing the model that should be
     fitted.}
  \item{data}{a data frame containing the variables in the model. Each row of \code{data}
     must correspond to an observation, and each column to a binary variable (coded by 0 and 1) 
     or a factor (for details, see \code{recdom}) except for the column comprising
     the response. The response must be either binary (coded by
     0 and 1), categorical or continuous. If continuous, a linear model is fitted in each of the \code{B} iterations of
     \code{logicFS}. If categorical, the column of \code{data} specifying the response must
     be a factor. In this case, multinomial logic regressions are performed as implemented in \code{\link{mlogreg}}.
     Otherwise, depending on \code{ntrees} (and \code{glm.if.1tree})
     the classification or the logistic regression approach of logic regression is used.}
  \item{recdom}{a logical value or vector of length \code{ncol(data)} comprising whether a SNP should
     be transformed into two binary dummy variables coding for a recessive and a dominant effect.
     If \code{TRUE} (logical value), then all factors (variables) with three levels will be coded by two dummy
     variables as described in \code{\link{make.snp.dummy}}. Each level of each of the other factors 
     (also factors specifying a SNP that shows only two genotypes) is coded by one indicator variable. 
     If \code{FALSE} (logical value),
     each level of each factor is coded by an indicator variable. If \code{recdom} is a logical vector,
     all factors corresponding to an entry in \code{recdom} that is \code{TRUE} are assumed to be SNPs
     and transformed into the two binary variables described above. Each variable that corresponds
     to an entry of \code{recdom} that is \code{TRUE} (no matter whether \code{recdom} is a vector or a value)
     must be coded by the integers 1 (coding for the homozygous reference genotype), 2 (heterozygous), 
     and 3 (homozygous variant).}
  \item{x}{a matrix consisting of 0's and 1's. Each column must correspond
     to a binary variable and each row to an observation.}
  \item{y}{a numeric vector or a factor specifying the values of a response for all the observations 
     represented in \code{x}. If a numeric vector, then \code{y} either contains 
     the class labels (coded by 0 and 1) or the values of a continuous response depending
     on whether the classification or logistic regression approach of logic
     regression, or the linear regression approach, respectively, should be used. If the response
     is categorical, then \code{y} must be a factor naming the class labels of the observations.}
  \item{B}{an integer specifying the number of iterations.}
  \item{useN}{logical specifying if the number of correctly classified out-of-bag observations should
     be used in the computation of the importance measure. If \code{FALSE}, the proportion of
     correctly classified oob observations is used instead.}
  \item{ntrees}{an integer indicating how many trees should be used. 
  
     For a binary response: If \code{ntrees}
     is larger than 1, the logistic regression approach of logic regreesion
     will be used. If \code{ntrees} is 1, then by default the classification
     approach of logic regression will be used (see \code{glm.if.1tree}.)
     
     For a continuous response: A linear regression model with \code{ntrees} trees
     is fitted in each of the \code{B} iterations.
     
     For a categorical response: \eqn{n.lev-1} logic regression models with \code{ntrees} trees
     are fitted, where \eqn{n.lev} is the number of levels of the response (for details, see
     \code{\link{mlogreg}}).}
  \item{nleaves}{a numeric value specifying the maximum number of leaves used
     in all trees combined. For details, see the help page of the function \code{logreg} of
     the package \code{LogicReg}.}
  \item{glm.if.1tree}{if \code{ntrees} is 1 and \code{glm.if.1tree} is \code{TRUE}
     the logistic regression approach of logic regression is used instead of
     the classification approach. Ignored if \code{ntrees} is not 1, or the response is not binary.}
  \item{replace}{should sampling of the cases be done with replacement? If 
     \code{TRUE}, a Bootstrap sample of size \code{length(cl)} is drawn
     from the \code{length(cl)} observations in each of the \code{B} iterations. If
     \code{FALSE}, \code{ceiling(sub.frac * length(cl))} of the observations
     are drawn without replacement in each iteration.}
  \item{sub.frac}{a proportion specifying the fraction of the observations that
     are used in each iteration to build a classification rule if \code{replace = FALSE}.
     Ignored if \code{replace = TRUE}.}
  \item{anneal.control}{a list containing the parameters for simulated annealing.
     See the help of the function \code{logreg.anneal.control} in the \code{LogicReg} package.}
  \item{onlyRemove}{should in the single tree case the multiple tree measure be used? If \code{TRUE},
     the prime implicants are only removed from the trees when determining the importance in the
     single tree case. If \code{FALSE}, the original single tree measure is computed for each prime
     implicant, i.e.\ a prime implicant is not only removed from the trees in which it is contained,
     but also added to the trees that do not contain this interaction. Ignored in all other than the
     classification case.}
  \item{prob.case}{a numeric value between 0 and 1. If the outcome of the
     logistic regression, i.e.\ the predicted probability, for an observation is
     larger than \code{prob.case} this observations will be classified as case 
     (or 1).}
  \item{addMatImp}{should the matrix containing the improvements due to the prime implicants
     in each of the iterations be added to the output? (For each of the prime implicants,
     the importance is computed by the average over the \code{B} improvements.) Must be
     set to \code{TRUE}, if standardized importances should be computed using 
     \code{\link{vim.norm}}, or if permutation based importances should be computed 
     using \code{\link{vim.perm}}.} 
  \item{rand}{numeric value. If specified, the random number generator will be
     set into a reproducible state.}
  \item{...}{for the \code{formula} method, optional parameters to be passed to the low level function
    \code{logicFS.default}. Otherwise, ignored.}
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants,}
  \item{vim}{the importance of the prime implicants,}
  \item{prop}{the proportion of logic regression models that contain the prime 
     implicants,}
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
   Ruczinski, I., Kooperberg, C., LeBlanc M.L. (2003). Logic Regression.
   \emph{Journal of Computational and Graphical Statistics}, 12, 475-511.
   
   Schwender, H., Ickstadt, K. (2007). Identification of SNP Interactions
   Using Logic Regression. \emph{Biostatistics}, 9(1), 187-198.
}
   
\author{Holger Schwender, \email{holger.schwender@udo.edu}}


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
   log.out<-logicFS(bin.snps,cl.logicfs,B=20,nleaves=10,
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
