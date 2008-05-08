\name{mlogreg}
\alias{mlogreg}
\alias{mlogreg.default}
\alias{mlogreg.formula}
\alias{print.mlogreg}

\title{Multinomial Logic Regression}
\description{
  Performs a multinomial logic regression for a nominal response by fitting
  a logic regression model (with logit as link function) for each of the
  levels of the response except for the level with the smallest value which
  is used as reference category.
}
\usage{
\method{mlogreg}{formula}(formula, data, recdom = TRUE, ...)

\method{mlogreg}{default}(x, y, ntrees = 1, nleaves = 8, anneal.control = logreg.anneal.control(), 
    rand = NA, ...)
}

\arguments{
  \item{formula}{an object of class \code{formula} describing the model that should be
     fitted.}
  \item{data}{a data frame containing the variables in the model. Each column of \code{data}
     must correspond to a binary variable (coded by 0 and 1) or a factor (for details on factors, see
     \code{recdom}) except for the column comprising
     the response, and each row to an observation. The response must be a categorical variable
     with less than 10 levels. This response can be either a factor or of type \code{numeric} or 
     \code{character}.}
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
  \item{y}{either a factor or a numeric or character vector specifying the values of the response.
     The length of \code{y} must be equal to the number of rows of \code{x}.}
  \item{ntrees}{an integer indicating how many trees should be used in the logic regression models.
     For details, see \code{logreg} in the \code{LogicReg package}.}
  \item{nleaves}{a numeric value specifying the maximum number of leaves used
     in all trees combined. See the help page of the function \code{logreg} in the \code{LogicReg}
     package for details.}
  \item{anneal.control}{a list containing the parameters for simulated annealing.
     For details, see the help page of \code{logreg.anneal.control} in the \code{LogicReg} package}
  \item{rand}{numeric value. If specified, the random number generator will be
     set into a reproducible state.}
  \item{...}{for the \code{formula} method, optional parameters to be passed to the low level function
    \code{mlogreg.default}. Otherwise, ignored.}
}

\value{
  An object of class \code{mlogreg} composed of
  \item{model}{a list containing the logic regression models,}
  \item{data}{a matrix containing the binary predictors,}
  \item{cl}{a vector comprising the class labels,}
  \item{ntrees}{a numeric value naming the maximum number of trees used in the logic regressions,}
  \item{nleaves}{a numeric value comprising the maximum number of leaves used in the logic regressions.} 
}

\references{
  Holger Schwender (2007).\ Measuring the Importances of Genotypes and Sets of Single Nucleotide
  Polymorphisms.\ Technical Report, SFB 475, Department of Statistics, University of Dortmund.\ Appears soon.
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}

\seealso{\code{\link{predict.mlogreg}}, \code{\link{logic.bagging}}, \code{\link{logicFS}}}

\keyword{tree}
\keyword{regression}
