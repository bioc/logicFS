\name{data.logicfs}
\docType{data}
\alias{data.logicfs}
\alias{cl.logicfs}
\title{Example Data of logicFS}


\description{
\code{data.logicfs} contains two objects: a simulated matrix \code{data.logicfs}
of 400 observations (rows) and 15 variables (columns) and a vector
\code{cl.logicfs} of length 400 containing the class labels of the 
observations. 

Each variable is categorical with realizations 1, 2 and 3. The first 200 observations
are cases, the remaining are controls. If one of the following expression is \code{TRUE},
then the corresponding observation is a case:

SNP1 == 3

SNP2 == 1  AND  SNP4 == 3

SNP3 == 3  AND  SNP5 == 3  AND  SNP6 == 1

where SNP1 is in the first column of \code{data.logicfs}, SNP2 in the second, and so on.
}

\seealso{
   \code{\link{logic.bagging}}, \code{\link{logic.fs}}
}

\keyword{datasets}
