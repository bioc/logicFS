\name{survivalFS}
\alias{survivalFS}
\alias{survivalFS.default}
\alias{survivalFS.formula}
\alias{survivalFS.logicBagg}
\title{Logic Feature Selection for Survival Data}
\description{
  Identification of interactions of binary variables associated with survival time
  using logic regression.
}
\usage{
\method{survivalFS}{default}(x, y, B = 20, replace = FALSE, 
  sub.frac = 0.632, score = c("DPO", "Conc", "Brier", "PL"), 
	addMatImp = TRUE, adjusted = FALSE, neighbor = NULL, 
	ensemble = FALSE, rand = NULL, ...)
  
\method{survivalFS}{formula}(formula, data, recdom = TRUE, ...)

\method{survivalFS}{logicBagg}(x, score = c("DPO", "Conc", "Brier", "PL"),
  adjusted = FALSE, neighbor = NULL, ensemble = FALSE,
	addMatImp = TRUE, rand = NULL, ...)
}

\arguments{
  \item{x}{a matrix consisting of 0's and 1's. Alternatively, \code{x} can also be an object of class \code{logicBagg}, i.e. the output of \code{\link{logic.bagging}}. If a matrix, each column must correspond
     to a binary variable and each row to an observation. Missing values are not allowed.}
  \item{y}{a vector of class \code{Surv} specifying the right-censored survival time for all observations represented in \code{x}, where no missing values are allowed in \code{y}. This vector can, e.g., be generated using the function \code{Surv} from the \code{R} package \code{survival}.}
  \item{B}{an integer specifying the number of iterations.}
   \item{replace}{should sampling of the cases be done with replacement? If 
     \code{TRUE}, a Bootstrap sample of size \code{length(y)} is drawn
     from the \code{length(y)} observations in each of the \code{B} iterations. If
     \code{FALSE}, \code{ceiling(sub.frac * length(y))} of the observations
     are drawn without replacement in each iteration.}
  \item{sub.frac}{a proportion specifying the fraction of the observations that
     are used in each iteration to build a classification rule if \code{replace = FALSE}.
     Ignored if \code{replace = TRUE}.}
	\item{score}{a character string naming the score that should be used in the computation of the importance measure for a survival time analysis. By default, the distance between predicted outcomes (\code{survival = "DPO"}) proposed by Tietz et al.\ (2018) is used in the determination of the importance of the variables. Alternatively, Harrell's C-Index (\code{"Conc"}), the Brier score (\code{"Brier"}), or the predictive partial log-likelihood (\code{"PL"}) can be used.}
	
  \item{addMatImp}{should the matrix containing the improvements due to the prime implicants
     in each of the iterations be added to the output? (For each of the prime implicants,
     the importance is computed by the average over the \code{B} improvements.) Must be
     set to \code{TRUE}, if standardized importances should be computed using 
     \code{\link{vim.norm}}, or if permutation based importances should be computed 
     using \code{\link{vim.signperm}}.} 
	\item{adjusted}{logical specifying whether the measures should be adjusted for noise. Often, the interaction actually associated with the response is not exactly found in some iterations of logic bagging, but an interaction is identified that either additionally contains one (or seldomly more) noise SNPs or does not contain one of SNPs forming the actually associated interaction. If \code{adjusted} is set to \code{TRUE}, the values of the importance measure is corrected for this behaviour.}
	\item{neighbor}{a list consisting of character vectors specifying SNPs that are in LD. If specified, all SNPs need to occur exactly one time in this list. If specified, the importance measures are adjusted for LD by considering the SNPs within a LD block as exchangable.}

	\item{ensemble}{in the case of a survival outcome, should \code{ensemble} importance measures (as, e.g., in \code{randomSurvivalSRC} be used? If \code{FALSE}, importance measures analogous to the ones in the logicFS analysis of other outcomes are used (see Tietz et al., 2018).}
  \item{rand}{numeric value. If specified, the random number generator will be
     set into a reproducible state.}
  \item{formula}{an object of class \code{formula} describing the model that should be
     fitted.}
  \item{data}{a data frame containing the variables in the model. Each row of \code{data}
     must correspond to an observation, and each column to a binary variable (coded by 0 and 1) 
     or a factor (for details, see \code{recdom}) except for the column comprising
     the response, where no missing values are allowed in \code{data}. The response must be an object of class \code{Surv}. }
  \item{recdom}{a logical value or vector of length \code{ncol(data)} comprising whether a SNP should
     be transformed into two binary dummy variables coding for a recessive and a dominant effect.
     If \code{recdom} is \code{TRUE} (and a logical value), then all factors/variables with three levels will be coded by two dummy
     variables as described in \code{\link{make.snp.dummy}}. Each level of each of the other factors 
     (also factors specifying a SNP that shows only two genotypes) is coded by one indicator variable. 
     If \code{recdom} is\code{FALSE} (and a logical value),
     each level of each factor is coded by an indicator variable. If \code{recdom} is a logical vector,
     all factors corresponding to an entry in \code{recdom} that is \code{TRUE} are assumed to be SNPs
     and transformed into two binary variables as described above. All variables corresponding
     to entries of \code{recdom} that are \code{TRUE} (no matter whether \code{recdom} is a vector or a value)
     must be coded either by the integers 1 (coding for the homozygous reference genotype), 2 (heterozygous), 
     and 3 (homozygous variant), or alternatively by the number of minor alleles, i.e. 0, 1, and 2, where
     no mixing of the two coding schemes is allowed. Thus, it is not allowed that some SNPs are coded by
     1, 2, and 3, and others are coded by 0, 1, and 2.}
  \item{...}{further arguments of \code{\link{logicFS}}. Ignored, if \code{x} is an object of class \code{logicBagg}.}
}

\value{
An object of class \code{logicFS} containing
  \item{primes}{the prime implicants,}
  \item{vim}{the importance of the prime implicants,}
  \item{prop}{the proportion of logic regression models that contain the prime 
     implicants,}
  \item{type}{the type of model (1: classification, 2: linear regression, 3: logistic regression, 4: Cox regression),}
  \item{param}{further parameters (if \code{addInfo = TRUE}),}
  \item{mat.imp}{the matrix containing the improvements if \code{addMatImp = TRUE},
    otherwise, \code{NULL},}
  \item{measure}{the name of the used importance measure,}
  \item{useN}{the value of \code{useN},}
  \item{threshold}{NULL,}
  \item{mu}{NULL.}
}

\references{
	 Tietz, T., Selinski, S., Golka, K., Hengstler, J.G., Gripp, S., Ickstadt, K.,
	 Ruczinski, I., Schwender, H. (2018). Identification of Interactions of
	 Binary Variables Associated with Survival Time Using survivalFS. Submitted.
}
   
\author{Tobias Tietz, \email{tobias.tietz@hhu.de}} 


\seealso{
   \code{\link{logicFS}}, \code{\link{logic.bagging}}
}


\keyword{tree}
\keyword{multivariate}
\keyword{regression}
