\name{make.snp.dummy}
\alias{make.snp.dummy}
\title{SNPs to Dummy Variables}
\description{
  Transforms SNPs into binary dummy variables.
}
\usage{
  make.snp.dummy(data)
}
\arguments{
  \item{data}{a matrix in which each column corresponds to a SNP and each row to an observation.
     The genotypes of all SNPs must be either coded by 1 (for the homozygous reference genotype),
     2 (heterozygous), and 3 (homozygous variant) or by 0, 1, 2. It is not allowed that some SNPs following the 1, 2, 3
     coding scheme and some SNPs the 0, 1, 2 coding. Missing values are allowed, but please note that 
     neither \code{\link{logic.bagging}} nor \code{\link{logicFS}} can handle missing values so that
     the missing values need to be imputed (preferably before an application of \code{make.snp.dummy}.}
}

\value{
  A matrix with 2*\code{ncol(data)} columns containing 2 dummy variables
  for each SNP.
}

\details{
  \code{make.snp.dummy} assumes that the homozygous dominant genotype 
  is coded by 1, the heterozygous genotype by 2, and the homozygous
  recessive genotype by 3. Alternatively, the three genotypes can be coded
  by the number of minor alleles, i.e. by 0, 1, and 2. 
  For each SNP, two dummy variables are generated:
  \describe{
     \item{SNP.1}{At least one of the bases explaining the SNP are of
        the recessive type.}
     \item{SNP.2}{Both bases are of the recessive type.}
  }  
}

\note{
  See the \code{R} package \code{scrime} for more general functions for recoding SNPs.
}

\author{Holger Schwender, \email{holger.schwender@hhu.de}}


\keyword{manip}
