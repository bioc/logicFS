\name{make.snp.dummy}
\alias{make.snp.dummy}
\title{SNPs to Dummy Variables}
\description{
  Transforms SNPs into binary dummy variables
}
\usage{
  make.snp.dummy(data)
}
\arguments{
  \item{data}{a matrix containing only 1's, 2's and 3's (see details). 
     Each column of \code{data} corresponds to a SNP and each row to 
     an observation.}
}

\value{
  A matrix with 2*\code{ncol(data)} columns containing 2 dummy variables
  for each SNP.
}

\details{
  \code{make.snp.dummy} assumes that the homozygous dominant genotype 
  is coded by 1, the heterozygous genotype by 2, and the homozygous
  recessive genotype by 3. For each SNP, 2 dummy variables are generated:
  \describe{
     \item{SNP.1}{At least one of the bases explaining the SNP are of
        the recessive type.}
     \item{SNP.2}{Both bases are of the recessive type.}
  }  
}

\author{Holger Schwender, \email{holger.schwender@udo.edu}}


\keyword{manip}
