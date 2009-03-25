\name{getSPIAMatrices}
\alias{getSPIAMatrices}
\title{Downloading gene/protein relation data for analysis of microarray data with the SPIA algorithm.}
\description{
This function downloads pathway data from http://bioinformaticsprb.med.wayne.edu/SPIA, to be used for analysis with SPIA algorithm 
in conjuction with microarray data, for several organisms. 
}
\usage{
getSPIAMatrices(organism="mmu")
}
\arguments{
  \item{organism}{A three letter character designating the organism. See a full list at ftp://ftp.genome.jp/pub/kegg/xml/organisms .}

}

\details{
 This function downloads gene/protein relation data to be used by the function \code{spia} from http://bioinformaticsprb.med.wayne.edu/SPIA.
}
\value{
 No object is returned. A list with as many sublists as there are signaling pathways for the selected organisms is created and saved as a 
RData file in the data folder of the SPIA package. Each sublist contains one square matrix for each type of gene/protein relation type. 
}

\references{
Adi L. Tarca, Sorin Draghici, Purvesh Khatri, et. al, A Signaling Pathway Impact Analysis for 
Microarray Experiments, 2008, Bioinformatics, 2009, 25(1):75-82. \cr

}

\author{Adi Laurentiu Tarca <atarca@med.wayne.edu>, Purvesh Khatri, Sorin Draghici}

\seealso{\code{\link{spia}}}

\examples{

getSPIAMatrices(organism="mmu")

}

\keyword{nonparametric}% at least one, from doc/KEYWORDS
\keyword{methods}% __ONLY ONE__ keyword per line
