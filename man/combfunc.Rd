\name{combfunc}
\alias{combfunc}
\title{Combining two p-values using Fisher's product or normal inversion method}
\description{
Combining two p-values using Fisher's product or normal inversion methods. 
}
\usage{
combfunc(p1=NULL,p2=NULL,combine="fisher")
}
\arguments{
  \item{p1}{A vector of probabilities.}
  \item{p2}{A vector of probabilities.}
  
  \item{combine}{A string with the name of the method to be used. Options include "fisher","norminv"}
}
  \details{Two vectors of p-values are combined into a vector of global p-values.}
\value{
 A vector of p-values.
}


\references{
Adi L. Tarca, Sorin Draghici, Purvesh Khatri, et. al, A Signaling Pathway Impact Analysis for 
Microarray Experiments, 2008, Bioinformatics, 2009, 25(1):75-82. \cr

}
\author{Adi Laurentiu Tarca <atarca@med.wayne.edu>, Purvesh Khatri, Sorin Draghici}

\seealso{\code{\link{spia}}}

\examples{
# Examples use colorectal cancer dataset
p1=c(0.2,0.4,0.1)
p2=c(0.01,0.7,0.01)
pG=combfunc(p1,p2,combine="fisher")
pG=combfunc(p1,p2,combine="norminv")


}

\keyword{nonparametric}% at least one, from doc/KEYWORDS
\keyword{methods}% __ONLY ONE__ keyword per line
