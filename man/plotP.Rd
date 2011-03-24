\name{plotP}
\alias{plotP}
\title{SPIA two-way evidence plot}
\description{
Plots each pathway as a point, using the over-representation p-value, pNDE, and perturbations accumulation p-value,
pPERT, as coordinates.  In addition the regions where FDR and FWER adjusted pG values are less than the specified threshold are plotted.
The function determines automatically which method (fisher or norminv) was used to combine the two p-values into pG, and plots the regions described above accordingly. 
}
\usage{
plotP(x,threshold=0.05)
}
\arguments{
  \item{x}{A data frame produced by \code{\link{spia}} function.}
  \item{threshold}{A numerical value between 0 and 1 to be used as significance threshold in inferring pathway significance.}
}
  \details{In this plot each pathway is a point and the coordinates are the log of pNDE (using a hypergeometric model)
  and the p-value from perturbations, pPERT. The oblique lines in the plot show the significance 
  regions based on the combined evidence.}
\value{
 This function does not return any value. It only generates a plot.
}


\references{
Adi L. Tarca, Sorin Draghici, Purvesh Khatri, et. al, A Signaling Pathway Impact Analysis for 
Microarray Experiments, 2008, Bioinformatics, 2009, 25(1):75-82. \cr

}
\author{Adi Laurentiu Tarca <atarca@med.wayne.edu>, Purvesh Khatri, Sorin Draghici}

\seealso{\code{\link{spia}}}

\examples{
# Examples use colorectal cancer dataset
data(colorectalcancer)

# pathway analysis based on combined evidence of ORA and perturbations
# use nB=2000 or larger for more accurate results
res<-spia(de=DE_Colorectal, all=ALL_Colorectal, organism="hsa",nB=200,plots=FALSE,verbose=TRUE,beta=NULL,combine="fisher")

#Generate the evidence plot
plotP(res,threshold=0.1)

res<-spia(de=DE_Colorectal, all=ALL_Colorectal, organism="hsa",nB=200,plots=FALSE,verbose=TRUE,beta=NULL,combine="norminv")

#Generate the evidence plot
plotP(res,threshold=0.1)

}

\keyword{nonparametric}% at least one, from doc/KEYWORDS
\keyword{methods}% __ONLY ONE__ keyword per line
