\name{makeSPIAdata}
\alias{makeSPIAdata}
\title{Process KGML files for spia analysis}
\description{
This function processes KEGG xml files into a xxxSPIA.RData file needed for spia function. 
}
\usage{
makeSPIAdata(kgml.path="./hsa",organism="hsa",out.path=".")
}
\arguments{
  \item{kgml.path}{Character vector giving the location of the folder containing two or more KEGG xml files. See for e.g.
  http://www.genome.jp/kegg/pathway/hsa/hsa04010.html and click the Download KGML to get such files.
  Users that have a license to the KEGG ftp directory can copy all the xml files corresponding to 
  a givne organism.}
 \item{organism}{A three letter character designating the organism. See a full list at ftp://ftp.genome.jp/pub/kegg/xml/organisms.}
 \item{out.path}{Directory where a "organism"SPIA.RData file will be saved. If left to null, it will 
 will try to save the file in th extdata folder of the SPIA library.}

  
}


\author{Adi Laurentiu Tarca <atarca@med.wayne.edu>}

\seealso{\code{\link{spia}}}

\examples{
library(SPIA)
data(colorectalcancer)
makeSPIAdata(kgml.path=system.file("extdata/keggxml/hsa",package="SPIA"),organism="hsa",out.path="./")
res<-spia(de=DE_Colorectal, all=ALL_Colorectal, organism="hsa",data.dir="./")
res[,-12]

}

\keyword{nonparametric}% at least one, from doc/KEYWORDS
\keyword{methods}% __ONLY ONE__ keyword per line
