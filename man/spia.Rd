\name{spia}
\alias{spia}
\title{Signaling Pathway Impact Analysis (SPIA) based on over-representation and signaling perturbations accumulation}
\description{
This function implements the SPIA algorithm to analyze KEGG signaling pathways. 
}
\usage{
spia(de=NULL,all=NULL,organism="hsa",data.dir=NULL,pathids=NULL,nB=2000,plots=FALSE,verbose=TRUE,beta=NULL,combine="fisher")
}
\arguments{
  \item{de}{A named vector containing log2 fold-changes of the differentially expressed genes. The names of this numeric vector
  are Entrez gene IDs.}
  \item{all}{A vector with the Entrez IDs in the reference set. If the data was obtained from a microarray experiment, 
  this set will contain all genes present on the specific array used for the experiment. This vector should
  contain all names of the \code{de} argument.}
  \item{organism}{A three letter character designating the organism. See a full list at ftp://ftp.genome.jp/pub/kegg/xml/organisms.}
  \item{data.dir}{Location of the "organism"SPIA.RData file containing the pathways data generated with makeSPIAdata.
  If set to NULL will look for this file in the extdata folder of the SPIA library.}
  \item{pathids}{A character vector with the names of the pathways to be analyzed. If left NULL all pathways available will be tested.}
  \item{nB}{Number of bootstrap iterations used to compute the P PERT value. Should be larger
  than 100. A recommended value is 2000.}
  \item{plots}{If set to TRUE, the function plots the gene perturbation accumulation vs log2 fold 
  change for every gene on each pathway. The null distribution of the total net accumulations from which PPERT is computed, is plotted as well.
  The figures are sent to the SPIAPerturbationPlots.pdf file in the current directory.}
   \item{verbose}{If set to TRUE, displays the number of pathways already analyzed.}
   \item{beta}{Weights to be assigned to each type of gene/protein relation type. It should be a named numeric vector of length 23, whose names must be:
\code{c("activation","compound","binding/association","expression","inhibition","activation_phosphorylation","phosphorylation",
"indirect","inhibition_phosphorylation","dephosphorylation_inhibition","dissociation","dephosphorylation","activation_dephosphorylation",
"state","activation_indirect","inhibition_ubiquination","ubiquination","expression_indirect","indirect_inhibition","repression",
"binding/association_phosphorylation","dissociation_phosphorylation","indirect_phosphorylation")}

If set to null, beta will be by default chosen as: c(1,0,0,1,-1,1,0,0,-1,-1,0,0,1,0,1,-1,0,1,-1,-1,0,0,0). 
}
\item{combine}{Method used to combine the two types of p-values. If set to \code{"fisher"} it will use Fisher's method. If set to \code{"norminv"} it will use the normal inversion method.}

}

\details{
See cited documents for more details.
}
\value{
 A data frame containing the ranked pathways and various statistics: \code{pSize} is the number of genes on the pathway;
 \code{NDE} is the number of DE genes per pathway; \code{tA} is the observed total preturbation 
 accumulation in the pathway; \code{pNDE} is the probability to observe at least \code{NDE} genes on
  the pathway using a hypergeometric model;
 \code{pPERT} is the probability to observe a total accumulation more extreme than \code{tA} only by 
 chance;
 \code{pG} is the p-value obtained by combining \code{pNDE} and \code{pPERT};
  \code{pGFdr} and \code{pGFWER} are the
 False Discovery Rate and respectively Bonferroni adjusted global p-values; and the \code{Status} gives the direction 
 in which the pathway is perturbed (activated or inhibited). 
 \code{KEGGLINK} gives a web link to the KEGG website that displays the pathway image with the differentially expressed genes 
 highlighted in red.
}

\references{
Adi L. Tarca, Sorin Draghici, Purvesh Khatri, et. al, A Signaling Pathway Impact Analysis for 
Microarray Experiments, 2008, Bioinformatics, 2009, 25(1):75-82. \cr

Purvesh Khatri, Sorin Draghici, Adi L. Tarca, Sonia S. Hassan, Roberto Romero. A system biology 
approach for the steady-state analysis of gene signaling networks. Progress in Pattern Recognition,
 Image Analysis and Applications, Lecture Notes in Computer Science. 4756:32-41, November 2007. \cr

Draghici, S., Khatri, P., Tarca, A.L., Amin, K., Done, A., Voichita, C., Georgescu, C., Romero, R.: 
A systems biology approach for pathway level analysis. Genome Research, 17, 2007. \cr

}

\author{Adi Laurentiu Tarca <atarca@med.wayne.edu>, Purvesh Khatri, Sorin Draghici}

\seealso{\code{\link{plotP}}}

\examples{
# Example using a colorectal cancer dataset obtained using Affymetrix geneChip technology (GEE GSE4107).
# Suppose that proper preprocessing was performed and a two group moderated t-test was applied. The topTable 
# result from limma package for this data set is called "top".
#The following lines will annotate each probeset to an entrez ID identifier, will keep the most significant probeset for each 
#gene ID and retain those with FDR<0.05 as differentially expressed.
#You can run these lines if hgu133plus2.db package is available

#data(colorectalcancer)
#x <- hgu133plus2ENTREZID 
#top$ENTREZ<-unlist(as.list(x[top$ID]))
#top<-top[!is.na(top$ENTREZ),]
#top<-top[!duplicated(top$ENTREZ),]
#tg1<-top[top$adj.P.Val<0.1,]
#DE_Colorectal=tg1$logFC
#names(DE_Colorectal)<-as.vector(tg1$ENTREZ)
#ALL_Colorectal=top$ENTREZ

data(colorectalcancer)

# pathway analysis using SPIA; # use nB=2000 or higher for more accurate results
#uses older version of KEGG signalimng pathways graphs
res<-spia(de=DE_Colorectal, all=ALL_Colorectal, organism="hsa",beta=NULL,nB=2000,plots=FALSE, verbose=TRUE,combine="fisher")
res
# Create the evidence plot
plotP(res)

#now combine pNDE and pPERT using the normal inversion method without running spia function again
res$pG=combfunc(res$pNDE,res$pPERT,combine="norminv")
res$pGFdr=p.adjust(res$pG,"fdr")
res$pGFWER=p.adjust(res$pG,"bonferroni")
plotP(res,threshold=0.05)
#highlight the colorectal cancer pathway in green
points(I(-log(pPERT))~I(-log(pNDE)),data=res[res$ID=="05210",],col="green",pch=19,cex=1.5)

#run SPIA using pathways data generated from (up-to-date) xml files that you can obtain from 
#KEGG ftp or by downloading  them from each pathway's web page:
# e.g. go to http://www.genome.jp/kegg/pathway/hsa/hsa04010.html  and click on DOwnload KGML
#to get the xml file for pathway 4010

makeSPIAdata(kgml.path=system.file("extdata/keggxml/hsa",package="SPIA"),organism="hsa",out.path="./")

res<-spia(de=DE_Colorectal, all=ALL_Colorectal, organism="hsa",data.dir="./")
res


}

\keyword{nonparametric}% at least one, from doc/KEYWORDS
\keyword{methods}% __ONLY ONE__ keyword per line
