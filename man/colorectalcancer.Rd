\name{colorectalcancer}
\alias{colorectalcancer}
\alias{DE_Colorectal}
\alias{ALL_Colorectal}
\alias{top}


\title{Results from a microarray experiment comparing colorectal cancer samples and normal tissue samples.}

\description{
  The \code{colorectal} dataset consists: i) an named vector \code{DE_Colorectal}
  , which represents the $log2$ fold changes of the genes chosen as differentially expressed between 
  colorectal cancer and normal samples based on data from Hong et al, 2007, using a $FDR=0.1$ and 
 the universe of all Entrez gene IDs available on the array, \code{ALL_Colorectal}. These two vectors were obtained starting from the
 \code{top} dataframe which is the output from the topTable function of the limma package using the RMA processed gene expression data downloaded from 
GEE (GSE4107).
 The microarray platform used was Affymetrix HGU-133PLUS2.0. 
 }

\usage{data(colorectalcancer)}

\source{ Yi Hong and  Kok Sun Ho and  Kong Weng Eu and  Peh Yean Cheah, A susceptibility gene set for early onset colorectal cancer that
      integrates diverse signaling pathways: implication for tumorigenesis, Clin Cancer Res, 2007, 13(4),1107-14.
}

\keyword{datasets}


