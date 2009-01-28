\name{hsaSPIA}
\alias{hsaSPIA}
\alias{path.info}


\title{Gene/protein relations data for homo sapiens KEGG signaling pathways prepared for SPIA analysis}

\description{
  The \code{path.info} object is a list containing gene/protein relations data in the form of matrices for human KEGG 
  signaling pathways. The names of this list are the pathway IDs. Each element of this list is a  list itself containg several matrices indicating
the relations between the different genes/proteins on the patwhay. There is one matrix for each of the 23 types of gene-gene interactions that we considered.
In each such matrix, the possible values are eithe 0 or 1. A 1 value means that the gene/protein designated by the column name has a relation with the gene/protein 
designated by the row name (directed relation). 
}

\usage{data(hsaSPIA)}

\source{These data were derived from KEGG pathways database http://www.genome.jp/kegg/}

\keyword{datasets}


