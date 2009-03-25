getSPIAMatrices<-function(organism="mmu"){
require(RCurl)
urlf<-paste("http://bioinformaticsprb.med.wayne.edu/SPIA/build032409/",paste(organism,"SPIA.RData",sep=""),sep="")
download.file(urlf, destfile=paste(system.file("extdata",package="SPIA"),paste(organism,"SPIA.RData",sep=""),sep="/"), quiet = FALSE, mode = "wb", cacheOK = TRUE)
}









