getSPIAMatrices<-function(organism="mmu"){
require(RCurl)
urlf<-paste("http://bioinformaticsprb.med.wayne.edu/SPIA/build012309/",paste(organism,"SPIA.RData",sep=""),sep="")
download.file(urlf, destfile=paste(system.file("data",package="SPIA"),paste(organism,"SPIA.RData",sep=""),sep="/"), quiet = FALSE, mode = "wb", cacheOK = TRUE)
}









