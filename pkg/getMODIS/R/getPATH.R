# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Version 0.1
# Licence GPL v3


getPATH <- function(deep=FALSE,quiet=FALSE){

if (Sys.getenv("MRT_HOME")!=""){
	MRTpath <- Sys.getenv("MRT_HOME")
	MRTpath <- paste(MRTpath,"/bin/",sep="")
	if (!quiet) cat("If this path doesn't work check you path for 'MRT_HOME'\n")
}else{  
  if (!quiet) {
      if (deep) {
          cat("No path variable found for MRT, trying to look for MRT/bin in deep modus (this can take a while, consider aborting it!)\n")
          }else{
          cat("No path variable found for MRT, trying to look for MRT/bin (this can take a while)\n")
          }   
      }
      
MRTpath <- list.files(path = if(deep){"/"}else{"."}, pattern = "mrtmosaic",full.names = TRUE, recursive = TRUE,ignore.case = FALSE)

if (length(MRTpath == 1)){
MRTpath
} else if (length(MRTpath > 1)) {
	isit    <- strsplit(MRTpath,"/")
	getBIN  <- sapply(isit,function(x){x[(length(x)-1)]=="bin"})

		if(sum(as.numeric(getBIN))==1){
		getBIN  <- which(getBIN==TRUE)
		isit <- isit[[getBIN]]
		MRTpath <- paste(isit[-length(isit)],collapse="/")
MRTpath <- paste(MRTpath,"/",sep="")
		} else if (sum(as.numeric(getBIN)) > 1 & !quiet) {
		cat("I'm not sure, is it one of those?\n")
		}
} else {
	if (!quiet){
	cat("MRT not found, sorry but you have to solve this problem first\n")
	}
MRTpath <- 0
	}
}
return(MRTpath)
}

