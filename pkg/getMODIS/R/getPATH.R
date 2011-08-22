# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Version 0.1
# Licence GPL v3


getPATH <- function(){
if (Sys.getenv("MRT_HOME")!=""){
	MRTpath <- Sys.getenv("MRT_HOME")
	MRTpath <- paste(MRTpath,"/bin/",sep="")
	cat("If this path doesn't work check you path for 'MRT_HOME'\n")
}else { 
	cat("No path variable found for MRT, trying to look for MRT /bin (this can take a while)\n")
	MRTpath <- list.files(path = ".", pattern = "mrtmosaic",full.names = TRUE, recursive = TRUE,ignore.case = FALSE) # looks recursive in local dir!
	isit <- strsplit(MRTpath,"/")
	getBIN  <- sapply(isit,function(x){x[(length(x)-1)]=="bin"})

	if(sum(as.numeric(getBIN))==1){
		getBIN  <- which(getBIN==TRUE)
		isit <- isit[[getBIN]]
		MRTpath <- paste(isit[-length(isit)],collapse="/")
		MRTpath <-paste(MRTpath,"/",sep="")
	} else {
	cat("I'm not sure, is it one of those?\n")
	}
}
MRTpath
}

