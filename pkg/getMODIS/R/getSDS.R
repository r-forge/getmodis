getSDS <- function(HdfName,MRTpath="check") {
######
fsep <- .Platform$file.sep

if (MRTpath=="check") {
	MRTpath <- getPATH(quiet=TRUE)
	}

if (!file.exists(MRTpath)) {stop("'MRTpath' is wrong or MRT not installed? Provide a good path, leave empty or run 'getPATH()' first!")}

if (!file.exists(HdfName)) {
	cat("Hm, I have to search for the file!\n")
	HdfName <- list.files(pattern=paste(HdfName,"$",sep=""),recursive=TRUE)
	}
	
	HdfName <- HdfName[1]
	
if (.Platform$OS=="unix"){
	sdsRaw <- system(paste(file.path(MRTpath,"sdslist",fsep=fsep),HdfName,sep=" "),intern=TRUE)
	
}else if (.Platform$OS=="windows"){
	sdsRaw <- call(paste(file.path(MRTpath,"sdslist",fsep=fsep),HdfName,sep=" "),intern=TRUE)

} else {
	stop(cat("What OS have you? Please tell me so I can fix this.\n")) 
}

sds <- list()
for (i in 1:length(sdsRaw)){
sds[[i]] <- substr(sdsRaw[i],1,11) == "SDgetinfo: "
}
sds <- sdsRaw[unlist(sds)]
sds <- unlist(lapply(sds,function(x){strsplit(x,": ")[[1]][2]}))
sds <- unlist(lapply(sds,function(x){paste(strsplit(x,", ")[[1]][1:2],collapse=": ")}))

return(sds)
}


