
getXML <- function(LocalPathToHdf="",HdfName="", wait=2, comments=FALSE){

###################
if (LocalPathToHdf!=""){
LocalPathToHdf <- path.expand(LocalPathToHdf)
} else {
LocalPathToHdf <- "."
}

if(HdfName!="") {
	
	HdfName <- unlist(HdfName)
	avFiles <- list()
	
	for (i in seq(length(HdfName))){
		if (file.exists(HdfName[i])) { # if exists than HdfName is a path+File+itexists
		avFiles[[i]] <- HdfName[i] 
		} else {
		avFiles[[i]] <- list.files(LocalPathToHdf,pattern=HdfName[i],recursive=TRUE,full.names=TRUE)
		avFiles[[i]] <- grep(avFiles[[i]], pattern=".hdf$",value=TRUE) # removes xml files from list 
		}
	}
	
avFiles <- unlist(avFiles)
} else {
avFiles <- list.files(LocalPathToHdf,pattern=".hdf$",recursive=TRUE,full.names=TRUE) # all hdf under the 'LocalPathToHdf'
}


if(length(avFiles)==0) {return(cat("No files to download\n"))} else { # handle situation where only Non supported Grid-HDFs are stored


success <- rep(NA,length(avFiles))
    for (u in seq(along=avFiles)){

	name <- strsplit(avFiles[u],"/")[[1]] # separate name from path
	name <- name[length(name)] # select filename
	secName  <- strsplit(name,"\\.")[[1]] # decompose filename
	PF <- substr(secName[1],1,3)

	# check if it is MODIS-grid File
	Acheck <- substr(secName[2],1,1)
	Tpat <- "h[0-3][0-9]v[0-1][0-9]" # to enhance

	if (sum((!grep(secName[3],pattern=Tpat)) +  (Acheck == "A") +  (PF %in% c("MOD","MYD")) + (length(secName)!=6) ) == 4) {
				

	if ( !file.exists(paste(avFiles[u],".xml",sep="")) || # if xml-file doesn't exists 

	 	if ( .Platform$OS.type == "unix") {as.numeric(system(paste("stat -c %s ",avFiles[u],".xml",sep=""), intern=TRUE)) < 2000}else{FALSE} # tested on Ubuntu 11.04
# 		if ( .Platform$OS.type == "windows") {as.numeric(system(paste("for %I in (",avFiles[u],") do @echo %~zI",sep=""),intern=TRUE)) < 2000} # sould work with win2000 and later...but not tested! (http://stackoverflow.com/questions/483864/windows-command-for-file-size-only)
# 		if (!.Platform$OS.type %in% c("unix","windows")) {FALSE} # if not unix or windows, skip this test...(for now)
	# if file exists but smaller than 2000 B...  so probably brocken download
	){

	if(PF=="MOD"){PF <- "MOLT"} else {PF <- "MOLA"}

	date <- substr(secName[2],2,8)
	date <- format(as.Date(as.numeric(substr(date,5,7))-1,origin=paste(substr(date,1,4),"-01-01",sep="")),"%Y.%m.%d")

	version <- secName[4]

	require(RCurl) # is it good here?

	success[u] <- print(
		download.file(
			paste("ftp://e4ftl01u.ecs.nasa.gov/", PF,"/",secName[1],".",version,"/",date,"/",name,".xml",sep=""),
			destfile=paste(avFiles[u],".xml",sep=""),
			mode='wb', method='wget', quiet=F, cacheOK=FALSE)
		) # print

		if (comments){
			cat(paste("downloaded file: ", name,".xml\n\n",sep=""))
			flush.console()
		}

		if (wait!=0){
			require(audio) # for wait() # is it good here?
			wait(wait) # waiting seams to decrease the chanse of ftp collapse
			}
	} else {
	success[u] <- 0}
	}
	} # avFiles[u] 
return(success)
} # if avFiles > 0
} # end getMODIS::.getXML
