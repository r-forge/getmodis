# Author: Matteo Mattiuzzi, Anja Klisch, matteo.mattiuzzi@boku.ac.at
# Date : July 2011
# Licence GPL v3
  

getHDF <- function(LocalArcPath,HdfName,product,startdate,enddate,tileH,tileV,extent,collection,quiet=FALSE,wait=1,checkXML=FALSE) {

if (wait > 0){require(audio)} # waiting seams to decrease the chance of ftp rejection!

fsep <- .Platform$file.sep

if (missing(LocalArcPath)) {
	LocalArcPath <- "~/"
	LocalArcPath <- normalizePath(path.expand(LocalArcPath), winslash = fsep)
	LocalArcPath <- paste(strsplit(LocalArcPath,fsep)[[1]],collapse=fsep)# removes "/" or "\" on last position (if present)
	LocalArcPath <- file.path(LocalArcPath,"MODIS_ARC",fsep=fsep)
	cat(paste("No archive path set, using/creating standard archive in: ",LocalArcPath,"\n",sep=""))
	flush.console()
}

dir.create(LocalArcPath,showWarnings=FALSE)
# test local LocalArcPath
try(testDir <- list.dirs(LocalArcPath),silent=TRUE)
if(!exists("testDir")) {stop("'LocalArcPath' not set properly!")} 
#################

# TODO HdfName as regex

if (!missing(HdfName)){ 

	HdfName <- unlist(HdfName)
	for (i in seq(along=HdfName)){
	
	secName <- strsplit(HdfName[i],"\\.")[[1]]
	
		if (secName[length(secName)]!= "hdf"){stop(secName,"is not a good hdf HdfName")}
					
	PF <- substr(secName[1],1,3)
		
		if (!PF %in% c("MOD","MYD")) {stop(PF," not from TERRA or AQUA")}

	if(PF == "MOD"){PF1 <- "MOLT"} else {PF1 <- "MOLA"}

	fdate <- substr(secName[2],2,8)
	fdate <- format(as.Date(as.numeric(substr(fdate,5,7))-1,origin=paste(substr(fdate,1,4),"-01-01",sep="")),"%Y.%m.%d")
	collection <- secName[4]

	arcPath <- paste(secName[1],".",collection,fsep,fdate,fsep,sep="")
	dir.create(paste(LocalArcPath,fsep,arcPath,sep=""),recursive=TRUE,showWarnings=FALSE) # this always generates the same structure as the original ftp (this makes sense if the local LocalArcPath becomes big!)
	
		if (!file.exists(paste(LocalArcPath,fsep,arcPath,HdfName[i],sep=""))) {
		    require(RCurl)
				ftpPath <- paste("ftp://e4ftl01u.ecs.nasa.gov/",PF1,"/", secName[1],".",collection,"/",fdate,"/",HdfName[i],sep="")
	download.file(
				ftpPath,
				destfile=paste(LocalArcPath,fsep,arcPath,HdfName[i],sep=""),
				mode='wb', method='wget', quiet=quiet, cacheOK=FALSE)
			
			if (wait!=0) {wait(wait)}

		}
		if(checkXML){getXML(HdfName = HdfName[i])}
	}
cat("downloaded: ",HdfName[i],"\n")


} else { # if HdfName is'nt provided:

if (missing(startdate)) {stop("Please provide a 'startdate' (format: 'YYYY.MM.DD')")} 
if (missing(enddate))   {stop("Please provide a 'endate' (format: 'YYYY.MM.DD')")} 
if (missing(extent) & (missing(tileH) | missing(tileV))){stop("Please provide eighter a 'tileH(s)' plus tileV(s) or an extent")} 
if (missing(product))   {stop("Please provide the MODIS-'product'")}
#######
# check product

product <- getPRODUCT(product=product)

# check collection
if (missing(collection)) {
	collection <- getCOLLECTION(product=product)
	} else {
	collection <- sprintf("%03d",as.numeric(collection))
	if (!getCOLLECTION(product=product,collection=collection)) {stop(paste("The collection you have requested may doesn't exist run: 'getCOLLECTION(LocalArcPath='",LocalArcPath,"',product='",product$request ,"',forceCheck=TRUE,newest=FALSE)' to update internal list and see available once!",sep=""))}
	}

#### convert dates 
begin   <- as.Date(startdate,format="%Y.%m.%d")
if (is.na(begin)) {stop("\n'startdate=",startdate,"' is eighter wrong format (not:'YYYY.MM.DD') or a invalid date")}
end     <- as.Date(enddate,format="%Y.%m.%d") 
if (is.na(end)) {stop("\n'enddate=",enddate,"' is eighter wrong format (not:'YYYY.MM.DD') or a invalid date")}
####
# tileID
if (substr(product$PD,3,nchar(product$PD))=="CMG") {
	tileID="GLOBAL"
	ntiles=1 
	} else {
	if(!missing(extent)) {
  	tileID <- getTILE(extent=extent)$tile
 	 } else {
 	 tileID <- getTILE(tileH=tileH,tileV=tileV)$tile
 	 }
	ntiles <- length(tileID)
}

auxPATH <- file.path(LocalArcPath,".auxiliaries",fsep=fsep)

dates  <- list()

for(z in 1:length(product$PF1)){ # Platforms MOD/MYD

	productName <- product$productName[z]
	
	ftp <- paste("ftp://e4ftl01u.ecs.nasa.gov/", product$PF1[z],"/", productName,".",collection,"/",sep="")

	invisible(getSTRUC(LocalArcPath=LocalArcPath,product=productName,collection=collection,startdate=startdate,enddate=enddate,wait=0))
		if (wait > 0){wait(as.numeric(wait))}

	FtpDayDirs <- read.table(file.path(auxPATH, "ftpdir.txt", fsep = fsep), stringsAsFactors = FALSE)
	FtpDayDirs <- FtpDayDirs[,which(colnames(FtpDayDirs)==paste(productName,".",collection,sep=""))] 
	FtpDayDirs <- FtpDayDirs[!is.na(FtpDayDirs)]
	
	sel <- as.Date(FtpDayDirs,format="%Y.%m.%d") # convert to date
	us  <- sel >= begin & sel <= end
	if (sum(us,na.rm=TRUE)>0){ 
	dates[[z]] <- FtpDayDirs[us]

	dates[[z]] <- cbind(dates[[z]],matrix(rep(NA, length(dates[[z]])*ntiles),ncol=ntiles,nrow=length(dates[[z]])))
	colnames(dates[[z]]) <- c("date",tileID)


	for (i in 1:nrow(dates[[z]])){

		year <- format(as.Date(dates[[z]][i,1],format="%Y.%m.%d"), "%Y")
		doy  <- as.integer(format(as.Date(dates[[z]][i,1],format="%Y.%m.%d"), "%j"))
		doy  <- sprintf("%03d",doy)
		datu <- paste("A",year,doy,sep="")
		mtr  <- rep(1,ntiles) # for file situation flaging

# creates local directory (HDF file container)
arcPath <- paste(LocalArcPath,fsep,product$PF2[z],product$PD,".",collection,fsep,dates[[z]][i,1],fsep,sep="")
dir.create(arcPath,showWarnings=FALSE,recursive=TRUE)

for(j in 1:ntiles){

dates[[z]][i,j+1] <- paste(product$PF2[z],product$PD,".",datu,".",if (tileID[j]!="GLOBAL") {paste(tileID[j],".",sep="")},collection,".*.hdf$",sep="") # create pattern
	
	if (length(dir(arcPath,pattern=dates[[z]][i,j+1]))>0){ # if available locally
		
		HDF <- dir(arcPath,pattern=dates[[z]][i,j+1])  # extract HDF file
		
		if (length(HDF)>1) { # in very recent files sometimes there is more than 1 file/tile/date if so get the last
			select <- list()
			for (d in 1:length(HDF)){ 
			select[[d]]<- strsplit(HDF[d],"\\.")[[1]][5]
			}
			HDF <- HDF[which.max(unlist(select))]		
			}
	dates[[z]][i,j+1] <- HDF
	mtr[j] <- 0
	}
}


if (sum(mtr)!=0) { # if one or more of the tiles in date is missing, its necessary to go on ftp

	ftpfiles <- getURL(paste(ftp,dates[[z]][i,1],"/",sep=""))
	ftpfiles <- strsplit(ftpfiles, if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
		if (wait > 0){wait(as.numeric(wait))}

	if (ftpfiles[1] != "total 0") {
    
    ftpfiles <- unlist(lapply(strsplit(ftpfiles," "),function(x){x[length(x)]})) # found empty dir!
	
		for(j in 1:ntiles){
		
			if(mtr[j]==1){ # if tile is missing get it
			onFtp <- grep(ftpfiles,pattern=dates[[z]][i,j+1],value=TRUE)
			HDF   <- grep(onFtp,pattern=".hdf$",value=TRUE)
		
				if (length(HDF)>1) { # in very recent files sometimes there is more than 1 file/tile/date if so get the last
				select <- list()
				for (d in 1:length(HDF)){
				select[[d]]<- strsplit(HDF[d],"\\.")[[1]][5]
				}
				HDF <- HDF[which.max(unlist(select))]		
				}
			dates[[z]][i,j+1] <- HDF
			hdf <- download.file(paste(ftp, dates[[z]][i,1], "/", HDF,sep=""), destfile=paste(arcPath, HDF, sep=""), mode='wb', method='wget', quiet=quiet, cacheOK=FALSE)
			mtr[j] <- hdf
				if (wait > 0){wait(as.numeric(wait))}
			}
		}
	} else {
	dates[[z]][i,(j+1):ncol(dates[[z]])] <- "No files for that date on FTP"} # on ftp is possible to find empty folders!
}

dir.create(paste(LocalArcPath,fsep,"LOGS",fsep,sep=""),showWarnings=FALSE)	
write.csv(dates[[z]],file=paste(LocalArcPath,fsep,"LOGS",fsep,product$PF2[z],product$PD,"_",collection,"_CHECK.csv",sep=""))

if(checkXML){xml <-  getXML(HdfName = list(paste(arcPath,dates[[z]][i,-1],sep="")),wait=wait)} # list() should not be needed

} # end dates i 
}else{ cat(paste("No files on ftp in date range for: ",product$PF2[z],product$PD,".",collection,"\n\n",sep=""))  }
} # if no files are avalable for product in date AND end platform z
} # end if not HdfName 
} ## END: FTP vs ARC check and download 



