# Author: Matteo Mattiuzzi, Anja Klisch, matteo.mattiuzzi@boku.ac.at
# Date : July 2011
# Version 0.2
# Licence GPL v3
  

getHDF <- function(LocalArcPath,HdfName,product,startdate,enddate,tileH,tileV,extent,collection,quiet=FALSE,wait=1,checkXML=FALSE) {

if (wait > 0){require(audio)} # waiting seams to decrease the chance of ftp rejection!

if (.Platform$OS.type == "unix") {
	slashes <- "/"
	ssplit <- slashes
}else{
	slashes <- "\\"
	ssplit <- "\\\\"
}

if (missing(LocalArcPath)) {
	LocalArcPath <- "~/"
	LocalArcPath <- normalizePath(path.expand(LocalArcPath), winslash = slashes)
	LocalArcPath <- paste(strsplit(LocalArcPath,ssplit)[[1]],collapse=slashes)# removes "/" or "//" on last position (if present)
	LocalArcPath <- paste(LocalArcPath,slashes,"MODIS_ARC",sep="")
	cat(paste("No archive path set, using/creating standard archive in: ",LocalArcPath,"\n",sep=""))
	flush.console()
}

LocalArcPath <- paste(strsplit(LocalArcPath,ssplit)[[1]],collapse=slashes)# removes "/" or "//" on last position (if present)

dir.create(LocalArcPath,showWarnings=FALSE)
# test local LocalArcPath
try(testDir <- list.dirs(LocalArcPath),silent=TRUE)
	if(!exists("testDir")) {stop("'LocalArcPath' not set properly!")} 
#################

# if filename is provided other args are ignored (filename is ok for not too many files (because of high ftp-request frequency)

if (!missing(HdfName)){ 

	HdfName <- unlist(HdfName)
	for (i in seq(along=HdfName)){
	
	secName <- strsplit(HdfName[i],"\\.")[[1]]
	
		if (secName[length(secName)]!= "hdf"){stop(secName,"is not a good hdf HdfName")}
					
	PF <- substr(secName[1],1,3)
		
		if (!PF %in% c("MOD","MYD")) {stop(PF," not from TERRA or AQUA")}

	if(PF == "MOD"){PF1 <- "MOLT"} else {PF1 <- "MOLA"}

	date <- substr(secName[2],2,8)
	date <- format(as.Date(as.numeric(substr(date,5,7))-1,origin=paste(substr(date,1,4),"-01-01",sep="")),"%Y.%m.%d")
	collection <- secName[4]

	arcPath <- paste(secName[1],".",collection,slashes,date,slashes,sep="")
	dir.create(paste(LocalArcPath,slashes,arcPath,sep=""),recursive=TRUE,showWarnings=FALSE) # this always generates the same structure as the original ftp (this makes sense if the local LocalArcPath becomes big!)
	
		if (!file.exists(paste(LocalArcPath,slashes,arcPath,HdfName[i],sep=""))) {
		    require(RCurl)
				ftpPath <- paste("ftp://e4ftl01u.ecs.nasa.gov/",PF1,"/", secName[1],".",collection,"/",date,"/",HdfName[i],sep="")
	download.file(
				ftpPath,
				destfile=paste(LocalArcPath,slashes,arcPath,HdfName[i],sep=""),
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
if (missing(collection)){stop("Please provide a product-'collection' (probably: '005')")} 


# following variables will be activated when the packge is ready for that
# interactiveExtent        <- FALSE # TODO
forceFtpCheck <-  TRUE # TODO

# Check Platform and product
PF <- substr(product,2,2)

#TODO if MCD, PF1 == "MOTA")
if 	  (PF %in% c("x","X")) { PF1  <- c("MOLT", "MOLA"); PF2  <- c("MOD", "MYD") 
} else if (PF %in% c("y","Y")) { PF1  <- "MOLA"; PF2 <- "MYD"
} else if (PF %in% c("o","O")) { PF1  <- "MOLT"; PF2 <- "MOD"
} else if (PF %in% c("c","C")) { PF1  <- "MOTA"; PF2 <- "MCD"
} else {stop("Check 'product', the Platform specific part seams wrong. Not one of 'MOD','MYD','MXD','MCD'.")
}


# Check product
PD <- substr(product,4,nchar(product)) #'09Q1',...
#####
# collection
collection <- sprintf("%03d",collection)

data("MODIS_Products")

# validy check and information
for (i in 1:length(PF2)){

	if (paste(PF2[i],PD,sep="") %in% MODIS_Products[,1]) {
	ind <- which(MODIS_Products[,1] == paste(PF2[i],PD,sep=""))

if(as.character(MODIS_Products[ind,4])=="Swath"){stop(paste("You are looking for a '",as.character(MODIS_Products[ind,4]),"' product, only 'tile' data is supported yet!",sep=""))
		} else { 
		if(i == 1){cat("\n")} else {cat("and\n")}
		cat(paste("You are looking for ", as.character(MODIS_Products[ind,1])," collection ",collection,", the ",as.character(MODIS_Products[ind,6])," ",as.character(MODIS_Products[ind,3])," product from ",as.character(MODIS_Products[ind,2])," with a ground resolution of ",as.character(MODIS_Products[ind,5]),"\n",sep=""))
		}
	} else {
	cat(paste("No product found with the name ",PF2[i],PD,sep=""))}
}
cat("\n")


#### convert dates 
begin   <- as.Date(startdate,format="%Y.%m.%d")
if (is.na(begin)) {stop("\n'startdate=",startdate,"' is eighter wrong format (not:'YYYY.MM.DD') or a invalid date")}
end     <- as.Date(enddate,format="%Y.%m.%d") 
if (is.na(end)) {stop("\n'enddate=",enddate,"' is eighter wrong format (not:'YYYY.MM.DD') or a invalid date")}
####
# tileID
if (substr(PD,3,nchar(PD))=="CMG") {
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

dirALL <- list()
dates  <- list()

for(z in 1:length(PF1)){ # Platforms MOD/MYD

	ftp <- paste("ftp://e4ftl01u.ecs.nasa.gov/", PF1[z],"/", PF2[z],PD,".",collection,"/",sep="")

	require(RCurl) # the function doesn't start if it isn't able to check the ftpserver on entering... TODO force FTPcheck=FALSE
	FtpDayDirs  <- getURL(ftp)
  FtpDayDirs  <- unlist(strsplit(FtpDayDirs[[1]], if(.Platform$OS.type=="unix"){"\n"}else{"\r\n"})) # its important to minimise getURL() queries, every check = risk of FTP break + much time!
		if (wait > 0){wait(as.numeric(wait))}

	FtpDayDirs  <- FtpDayDirs[substr(FtpDayDirs, 1, 1)=='d'] # removes not usable folders i.e the first: "total 34128"
	dirALL[[z]] <- unlist(lapply(strsplit(FtpDayDirs, " "), function(x){x[length(x)]})) # dir name below ftp

	sel <- as.Date(dirALL[[z]],format="%Y.%m.%d") # convert to date
	us  <- sel >= begin & sel <= end
	if (sum(us)>0){ 
	dates[[z]] <- dirALL[[z]][us]

	dates[[z]] <- cbind(dates[[z]],matrix(rep(NA, length(dates[[z]])*ntiles),ncol=ntiles,nrow=length(dates[[z]])))
	colnames(dates[[z]]) <- c("date",tileID)

#### check archive... download
	for (i in 1:nrow(dates[[z]])){

		year <- format(as.Date(dates[[z]][i,1],format="%Y.%m.%d"), "%Y")
		doy  <- as.integer(format(as.Date(dates[[z]][i,1],format="%Y.%m.%d"), "%j"))
		doy  <- sprintf("%03d",doy)
		datu <- paste("A",year,doy,sep="")
		mtr  <- rep(1,ntiles) # for file situation flaging

# creates local directory (HDF file container)
arcPath <- paste(LocalArcPath,slashes,PF2[z],PD,".",collection,slashes,dates[[z]][i,1],slashes,sep="")
dir.create(arcPath,showWarnings=FALSE,recursive=TRUE)

for(j in 1:ntiles){

dates[[z]][i,j+1] <- paste(PF2[z],PD,".",datu,".",if (tileID[j]!="GLOBAL") {paste(tileID[j],".",sep="")},collection,".*.hdf$",sep="") # create pattern
	
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

dir.create(paste(LocalArcPath,slashes,"LOGS",slashes,sep=""),showWarnings=FALSE)	
write.csv(dates[[z]],file=paste(LocalArcPath,slashes,"LOGS",slashes,PF2[z],PD,"_",collection,"_CHECK.csv",sep=""))

if(checkXML){xml <-  getXML(HdfName = list(paste(arcPath,dates[[z]][i,-1],sep="")),wait=wait)} # list() should not be needed

} # end dates i 
}else{ cat(paste("No files on ftp in date range for: ",PF2[z],PD,".",collection,"\n\n",sep=""))  }
} # if no files are avalable for product in date AND end platform z
} # end if not HdfName 
} ## END: FTP vs ARC check and download 



