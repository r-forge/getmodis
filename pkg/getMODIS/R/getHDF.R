# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : July 2011
# Version 0.2
# Licence GPL v3

# TODO arcPath: 'simple' files are stored by Product, 'complex' files are stored in ftp-like structure
  
getHDF <- function(LocalArcPath,HdfName,product,startdate,enddate,tileH,tileV,extent,collection,quiet=FALSE,wait=1,checkXML=FALSE) {

if (missing(LocalArcPath)) {
	if (.Platform$OS.type == "unix") {
		LocalArcPath <- "~/"
		LocalArcPath <- path.expand(LocalArcPath)
		LocalArcPath <- paste(LocalArcPath,"MODIS_ARC/",sep="")
		dir.create(LocalArcPath,showWarnings=FALSE)
		cat(paste("\n No arichve path set, using/creating standard archive in: ",LocalArcPath,"\n\n",sep=""))
		flush.console()
		} else {
		stop("'LocalArcPath' not set properly")
	}
}
# test local LocalArcPath
try(testDir <- list.dirs(LocalArcPath),silent=TRUE)
	if(!exists("testDir")) {stop("'LocalArcPath' not set properly")} 
#################

# if filename is provided other args are ignored (filename is ok for not too many files (because of high ftp-request frequency)

if (!missing(HdfName)){ 
	HdfName <- unlist(HdfName)
	for (i in seq(along=HdfName)){
	
	secName <- strsplit(HdfName[i],"\\.")[[1]] # decompose filename
		
		if (secName[length(secName)]!= "hdf"){stop(secName,"is not a good hdf HdfName")}
					
	PF <- substr(secName[1],1,3)
		
		if (!PF %in% c("MOD","MYD")) {stop(PF," not from TERRA or AQUA")}

	if(PF == "MOD"){PF1 <- "MOLT"} else {PF1 <- "MOLA"}

	date <- substr(secName[2],2,8)
	date <- format(as.Date(as.numeric(substr(date,5,7))-1,origin=paste(substr(date,1,4),"-01-01",sep="")),"%Y.%m.%d")
	collection <- secName[4]

	arcPath <- paste(secName[1],".",collection,"/",date,"/",sep="")
	dir.create(paste(LocalArcPath,arcPath,sep=""),recursive=TRUE,showWarnings=FALSE) # this always generates the same structure as the original ftp (this makes sense if the local LocalArcPath becomes big!)
	
		if (!file.exists(paste(LocalArcPath,arcPath,HdfName[i],sep=""))) {
		    require(RCurl)
			download.file(
				paste("ftp://e4ftl01u.ecs.nasa.gov/",PF1,"/", arcPath,HdfName[i],sep=""),
				destfile=paste(LocalArcPath, arcPath,HdfName[i],sep=""),
				mode='wb', method='wget', quiet=quiet, cacheOK=FALSE)
			
			if (wait!=0) {
				require(audio)
				wait(wait)				
			}

		}
		if(checkXML){getXML(HdfName = HdfName[i])}
	}
cat("downloaded: ",HdfName[i],"\n")

} else { # end by HdfName

if (missing(startdate)) {stop("Please provide a 'startdate' (format: 'YYYY.MM.DD')")} 
if (missing(enddate))   {stop("Please provide a 'endate' (format: 'YYYY.MM.DD')")} 
if (missing(extent) & (missing(tileH) | missing(tileV))){stop("Please provide eighter a 'tileH(s)' plus tileV(s) or an extent")} 
if (missing(product))   {stop("Please provide the MODIS-'product'")}
if (missing(collection)){stop("Please provide a product-'collection' (probably: '005')")} 


# following variables will be activated when the packge is ready for that
useExt        <- FALSE	## not implemented in the package version, needs LDOPE (if exist) use Extension file: "<Job>_1ULlat_2ULlon_3LRlat_4LRlon.txt"
forceFtpCheck <-  TRUE # if FTP connection doesn't work FALSE will prozess files available in ARC! if TRUE without FTP there is no processing 

# Check Platform and product
PF <- substr(product,2,2)

if (PF %in% c("x","X")) { # oioioi
		PF1  <- c("MOLT", "MOLA"); PF2  <- c("MOD", "MYD") 
} else {
	if (PF %in% c("y","Y")) {
		PF1  <- "MOLA"; PF2 <- "MYD"
	} else {
	if (PF %in% c("o","O")) {
		PF1  <- "MOLT"; PF2 <- "MOD"
		} else {
		stop("check 'product', the Platform spezific part seams to be wrong. Not one of 'MOD','MYD','MXD'. ")
		}
	}
} 


# Check product
PD <- substr(product,4,7)

if (!PD %in% c("13Q1", "09A1","09GA","09GQ", "09Q1")) { stop("at the moment supported only '13Q1', '09A1','09GA','09GQ', '09Q1', its easy to add other just tell me!")} 

# collection
collection <- sprintf("%03d",collection)

#### convert dates # TODO error handling
begin   <- as.Date(startdate,format="%Y.%m.%d") 
end     <- as.Date(enddate,format="%Y.%m.%d") 
####

#### 
# tileID
if(!missing(extent)) {
  tileID <- getTILE(extent=extent)
  } else {
  tileID <- getTILE(tileH=tileH,tileV=tileV)}
ntiles <- length(tileID)

dirALL <- list()
dates  <- list()

for(z in 1:length(PF1)){ # Platforms MOD/MYD

	ftp <- paste("ftp://e4ftl01u.ecs.nasa.gov/", PF1[z],"/", PF2[z],PD,".",collection,"/",sep="")

	require(RCurl) # the function doesn't start if it isn't able to check the ftpserver on entering... TODO force FTPcheck=FALSE
	FtpDayDirs  <- strsplit(getURL(ftp), "\n")[[1]] # its important to minimise getURL() queries, every check = risk of FTP breack + much time!
	FtpDayDirs <- FtpDayDirs[substr(FtpDayDirs, 1, 1)=='d'] # removes not usable folders i.e the first: "total 34128" 
	dirALL[[z]] <- unlist(lapply(strsplit(FtpDayDirs, " "), function(x){x[length(x)]})) # dir name below ftp

	sel <- as.Date(dirALL[[z]],format="%Y.%m.%d") # convert to date
	us  <- sel >= begin & sel <= end
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
arcPath <- paste(LocalArcPath,PF2[z],PD,".",collection,"/",dates[[z]][i,1],"/",sep="")
dir.create(arcPath,showWarnings=FALSE,recursive=TRUE)
 
for(j in 1:ntiles){ # in one date get tiles in tileID

dates[[z]][i,j+1] <- paste(PF2[z],PD[z],".",datu,".",tileID[j],".",collection,".*.hdf$",sep="") # create pattern
	
	if (length(dir(arcPath,pattern=dates[[z]][i,j+1]))>0){ # if file found locally
		HDF <- dir(arcPath,pattern=dates[[z]][i,j+1])  # extract only the HDF file
		
		if (length(HDF)>1) {
			select <- list()
			for (d in 1:length(HDF)){ # in very new files there are more than 1 file/tile if so take the last 
			select[[d]]<- strsplit(HDF[d],"\\.")[[1]][5]
			}
			HDF <- HDF[which.max(unlist(select))]		
			}
	
	dates[[z]][i,j+1] <- HDF
	mtr[j] <- 0
	}
}

 # if one of the tiles is missing, its necessary to go on ftp
if (sum(mtr)!=0) {
	require(RCurl)
	ftpfiles <- strsplit(getURL(paste(ftp, dates[[z]][i,1], "/", sep="")), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]] # get HDF in dates[[z]][i,] 
	if (ftpfiles[1] != "total 0") {ftpfiles <- unlist(lapply(strsplit(ftpfiles," "),function(x){x[length(x)]})) # found empty dir!
	for(j in 1:ntiles){ # go thought tiles in date
		if(mtr[j]!=0){ # if tile is missing get it
		onFtp <- grep(ftpfiles,pattern=dates[[z]][i,j+1],value=T)
		HDF   <- grep(onFtp,pattern=".hdf$",value=T)
			if (length(HDF)>1) {
			select <- list()
			for (d in 1:length(HDF)){ # in very new situations there are more than 1 file/tile/date if so: 
			select[[d]]<- strsplit(HDF[d],"\\.")[[1]][5] # get the last processed
			}
			HDF <- HDF[which.max(unlist(select))]		
			}
		dates[[z]][i,j+1] <- HDF
		
	hdf <- download.file(paste(ftp, dates[[z]][i,1], "/", HDF,sep=""), destfile=paste(arcPath, HDF, sep=""), mode='wb', method='wget', quiet=quiet, cacheOK=FALSE)
	mtr[j] <- hdf

        		if (wait > 0){
			require(audio) # for wait() # is it good here?
			wait(as.numeric(wait)) # waiting seams to decrease the chanse of ftp collapse
			}			
		}
	}
} else {dates[[z]][i,(j+1):ncol(dates[[z]])] <- "No files for that date on FTP"} # on ftp is possible to find empty folders!
}

if(checkXML){xml <-  getXML(HdfName = list(paste(arcPath,dates[[z]][i,-1],sep="")),wait=wait)} # list() should not be needed

dir.create(paste(LocalArcPath,"LOGS/",sep=""),showWarnings=FALSE)	
write.csv(dates[[z]],file=paste(LocalArcPath,"LOGS/",PF2[z],PD,"_",collection,"_CECK.csv",sep=""))

} # end dates i 
} # end Platform z
} # end if not file 
} ## END: FTP vs ARC check and download 
# ... post processings, require MRT and maybe LDOPE 


