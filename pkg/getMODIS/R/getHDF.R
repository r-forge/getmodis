# Author: Matteo Mattiuzzi, matteo@mattiuzzi.com
# Date : July 2011
# Version 0.2
# Licence GPL v3


getHDF <- function(LocalArcPath,...) {

if (missing(LocalArcPath)) {
	if (.Platform$OS.type == "unix") {
		LocalArcPath <- "~/"
		LocalArcPath <- path.expand(LocalArcPath)
		LocalArcPath <- paste(LocalArcPath,"MODIS_ARC/",sep="")
		dir.create(paste(LocalArcPath,"MODIS_ARC",sep=""),showWarnings=FALSE)
		cat(paste("No arichve path set, using/creating standard archive in: ",LocalArcPath,"\n",sep=""))
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
if (!missing("file"){ 
	file <- unlist(file)
	for (i in seq(along=file)){
	
	secName <- strsplit(file[i],"\\.")[[1]] # decompose filename
		
		if (secName[length(secName)]!= "hdf"){stop(secName,"is not a good hdf file")}
					
	PF <- substr(secName[1],1,3)
		
		if (!PF %in% c("MOD","MYD")) {stop(PF,"not from TERRA or AQUA")}

	if(PF=="MOD"){PF <- "MOLT"}else {PF <- "MOLA"}

	date <- substr(secName[2],2,8)
	date <- format(as.Date(as.numeric(substr(date,5,7))-1,origin=paste(substr(date,1,4),"-01-01",sep="")),"%Y.%m.%d")
	version <- secName[4]

	arcPath <- paste(PF,"/",secName[1],".",version,"/",date,"/",sep="")
	dir.create(paste(LocalArcPath,arcPath,recursive=TRUE,showWarnings=FALSE) # this always generates the same structure as the original ftp (this makes sense if the local LocalArcPath becomes big!
	
		if (!file.exists(paste(LocalArcPath,arcPath,file[i],sep="")) {
			require(RCurl)
			download.file(
				paste("ftp://e4ftl01u.ecs.nasa.gov/", arcPath,file[i],sep=""),
				destfile=paste(LocalArcPath, arcPath,file[i],sep=""),
				mode='wb', method='wget', quiet=F, cacheOK=FALSE)
			
			if (!missing(wait) && wait!=0) {
				require(audio)
				wait(wait)				
			}

		}
	}
cat("downloaded: ",file,"\n")

} else { # end by file

if (missing(startdate)){stop("Please provide a 'startdate' (format: 'YYYY.MM.DD')")} 
if (missing(enddate))  {stop("Please provide a 'endate' (format: 'YYYY.MM.DD')")} 
if (missing(tileID))   {stop("Please provide the 'tileID(s)' ('hXXvXX')")} 
if (missing(Product))  {stop("Please provide the MODIS-'Product' (i.e. '13Q1')")}
if (missing(Platform)) {stop("Please provide a 'Platform' (or 'TERRA' or 'AQUA' od 'both')")} 
if (missing(Version))  {stop("Please provide a Product-'Version' (probably: '005')")} 


# following variables will be activated when the packge is ready for that
useExt        <- FALSE	## not implemented in the package version, needs LDOPE (if exist) use Extension file: "<Job>_1ULlat_2ULlon_3LRlat_4LRlon.txt"
forceFtpCheck <-  TRUE # if FTP connection doesn't work FALSE will prozess files available in ARC! if TRUE without FTP there is no processing 

# Check Platform
if (!Platform %in% c("both","TERRA", "AQUA")) {stop("'Platform' must be one of: 'both','TERRA', 'AQUA'")} # if comined product is needed ("MCD" please contact me schoud work easily but not testet for now!
if (Platform == "both"){PF1  <- c("MOLT", "MOLA"); PF2  <- c("MOD", "MYD") }else{
			if(Platform == "TERRA"){PF1 <- "MOLT" ; PF2 <- "MOD"
			}else{
			if(Platform == "AQUA"){PF1 <- "MOLA" ; PF2 <- "MYD"}
			}
		}

# Check Product
if (!Product %in% c("13Q1", "09A1","09GA","09GQ", "09Q1")) { stop("at the moment supported only '13Q1', '09A1','09GA','09GQ', '09Q1', it easy to add other just tell me!")} 

### FTP-Dir composition
ftps <- rep(NA,length(PF1))
for (u in 1:length(PF1)){
ftps[u] <- paste("ftp://e4ftl01u.ecs.nasa.gov/", PF1[u],"/", PF2[u],Product,".",Version,"/",sep="")
}

####

# handle dates # provisoric will be simplefied
begin   <- as.Date(startdate,format="%Y.%m.%d") 
end     <- as.Date(enddate,format="%Y.%m.%d") 

beginYYYYDDD <- format(begin,"%Y%j")
endYYYYDDD   <- format(end,"%Y%j")

beginYEAR <- format(begin,"%Y")
endYEAR   <- format(end,"%Y")
beginDOY  <- format(begin,"%j")
endDOY    <- format(end,"%j")
####

#### 
# tileID (can be a single tile, or a list/vector of toles
# i.e. tileID <- c("h19v04","h19v05")
tileID <- unlist(tileID)
ntiles <- length(tileID)


	}
	name <- strsplit(avFiles[u],"/")[[1]] # separate name from path
	name <- name[length(name)] # select filename

	secName  <- strsplit(name,"\\.")[[1]] # decompose filename

	PF <- substr(secName[1],1,3)
	if(PF=="MOD"){PF <- "MOLT"}else {PF <- "MOLA"}

	date <- substr(secName[2],2,8)
	date <- format(as.Date(as.numeric(substr(date,5,7))-1,origin=paste(substr(date,1,4),"-01-01",sep="")),"%Y.%m.%d")

	version <- secName[4]




 }


dirALL <- list()
dates  <- list()

for(z in 1:length(PF1)){ # Platforms MOD/MYD

FtpDayDirs  <- strsplit(getURL(ftps[z]), "\n")[[1]] # its important to minimise getURL queries, every check = risk of FTP "breack" + takes its time!
FtpDayDirs <- FtpDayDirs[substr(FtpDayDirs, 1, 1)=='d'] # removes not usable folders i.e the first: "total 34128" 
dirALL[[z]] <- unlist(lapply(strsplit(FtpDayDirs, " "), function(x){x[length(x)]})) # dir name below ftps[z]

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
mtr  <- matrix(1,ncol=ntiles,nrow=2) # for file situation flaging

# creates local directory
outArcPath <- paste(LocalArcPath,PF2[z],Product,".",Version,"/",dates[[z]][i,1],"/",sep="")
dir.create(outArcPath,showWarnings=FALSE,recursive=TRUE)
 
for(j in 1:ntiles){ # in one date get tiles in tileID

dates[[z]][i,j+1] <- paste(PF2[z],Producttype,".",datu,".",tileID[j],".",Version,".*.hdf",sep="") # create pattern
	
	if (length(dir(outArcPath,pattern=dates[[z]][i,j+1]))>0){  # if file found locally with the pattern
		onArc <- dir(outArcPath,pattern=dates[[z]][i,j+1]) # get matching files on ARC (HDF + xml)
		dates[[z]][i,j+1]  <- grep(onArc,pattern=".hdf$",value=T) #extract only the HDF file
			if(checkXML){xml <-  getXML(HdfName = paste(outArcPath,dates[[z]][i,j+1],sep=""),wait=1)
			mtr[2,j] <- xml # value returned from getXML os 0 
			} # if checkXML is TRUE && xml not local, it is downloaded here
	
	}
} # pattern is genereted for all files in date, if files where available the full name inserted and if enabled checkXML xml file downloaded! 

# of some files where missing, do the following
if (sum(mtr)!=0) {ftpfiles <- strsplit(getURL(paste(ftps[z], dates[[z]][i,1], "/", sep="")), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]] # if hdf OR xml is missing
	if (ftpfiles[1] != "total 0") {ftpfiles <- unlist(lapply(strsplit(ftpfiles," "),function(x){x[length(x)]}))
		for(j in 1:ntiles){ ### MATTEEEEEEEOOO THINK AGAIN ON THAT!!!
			if(sum(mtr[,j])!=0){
				onFtp <- grep(ftpfiles,pattern=dates[[z]][i,j+1],value=T)
				HDF   <- grep(onFtp,pattern=".hdf$",value=T)
					if (length(HDF)>1) {
						select <- list()
						for (d in 1:length(HDF)){ # in very new files often there are more than 1 file/tile if so: 
							select[[d]]<- strsplit(HDF[d],"\\.")[[1]][5] #"..YYYYDDD...hdf" which.max gets the last processed!
						}
					HDF <- HDF[which.max(unlist(select))]		
					}
					
			if(checkXML){xml <-  getXML(HdfName = paste(outArcPath,dates[[z]][i,j+1],sep=""),wait=1)
			mtr[2,j] <- xml # value returned from getXML os 0 
			} # if checkXML is TRUE && xml not local, it is downloaded here
			
				if(mtr[1,j]==1 & length(HDF)==1){download.file(paste(ftps[z], dates[[z]][i,1], "/", HDF,sep=""), destfile=paste(outArcPath, HDF, sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
				mtr[1,j] <- 0} # get XML info (for filesize!)
			if(checkXML)	{if(mtr[2,j]==1 & length(XML)==1){download.file(paste(ftps[z], dates[[z]][i,1], "/", XML,sep=""), destfile=paste(outArcPath, XML, sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)
				mtr[2,j] <- 0}}
				if (length(HDF)==0) {dates[[z]][i,j+1] <- "HDF missing on FTP"} else {dates[[z]][i,j+1] <- HDF}
				#if (length(XML)==0) {outInfo2 <- "XML missing on FTP"} {outInfo2 <- XML} # use a missing code?
			}
		}
	} else {dates[[z]][i,(j+1):ncol(dates[[z]])] <- "no files for that date on FTP"} # on ftp is possible to find empty folders!
  	}

for(j in 1:ntiles){ # a kind of checksum (an additional check!)
	if(sum(mtr[,j])==0){
		onArc  <- dir(outArcPath,pattern=dates[[z]][i,j+1]) 
		HDF    <- grep(onArc,pattern=".hdf$",value=T)
		if(checkXML){XML    <- grep(onArc,pattern=".hdf.xml$",value=T)
			if (length(XML)==1) {MetaFileSize <- as.numeric(system(paste("stat -c %s ",outArcPath,XML,sep=""), intern=T)) # is xml available? 
				if (MetaFileSize > 9000) { # is the size of the xml reasonable? than do the size test, else...next time next chance...!
					XML    <- xmlParse(paste(outArcPath,XML,sep="")) # removed "try()". T think it was just forgotten after a test! (
					MetaSize <- getNodeSet( XML, "/GranuleMetaDataFile/GranuleURMetaData/DataFiles/DataFileContainer/FileSize" )
					MetaSize <- as.numeric(xmlValue( MetaSize[[1]] ))
					FileSize <- as.numeric(system(paste("stat -c %s ",outArcPath,dir(outArcPath,pattern=paste(HDF,"$",sep="")),sep=""), intern=T))

				if (MetaSize != FileSize) {
					cat("\nMETA check for file:",HDF,"\nFileSize:",FileSize,"but expected:",MetaSize,"\n")
					flush.console()
					download.file(paste(ftps[z], dates[[z]][i,1], "/", HDF,sep=""), destfile=paste(outArcPath, HDF, sep=""), mode='wb', method='wget', quiet=T, cacheOK=FALSE)}
				}
			}
		}
	}
}
	
write.csv(dates[[z]],file=paste("/home/arc/MODIS_Parameterfiles/LOGS/",Job,"_",PF2[z],Producttype,"_CECK.csv",sep=""))

## END: FTP vs ARC check and download

if(sum(mtr[,1])!=ncol(mtr)){	

drn <- paste(wrkdr,Job,"/",sep="")
if (.Platform$OS.type == "unix") {dir.create(drn,showWarnings=F, mode = "777")} else {dir.create(drn,showWarnings=F)}

drn <- paste(drn,Producttype,"/",sep="")
if (.Platform$OS.type == "unix") {dir.create(drn,showWarnings=F, mode = "777")} else {dir.create(drn,showWarnings=F)}

basenam <- unlist(lapply(strsplit(dates[[z]][i,2], "\\."), function(x){x[1:length(x)]}))
basenam <- paste(basenam[1:2],sep="",collapse="_")

#if(file.exists(paste(drn,Job,"_1ULlat_2ULlon_3LRlat_4LRlon.txt",sep=""))){
#	genext <- as.matrix(read.table(paste(drn,Job,"_1ULlat_2ULlon_3LRlat_4LRlon.txt",sep=""),sep=","),dec=".")
#	ULlatC <- genext[1]
#	ULlonC <- genext[2]
#	LRlatC <- genext[3]
#	LRlonC <- genext[4]
#	if(ULlatC == ULlat & ULlonC == ULlon & LRlatC == LRlat & LRlonC == LRlon)
#}

exte <- as.matrix(c(ULlat,ULlon,LRlat,LRlon))
rownames(exte) <- c("ULlat","ULlon","LRlat","LRlon")
write.table(exte,paste(drn,Job,"_1ULlat_2ULlon_3LRlat_4LRlon.txt",sep=""),col.names=FALSE,sep=",") # make a save copy of EXTENT information in output dir

# Part that handles with SDS names 
sds <- system(paste("/home/arc/MODIS_Processing_Tools/LDOPE_Linux_bin/bin/read_sds_attributes -help ", paste(archive,PF2[z],Producttype,".",Version,"/",dates[[z]][i,2],sep='',collapse=' '),sep=""),intern=T)
SDSnum <- as.numeric(strsplit(SDSstring," ")[[1]])

sdsl <- list()
for (f in 1:length(sds)){
sdsl[[f]] <- strsplit(sds[[f]],"\t")[[1]][2]

	if(Producttype=="13Q1"){
		sdsl[[f]] <- strsplit(sdsl[[f]]," \\(4800 x 4800)")[[1]][1]
		sdsl[[f]] <- paste(strsplit(sdsl[[f]]," ")[[1]],sep="",collapse="_")
	} else {
sdsl[[f]] <- strsplit(sdsl[[f]]," ")[[1]][1]
	}
}

sds <- unlist(sdsl)
inQuest <- paste(basenam, sds[1+(which(SDSnum==1))],"tif",sep=".")

if(extractQA) {
	if (Producttype=="13Q1") {
		QCfile <- grep(inQuest,pattern="_VI_",ignore.case=TRUE,value=T)
	} else{
		QCfile <- grep(inQuest,pattern="QC_",ignore.case=TRUE,value=T)
	}
QCout  <- paste(basenam,'.QC_',QcOutNam,'.tif',sep='')
inQuest<- c(inQuest,QCout)
}

if(sum(!inQuest %in% dir(drn))!=0){ # todo add QCout

TmpMosNam <- paste("TmpMosaic",round(runif(1,1,10000)),".hdf",sep="") # to make sure access priority
### in subset
paraname <- paste(drn,"MRTgMosaic.prm",sep="") # create mosaic prm file ((removed prmDir put wrkdr))
filename = file(paraname, open="wt")
write(paste(paste(outArcPath,dates[[z]][i,2:(ntiles+1)],sep='',collapse=' '),sep=''), filename)
close(filename)
# run mosaic
system(paste("/home/arc/MODIS_Processing_Tools/MRT/bin/mrtmosaic -i ",paraname," -o ",drn,TmpMosNam," -s '",SDSstring,"'" ,sep=""))

unlink(paraname)

wait(1) # with daily data not every "i" is resampled, it seams that R goes to fast to the next process! "wait" is a try but it seams to work!!!

paraname <- paste(drn,"MRTgResample.prm",sep="") # create resample prm file ((removed prmDir put wrkdr))
filename = file(paraname, open="wt")
write(paste('INPUT_FILENAME = ',drn,TmpMosNam,sep=""), filename)
write('SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG',filename)
write(paste('SPATIAL_SUBSET_UL_CORNER = (',ULlat,' ',ULlon,')',sep=''),filename)
write(paste('SPATIAL_SUBSET_LR_CORNER = (',LRlat,' ',LRlon,')',sep=''),filename)
write(paste('OUTPUT_FILENAME = ',drn,basenam,'.tif',sep=''),filename) 
write(paste('RESAMPLING_TYPE = ',RT,sep=''),filename)
write(paste('OUTPUT_PROJECTION_TYPE = ',Proj,sep=''),filename)
write('OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0  )',filename)
write('DATUM = WGS84',filename)
close(filename)

system(paste("/home/arc/MODIS_Processing_Tools/MRT/bin/resample -p ",paraname,sep=""))  # run resample prm file ((removed prmDir put wrkdr))

unlink(paste(drn,TmpMosNam,sep="")) # delete temp hdf-File
unlink(paraname)

###################
gc()    #####
#########
if (rmhdf){
for(j in 1:ntiles){
unlink(paste(dates[[z]][i,j+1],sep=""))
      }
}
#unlink("paraname")
gc()
### 

# extract QA
if (extractQA) {
qc <- raster(paste(drn,QCfile,sep=""))
	if(BitShift==0){
		aa <- calc(qc,function(x){bitAnd(x,BitMask)}) # I think separating it on BitShift==0 should make it faster!
	} else{
		aa <- calc(qc,function(x){bitShiftR(bitAnd(x,BitMask),BitShift)})
	}
aa <- writeRaster(aa,filename=paste(drn,QCout,sep=""),overwrite=T)
}

} # end inQuest
} # sum(mtr[,1])!=ncol(mtr)
} # end i (dates)
rm(items)} # end z (PF1 Platform)
} 
####################################


