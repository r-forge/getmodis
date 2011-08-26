# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date: August 2011
# Licence GPL v3


getSTRUC <- function(LocalArcPath,product,collection,startdate,enddate,wait=1) {



fsep <- .Platform$file.sep

if (missing(LocalArcPath)) {
	LocalArcPath <- "~/"
	LocalArcPath <- normalizePath(path.expand(LocalArcPath), winslash = fsep)
	LocalArcPath <- paste(strsplit(LocalArcPath,fsep)[[1]],collapse=fsep)# removes "/" or "\" on last position (if present)
	LocalArcPath <- file.path(LocalArcPath,"MODIS_ARC",fsep=fsep)
} 
dir.create(LocalArcPath,showWarnings=FALSE)
# test local LocalArcPath
try(testDir <- list.dirs(LocalArcPath),silent=TRUE)
if(!exists("testDir")) {stop("'LocalArcPath' not set properly")} 

auxPATH <- file.path(LocalArcPath,".auxiliaries",fsep=fsep)
dir.create(auxPATH,showWarnings=FALSE)

# Check Platform and product
PF <- substr(product,2,2)

#TODO if MCD, PF1 == "MOTA")
if 	  (PF %in% c("x","X")) { PF1  <- c("MOLT", "MOLA"); PF2  <- c("MOD", "MYD") 
} else if (PF %in% c("y","Y")) { PF1  <- "MOLA"; PF2 <- "MYD"
} else if (PF %in% c("o","O")) { PF1  <- "MOLT"; PF2 <- "MOD"
} else if (PF %in% c("c","C")) { PF1  <- "MOTA"; PF2 <- "MCD"
} else {stop("Check 'product', the Platform specific part seams wrong. Not one of 'MOD','MYD','MXD','MCD'.")
}
####
PD <- substr(product,4,nchar(product)) #'09Q1',...
#####
# collection
collection <- sprintf("%03d",as.numeric(collection))

# load aux
if (file.exists(file.path(auxPATH,"ftpdir.txt",fsep=fsep))) {
	ftpdirs <- read.table(file.path(auxPATH,"ftpdir.txt",fsep=fsep),stringsAsFactors=FALSE)
	} else {
	ftpdirs <- list()
	}
	

# validity check 2
for (i in 1:length(PF2)){
	
	productName <- paste(PF2[i],PD, ".",collection,sep="")
	
		if (productName %in% names(ftpdirs)) {
			createNew <- FALSE
			ind <- which(names(ftpdirs)==productName)

			if (length(ftpdirs[,ind]) == 0 ) { # ...relevant for the first time only
				getIT <- TRUE
			} else {

				avDates <- as.Date(ftpdirs[,ind],format="%Y.%m.%d")
		
				if (!missing(startdate)){
					begin <- as.Date(startdate,format="%Y.%m.%d")
						if (is.na(begin)) {stop("\n'startdate=",startdate,"' is eighter wrong format (not:'YYYY.MM.DD') or an invalid date")}
					if (begin < min(avDates,na.rm=TRUE)) {
					getIT <- TRUE
					} else {
					getIT <- FALSE
					}
				} else {
					getIT <- TRUE
				}
	
				if (!missing(enddate) & !getIT) {
					end <- as.Date(enddate,format="%Y.%m.%d") 
						if (is.na(end)) {stop("\n'enddate=",enddate,"' is eighter wrong format (not:'YYYY.MM.DD') or an invalid date")}
					if (end > max(avDates,na.rm=TRUE)) {
					getIT <- TRUE
					} else {
					getIT <- FALSE
					}
				} else {
					getIT <- TRUE
				}
			}
		} else {
			getIT <- TRUE
			createNew <- TRUE
		}

	if (getIT) {

		ftp <- paste("ftp://e4ftl01u.ecs.nasa.gov/",PF1[i],"/", productName,"/",sep="")
		cat("Getting:", ftp,"\n")	
		require(RCurl)
		FtpDayDirs  <- getURL(ftp)
	
			if (wait > 0 & i != length(PF2)) {
					require(audio)
					wait(wait)
					}
	
		FtpDayDirs  <- unlist(strsplit(FtpDayDirs[[1]], if(.Platform$OS.type=="unix"){"\n"}else{"\r\n"})) # Is this enought? Mac? Solaris?....
		FtpDayDirs  <- FtpDayDirs[substr(FtpDayDirs, 1, 1)=='d'] 
		FtpDayDirs  <- unlist(lapply(strsplit(FtpDayDirs, " "), function(x){x[length(x)]}))
	} else {
		FtpDayDirs <- ftpdirs[,ind]
	}

	if (createNew) { 
		FtpDayDirs <- matrix(FtpDayDirs)
		mtr <- matrix(NA,ncol=ncol(ftpdirs)+1,nrow=max(dim(FtpDayDirs)[1],dim(ftpdirs)[1]))
		colnames(mtr) <- c(colnames(ftpdirs),productName)	
			for(j in 1:ncol(ftpdirs)){
				mtr[,j] <- replace(mtr[,j], 1:length(ftpdirs[,j]),ftpdirs[,j])
			}
		mtr[,ncol(mtr)] <- replace(mtr[,ncol(mtr)], 1:length(FtpDayDirs),FtpDayDirs) 
		ftpdirs <- mtr
	}

}

write.table(ftpdirs,file.path(auxPATH,"ftpdir.txt",fsep=fsep))

invisible(0) 
}




