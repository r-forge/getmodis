# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Licence GPL v3



runMRT <- function(LocalArcPath,ParaSource, MRTpath = "check") {

if (missing(ParaSource)) {stop("Provide a Parameter file see: data('ParaExample'))")}

fsep <- .Platform$file.sep

if (missing(LocalArcPath)) {
	LocalArcPath <- "~/"
	LocalArcPath <- normalizePath(path.expand(LocalArcPath), winslash = fsep)
	LocalArcPath <- paste(strsplit(LocalArcPath,fsep)[[1]],collapse=fsep) # removes "/" or "\" on last position (if present)
	LocalArcPath <- file.path(LocalArcPath,"MODIS_ARC",fsep=fsep)
	cat(paste("No archive path set, using/creating standard archive in: ",LocalArcPath,"\n",sep=""))
	flush.console()
}

LocalArcPath <- paste(strsplit(LocalArcPath,fsep)[[1]],collapse=fsep) # removes "/" or "\" on last position (if present)
dir.create(LocalArcPath,showWarnings=FALSE)
# test local LocalArcPath
try(testDir <- list.dirs(LocalArcPath),silent=TRUE)
if(!exists("testDir")) {stop("'LocalArcPath' not set properly!")} 
#################

if (!exists("extent"))  {stop("Provide a valid 'extent'.")}
if (!exists("Job"))     {stop("Provide a valid 'Job'-name")}
if (!exists("startdate")) {stop("Provide a 'startdate'")}
if (!exists("enddate")) {stop("Provide a 'enddate'")}
if (!exists("product")) {stop("Provide a MODIS product to be processed")}

if (!exists("outDir"))  {
	outDir <- "~/"
	outDir <- normalizePath(path.expand(outDir), winslash = fsep)
	outDir <- file.path(outDir,"MRTresults",Job,fsep=fsep)
	}
dir.create(outDir,recursive=TRUE,showWarnings=FALSE)
# test local LocalArcPath
try(testDir <- list.dirs(outDir),silent=TRUE)
if(!exists("testDir")) {stop("'outDir' not set properly!")} 
##############

if (!exists("pixelsize") | pixelsize=="") {
	cat("No output 'pixelsize' spezified, input size used!\n")
	pixelsize <- "asIn"
	} else {
	cat("resampling to pixelsize:", pixelsize,"\n")
	}

if (!exists("resample") | resample=="" ) {
	cat("No resampling method spezified, using nearest neighbor!\n")
	resample <- "NN"
	} else {
	cat("Resampling method:", resample,"\n")
	}

if (!exists("outProj") | outProj==""  ) {
	cat("No output projection spezified, using WGS84!\n")
	outProj <- "GEOGRAPHIC"
	} else {
	cat("Output projection:", outProj,"\n")
	}

######
if (MRTpath=="check") {
	MRTpath <- getPATH(quiet=TRUE)
	}
if (!file.exists(MRTpath)) {stop("'MRTpath' is wrong. Provide a good path, leave empty or run 'getPATH()'")}

product <- getPRODUCT(product=product)

# check collection
if (!exists(collection)) {
	collection <- getCOLLECTION(product=product)
	} else {
	collection <- sprintf("%03d",as.numeric(collection))
	if (!getCOLLECTION(product=product,collection=collection)) {stop(paste("The collection you have requested may doesn't exist run: 'getCOLLECTION(LocalArcPath='",LocalArcPath,"',product='",product$request ,"',forceCheck=TRUE,newest=FALSE)' to update internal list and see available once!",sep=""))}
	}

# after getSTRUC is called, getHDF can easily be called on single files...
# getSTRUC garants that all needed dir structure information is made avalable offline
ftpdirs <- getSTRUC(product=product$request,collection=collection,startdate=startdate,enddate=enddate)

for(i in 1:length(product$PF1)) { # along platform

avDates <- ftpdirs[,colnames(ftpdirs)==paste(product$productName[i],".",collection,sep="")]
avDates <- avDates[!is.na(avDates)]
avDates <- as.Date(avDates,format="%Y.%m.%d")

begin <- as.Date(startdate,format="%Y.%m.%d")
		if (is.na(begin)) {stop("\n'startdate=",startdate,"' is eighter wrong format (not:'YYYY.MM.DD') or an invalid date")}
end <- as.Date(enddate,format="%Y.%m.%d") 
		if (is.na(end)) {stop("\n'enddate=",enddate,"' is eighter wrong format (not:'YYYY.MM.DD') or an invalid date")}

	us  <- avDates >= begin & avDates <= end
	if (sum(us,na.rm=TRUE)>0){

avDates <- avDates[us]

for (l in 1:length(avDates)){ # along start-end-date

files <- getHDF(LocalArcPath=LocalArcPath,product=product$productName[i],collection=collection,startdate=avDates[i],enddate=avDates[i],extent=extent,log=FALSE)

if (file.exists(files)){

if (!exists("SDSstring")) {stop(paste("No 'SDSstring' is specified, run: 'getSDS(HdfName='",files[1],"')' to see which are available, and generate the sting",sep=""))
} else {
SDSstring <- getSDS(HdfName=files[1],SDSstring=SDSstring)
} 

if (!quiet & i == 1) {cat("Extracing SDS:",SDSstring$SDSnames,sep="\n")}


TmpMosNam <- paste("TmpMosaic",round(runif(1,1,10000)),".hdf",sep="") # to make sure access priority
### in subset
paraname <- file.path(outDir,"MRTgMosaic.prm",fsep=fsep) # create mosaic prm file ((removed prmDir put wrkdr))
filename = file(paraname, open="wt")
write(paste(files,sep='',collapse=' '), filename)
close(filename)

# run mosaic
if (.Platform$OS=="unix") {
		system(paste(MRTpath,fsep,"mrtmosaic -i ",paraname," -o ",outDir,fsep,TmpMosNam," -s '",SDSstring$SDSstring,"'" ,sep=""))
	} else { # TODO CHECK on Win!!
	  call(paste(MRTpath,fsep,"mrtmosaic -i ",paraname," -o ",outDir,fsep,TmpMosNam," -s '",SDSstring,"'" ,sep=""))
	}
unlink(paraname)

require(audio)
wait(1) # without wait the skript can break here. "wait" is a try but it seams to work!!!

basenam <- strsplit(files[1],fsep)[[1]]
basenam <- basenam[length(basenam)]
basenam <- paste(paste(strsplit(basenam,"\\.")[[1]][c(1,2,4)],collapse="."),Job,sep=".")

# TODO: output pixelsize, OUTPUT_PROJECTION_PARAMETERS...
paraname <- paste(outDir,"MRTgResample.prm",sep="")
filename = file(paraname, open="wt")
write(paste('INPUT_FILENAME = ',outDir,fsep,TmpMosNam,sep=""), filename)
write('SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG',filename)
write(paste('SPATIAL_SUBSET_UL_CORNER = (',extent$lat_max,' ',extent$lon_min,')',sep=''),filename)
write(paste('SPATIAL_SUBSET_LR_CORNER = (',extent$lat_min,' ',extent$lon_max,')',sep=''),filename)
write(paste('OUTPUT_FILENAME = ',outDir,fsep,basenam,'.tif',sep=''),filename) 
write(paste('RESAMPLING_TYPE = ',resample,sep=''),filename)
write(paste('OUTPUT_PROJECTION_TYPE = ',outProj,sep=''),filename)
write('OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )',filename)
write('DATUM = WGS84',filename)
close(filename)

if (.Platform$OS=="unix") {
		system(paste(MRTpath,fsep,"resample -p ",paraname,sep=""))
	} else { # TODO CHECK on Win!!
	  call(paste(MRTpath,fsep,"resample -p ",paraname,sep=""))
	}
unlink(paraname)
unlink(paste(outDir,fsep,TmpMosNam,sep=""))

} else {cat("missing files?",files,"jumping to the next date",sep="\n")}

}
} else {cat("No files found for that product within the date range\n")}
}
}
