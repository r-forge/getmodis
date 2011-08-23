# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date: August 2011
# Licence GPL v3


organiseStructure <- function(LocalArcPath,HdfName,to,remove=TRUE) {


if (missing(to)| (!to %in% c(1:3))) stop("Provide a valid 'to' argument!")
if (missing(LocalArcPath)) {LocalArcPath <- "./MODIS_ARC/"; cat("No 'LocalArcPath' set, looking in '", LocalArcPath,"'.\n")}
if (missing(HdfName)) {cat("No 'HdfName' pattern set, moving all MODIS grid data under '", LocalArcPath,"'.\n")} 
#######################


if (missing(LocalArcPath)) {
	if (.Platform$OS.type == "unix") {
		LocalArcPath <- "~/"
		LocalArcPath <- path.expand(LocalArcPath)
		LocalArcPath <- paste(LocalArcPath,"MODIS_ARC/",sep="")
		dir.create(LocalArcPath,showWarnings=FALSE)
		cat(paste("\nNo arichive path set, using/creating standard archive in: ",LocalArcPath,"\n\n",sep=""))
		flush.console()
		} else {
		stop("'LocalArcPath' not set properly")
	}
}
# test local LocalArcPath
try(testDir <- list.dirs(LocalArcPath),silent=TRUE)
	if(!exists("testDir")) {stop("'LocalArcPath' not set properly")} 
#################

if(!missing(HdfName)) {
		avFiles <- unlist(list.files(LocalArcPath,pattern=HdfName,recursive=TRUE,full.names=TRUE))
	} else {
		avFiles <- unlist(list.files(LocalArcPath,pattern="hdf",recursive=TRUE,full.names=TRUE))
	}

data("MODIS_Products")

# tests if MODIS-grid file(s)
doit <- sapply(avFiles,function(x) {
	name <- strsplit(x,"/")[[1]] # separate name from path
	name <- name[length(name)] # select filename
	secName  <- strsplit(name,"\\.")[[1]] # decompose filename

	if(secName[1] %in% MODIS_Products[,1]) {
	info <- MODIS_Products[which(MODIS_Products[,1] == secName[1]),]
		
		PF <- substr(secName[1],1,3)
		Tpat <- "h[0-3][0-9]v[0-1][0-9]" # to enhance

		if(info[4]=="CMG" && (sum((substr(secName[2],1,1) == "A") + (PF %in% c("MOD","MYD","MCD"))) == 2 )) #? M[o,O,y,Y,c,C]D
			{ res <- TRUE }
		else if(info[4]=="Tile" && (sum((substr(secName[2],1,1) == "A") + (PF %in% c("MOD","MYD","MCD")) + (grep(secName[3],pattern=Tpat)))==3))
			{ res <- TRUE }
		else 
			{ res <- FALSE }
	
	} else { res <- FALSE }
	return(res)}
	)

avFiles <- avFiles[doit] 
cat("Found",length(avFiles),"files \n")
#########################
moved <- sapply(avFiles,function(x) {


	name   <- strsplit(x,"/")[[1]] # separate name from path
	orpath <- name[-length(name)]
	orpath <- paste(orpath,collapse="/")
	orpath <- paste(paste(strsplit(orpath,"//")[[1]],collapse="/"),"/",sep="")
	name   <- name[length(name)] # select filename
	secName  <- strsplit(name,"\\.")[[1]] # decompose filename

########################
# generate structure
	info <- MODIS_Products[which(MODIS_Products[,1] == secName[1]),]
basedir <- paste(secName[1],".",if (info[4]=="Tile"){secName[4]}else{secName[3]},sep="")
date    <- substr(secName[2],2,nchar(secName[2]))
date    <- as.Date(as.numeric(substr(date,5,nchar(date)))-1,origin=paste(substr(date,1,4),"-01-01",sep=""))
year    <- format(date,format="%Y")
date    <- gsub(x=date,"-",".")

if (to==1) {path <- paste(LocalArcPath,basedir,"/",sep="")}
if (to==2) {path <- paste(LocalArcPath,basedir,"/",year,"/",sep="")}
if (to==3) {path <- paste(LocalArcPath,basedir,"/",date,"/",sep="")}

dir.create(path,showWarnings=FALSE,recursive=TRUE)
moved <- file.copy(from=x,to=paste(path,name,sep=""),overwrite=FALSE)
if (file.exists(paste(path,name,sep="")) & orpath!=path & remove) {unlink(paste(orpath,name,sep=""))}
if (length(list.files(orpath))==0) {unlink(orpath,recursive=TRUE)}
return(moved)
})

cat("Moved ", sum(moved)," files!\n")
cat("Not moved ", sum(!moved)," files!\n")

}
########################


