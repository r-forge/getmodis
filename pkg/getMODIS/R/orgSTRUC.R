# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date: August 2011
# Licence GPL v3

orgSTRUC <- function(LocalArcPath,HdfName,to,remove=TRUE) {


if (missing(to)|(!to %in% 1:3)) stop("Provide a valid 'to' argument!")

#if (.Platform$OS.type == "unix") {
	slashes <- "/"
	ssplit <- slashes
#}else{
#	slashes <- "\\"
#	ssplit <- "\\\\"
#}

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
	if(!exists("testDir")) {stop("'LocalArcPath' not set properly")} 

if (missing(HdfName)) {cat(paste("No 'HdfName' pattern set, moving/coping all MODIS grid data found in '", LocalArcPath,"'.\n",sep=""))} 

#################

if(!missing(HdfName)) {
		avFiles <- unlist(list.files(LocalArcPath,pattern=HdfName,recursive=TRUE,full.names=TRUE))
	} else {
		avFiles <- unlist(list.files(LocalArcPath,pattern="hdf",recursive=TRUE,full.names=TRUE))
	}
avFiles <- unlist(gsub("/",ssplit,avFiles)) # treats slashes according to platform

data("MODIS_Products")

# tests if MODIS-grid file(s) # maybe using regex methods it becomes much faster!
doit <- sapply(avFiles,function(x) {
	name <- strsplit(x,ssplit)[[1]] # separate name from path
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

name   <- strsplit(x,ssplit)[[1]] # separate name from path
orpath <- name[-length(name)]
orpath <- paste(orpath,collapse=slashes)
name   <- name[length(name)] # select filename
secName  <- strsplit(name,"\\.")[[1]] # decompose filename

########################
# generate structure
info    <- MODIS_Products[which(MODIS_Products[,1] == secName[1]),]
basedir <- paste(secName[1],".",if (info[4]=="Tile"){secName[4]}else{secName[3]},sep="")
date    <- substr(secName[2],2,nchar(secName[2]))
date    <- as.Date(as.numeric(substr(date,5,nchar(date)))-1,origin=paste(substr(date,1,4),"-01-01",sep=""))
year    <- format(date,format="%Y")
date    <- gsub(x=date,"-",".")

if (to==1) {path <- paste(LocalArcPath,slashes,basedir,sep="")}
if (to==2) {path <- paste(LocalArcPath,slashes,basedir,slashes,year,sep="")}
if (to==3) {path <- paste(LocalArcPath,slashes,basedir,slashes,date,sep="")}

dir.create(path,showWarnings=FALSE,recursive=TRUE)
###################
# move files

if (!file.exists(paste(path,slashes,name,sep=""))) { # do nothing if file is already in dest dir 
	if (.Platform$OS.type == "unix" & remove) {
		system(paste("mv ",x," ",path,sep=""))
		moved <- 1
	} else if (.Platform$OS.type == "windows" & remove) {
		shell(paste("move ",x," ", path,sep=""),intern=T)
		moved <- 1
	} else {
		file.copy(from=x,to=paste(path,"/",name,sep=""),overwrite=FALSE)
		moved <- 2
		
		if (file.exists(paste(path,"/",name,sep="")) & orpath!=path & remove) {
			unlink(paste(orpath,name,sep=""))
		moved <- 1
		}
		
	}
}  else { moved <- 0 }
if (length(list.files(orpath))==0) {unlink(orpath,recursive=TRUE)} # delete emty dir
return(moved)})

cat("Moved ", sum(moved==1)," files!\n")
cat("Copied ", sum(moved==2)," files!\n")
cat("Not moved ", sum(moved==0)," files!\n")

}
#######################

