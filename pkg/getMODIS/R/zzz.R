# zzz.R taken from raster package
.onLoad <- function(lib, pkg)  {
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkg), fields=c("Version","Date")))
	packageStartupMessage(paste("\n",pkg, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")\n\nThe code of 'getMODIS' has been shifted to the package 'MODIS' and will not be updated anymore.\nPlease install the new package with 'install.packages('MODIS', repos='http://R-Forge.R-project.org').\nThe 'MODIS' package will contain a much wider range of 'MODIS' relevant functionalities and a much stronger connection to other R-packages.\nBug reports and ideas always welcome!", sep=""))
	
	tst <- try( removeTmpFiles(), silent=TRUE )

	return(invisible(0))
}

