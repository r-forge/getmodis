# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Version 0.1
# Licence GPL v3

getTILE <- function(tileH,tileV,extent) {

tiles <- list()

	if (!missing(tileH) && !missing(tileV)) {
		tileH <- as.vector(tileH)
			if (tileH < 1 || tileH > 36) {stop("'tileH' number(s) must be between 1 and 35")}
		tileV <- as.vector(tileV)
			if (tileV < 1 || tileV > 17) {stop("'tileV' number(s) must be between 1 and 17")}
		for (i in seq(along=tileH)){
			tiles[[i]] <- paste("h",sprintf("%02d",tileH[i]),"v",sprintf("%02d",tileV),sep="")	
		}
	} else {
	if (missing(extent)) {stop("Provide eighter informations for 'h'-tile(s) and 'v'-tile(s) or for 'extent'")}
stop("'getTILE' with 'extent' not yet implemented")
	}
return(unlist(tiles))
}

