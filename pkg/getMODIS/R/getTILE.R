# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Version 0.1
# Licence GPL v3

getTILE <- function(tileH,tileV,extent) {

tiles <- list()

if (!missing(extent) && class(extent) == "list"){
data("tiletable") # this file solves the dependency from LDOPE (for this task!)
  # Austria <- list(lat_min=46.12,lat_max=49.3,lon_max=17.47,lon_min=9.2) # h18v04 and h19v04

  minTile <- subset(tiletable,
                  (tiletable$lon_min <= extent$lon_min & tiletable$lon_max >= extent$lon_min) &
                  (tiletable$lat_min <= extent$lat_min & tiletable$lat_max >= extent$lat_min)
         ,select=c(iv,ih))
  
  maxTile <-  subset(tiletable,
                  (tiletable$lon_min <= extent$lon_max & tiletable$lon_max >= extent$lon_max) &
                  (tiletable$lat_min <= extent$lat_max & tiletable$lat_max >= extent$lat_max)
         ,select=c(iv,ih))

  tileV <- minTile$iv:maxTile$iv
  tileH <- minTile$ih:maxTile$ih
}

if (!missing(extent) && class(extent) == "Extent"){
stop("todo")
data("tiletable") # this file solves the dependency from LDOPE (for this task!)
}


tileH <- as.vector(tileH)
	if (tileH < 1 || tileH > 36) {stop("'tileH' number(s) must be between 1 and 35")}
tileV <- as.vector(tileV)
	if (tileV < 1 || tileV > 17) {stop("'tileV' number(s) must be between 1 and 17")}
for (i in seq(along=tileH)){
	tiles[[i]] <- paste("h",sprintf("%02d",tileH[i]),"v",sprintf("%02d",tileV),sep="")	
}


#if (missing(extent)) {stop("Provide eighter informations for 'tileH' and 'tileV' or an 'extent'")}
#stop("'getTILE' with 'extent' not yet implemented")

#if (!class(extent)!=c("Extent","list")){stop("'extent' must be eighter a list like: 'myExtent <- list(lon_min=numeric,lon_max=numeric,lat_min=numeric,lat_max=numeric)' or a 'extent' from raster package!")}} 

return(unlist(tiles))
}

