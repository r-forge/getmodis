# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Version 0.1
# Licence GPL v3

getTILE <- function(tileH,tileV,extent) {

###########################################
# from maps

if (!missing(extent) && inherits(extent,"map")){
extent <- list(lat_min=min(extent$range[3:4]),lat_max=max(extent$range[3:4]),lon_min=min(extent$range[1:2]),lon_max=max(extent$range[1:2]))
}
############################################
# extent class "raster* object (extent)"
if (!missing(extent) && class(extent) %in% c("Extent","RasterLayer","RasterStack","RasterBrick") ){

require(raster) # should allready be loaded... but anyway

	if (class(extent) != "Extent"){  
		extent <- extent(extent)# checking if lat/lon !!??
	} 

extent <- list(lat_min=extent@ymin,lat_max=extent@ymax,lon_min=extent@xmin,lon_max=extent@xmax)
}

####################################
# extent class "list"
if (!missing(extent) && class(extent) == "list"){ # !missing() refers to function var, class() can be result of if raster&Co...

data("tiletable")

  minTile <- subset(tiletable,
                  (tiletable$lon_min <= extent$lon_min & tiletable$lon_max >= extent$lon_min) &
                  (tiletable$lat_min <= extent$lat_min & tiletable$lat_max >= extent$lat_min)
         ,select=c(iv,ih))
  minTile <- c(min(minTile$iv),min(minTile$ih))
    
  maxTile <-  subset(tiletable,
                  (tiletable$lon_min <= extent$lon_max & tiletable$lon_max >= extent$lon_max) &
                  (tiletable$lat_min <= extent$lat_max & tiletable$lat_max >= extent$lat_max)
         ,select=c(iv,ih))
  maxTile <- c(max(maxTile$iv),max(maxTile$ih))

  tileV <- minTile[1]:maxTile[1]
  tileH <- minTile[2]:maxTile[2]
}

###################################
# get the results
tiles <- list()

tileH <- as.vector(tileH)
	if (tileH < 0 || tileH > 36) {stop("'tileH' number(s) must be between 0 and 35")}
tileV <- as.vector(tileV)
	if (tileV < 0 || tileV > 17) {stop("'tileV' number(s) must be between 0 and 17")}
	
for (i in seq(along=tileH)){
	tiles[[i]] <- paste("h",sprintf("%02d",tileH[i]),"v",sprintf("%02d",tileV),sep="")	
}
result <- list(tile=unlist(tiles),tileH=tileH,tileV=tileV)

return(result)
}

