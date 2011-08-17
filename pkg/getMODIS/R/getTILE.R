# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Version 0.1
# Licence GPL v3

getTILE <- function(tileH,tileV,extent) {

tiles <- list()


# extent class "list"
if (!missing(extent) && class(extent) == "list"){

data("tiletable") # this file solves the dependency from LDOPE (for this task!)
  # Austria <- list(lat_min=46.12,lat_max=49.3,lon_max=17.47,lon_min=9.2) # h18v04 and h19v04

  minTile <- subset(tiletable,
                  (tiletable$lon_min <= extent$lon_min & tiletable$lon_max >= extent$lon_min) &
                  (tiletable$lat_min <= extent$lat_max & tiletable$lat_max >= extent$lat_max)
         ,select=c(iv,ih))
  minTile <- c(min(minTile$iv),min(minTile$ih))
    
  maxTile <-  subset(tiletable,
                  (tiletable$lon_min <= extent$lon_max & tiletable$lon_max >= extent$lon_max) &
                  (tiletable$lat_min <= extent$lat_min & tiletable$lat_max >= extent$lat_min)
         ,select=c(iv,ih))
  maxTile <- c(max(maxTile$iv),max(maxTile$ih))

  tileV <- minTile[1]:maxTile[1]
  tileH <- minTile[2]:maxTile[2]
}

# extent class "raster* object (extent)"
if (!missing(extent) && class(extent) %in% c("Extent","RasterLayer","RasterStack","RasterBrick") ){

require(raster) # should allready be loaded... but anyway

	if (class(extent) != "Extent"){  
		extent <- extent(extent)# checking if lat/lon !!??
	} 

data("tiletable")

  minTile <- subset(tiletable,
                  (tiletable$lon_min <= extent@xmin & tiletable$lon_max >= extent@xmin) &
                  (tiletable$lat_min <= extent@ymax & tiletable$lat_max >= extent@ymax)
         ,select=c(iv,ih))
  minTile <- c(min(minTile$iv),min(minTile$ih))
  
  maxTile <-  subset(tiletable,
                  (tiletable$lon_min <= extent@xmax & tiletable$lon_max >= extent@xmax) &
                  (tiletable$lat_min <= extent@ymin & tiletable$lat_max >= extent@ymin)
         ,select=c(iv,ih))
  maxTile <- c(max(maxTile$iv),max(maxTile$ih))

  tileV <- minTile[1]:maxTile[1]
  tileH <- minTile[2]:maxTile[2]
  
}
###########################
# from maps

#if (!missing(extent) && class(map)){
#stop("Maps object not supported yet!")

#require(maps)
#NAT <- identify(map("world",fill=T,col="red"))
#extent <- map("world",NAT)$range

# ex <- map("world")
# is <- grep(ex$names,pattern="Austria",ignore.case=T)

#}

####################################
# get the results

tileH <- as.vector(tileH)
	if (tileH < 0 | tileH > 36) {stop("'tileH' number(s) must be between 0 and 35")}
tileV <- as.vector(tileV)
	if (tileV < 0 | tileV > 17) {stop("'tileV' number(s) must be between 0 and 17")}
for (i in seq(along=tileH)){
	tiles[[i]] <- paste("h",sprintf("%02d",tileH[i]),"v",sprintf("%02d",tileV),sep="")	
}

return(unlist(tiles))
}

