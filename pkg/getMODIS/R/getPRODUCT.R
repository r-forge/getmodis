# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Licence GPL v3

getPRODUCT <- function(product,quiet=TRUE){

# if product is already a result of getPRODUCT turn it bach to the original request.
if (is.list(product) && names(product) %in% c("request","PF1","PF2","PD","raster_type","productName")) { # matching with class should be done here!
	product <- product$request
	} 

# Check Platform and product
PF <- substr(product,2,2)

#TODO if MCD, PF1 == "MOTA")
if        (PF %in% c("x","X")) { PF1  <- c("MOLT", "MOLA"); PF2  <- c("MOD", "MYD") 
} else if (PF %in% c("y","Y")) { PF1  <- "MOLA"; PF2 <- "MYD"
} else if (PF %in% c("o","O")) { PF1  <- "MOLT"; PF2 <- "MOD"
} else if (PF %in% c("c","C")) { PF1  <- "MOTA"; PF2 <- "MCD"
} else {stop("Check 'product', the Platform specific part seams wrong. Not one of 'MOD','MYD','MXD','MCD'.")
}

# Check product
PD <- substr(product,4,nchar(product)) #'09Q1',...
#####

data("MODIS_Products")

productName <- list()
# validy check and information
for (i in 1:length(PF2)){

	if (paste(PF2[i],PD,sep="") %in% MODIS_Products[,1]) {
	ind <- which(MODIS_Products[,1] == paste(PF2[i],PD,sep=""))
	productName[[i]] <- MODIS_Products[ind,1]
	
if(as.character(MODIS_Products[ind,4])=="Swath"){stop(paste("You are looking for a '",as.character(MODIS_Products[ind,4]),"' product, only 'tile' data is supported yet!",sep=""))
		} else { 
			if (!quiet){
				if(i == 1){cat("\n")} else {cat("and\n")}
			cat(paste("You are looking for ", as.character(MODIS_Products[ind,1]),", the ",as.character(MODIS_Products[ind,6])," ",as.character(MODIS_Products[ind,3])," product from ",as.character(MODIS_Products[ind,2])," with a ground resolution of ",as.character(MODIS_Products[ind,5]),"\n",sep=""))
			}
		}
	} else {
	cat(paste("No product found with the name ",PF2[i],PD,sep=""))}
}
if (!quiet){cat("\n")}

invisible(list(request=product,PF1 = PF1, PF2 = PF2,PD = PD,raster_type = as.character(MODIS_Products[ind,4]),productName =  as.character(unlist(productName))))
}


