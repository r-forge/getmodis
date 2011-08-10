library(RCurl)

source('/home/arc/WualaDrive/MatteoMattiuzzi/ARC-Server/getMODIS/getXML.R') # provisoric! sources getXML function


getHDF <- function(archive=".",file,platform,version,product,startdate,enddate,date,getXML=F) {

if (!missing(file)) {  }

if (missing(platform)) {platform = "both"}
if (missing(version)) {version = "005"} else{version <- sprintf("%03d",as.numeric(version))}
#if (missing(product)) {stop("'Can't' download all Products, please specify one (i.e. '13q1')")}




### create FTP-base strings
PF1  <- if(platform == "both"){c("MOLT", "MOLA")}else{platform}
PF2  <- if(platform == "both"){c("MOD", "MYD")}else{if(platform == "MOLT"){"MOD"}else{if(platform=="MOLA"){"MYD"}}}
ftps <- rep(NA,length(PF1))
for (u in 1:length(PF1)){
ftps[u] <- paste("ftp://e4ftl01u.ecs.nasa.gov/", PF1[u],"/", PF2[u],Producttype,".",Version,"/",sep="")
}
####


} # end getHDF

