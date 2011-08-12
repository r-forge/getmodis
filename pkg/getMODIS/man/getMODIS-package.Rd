\name{getMODIS-package}
\alias{getMODIS-package}
\alias{getMODIS}
\docType{package}

\title{Functions for MODIS-GRID-data download and local storage. Plus pre-processing routines (cropping, reprojecting, mosaicing,...).}

\description{Functions for selective mirroring of the global MODIS-ftp data pool (\url{ftp://e4ftl01u.ecs.nasa.gov/}). Simple parameters are defining: MODIS-PRODUCT, COLLECTION, AREA and PERIOD. The functions are performing the following tasks: if not existing it creates a local archive structure equivalent to the online storage (or a simplified structure), checks if files are already stored locally, if not download the needed file(s).\cr

Installing some additional software it is possible to further process downloaded data using all the functions (not yet implemented!):\cr\cr
- MODIS ReprojectionTool (MRT): format conversion, mosaicing, SDS-extraction, reprojection, resampling. (\url{https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool})\cr\cr
- LDOPE: format conversion, SDS-extraction, bit-extraction,...(\url{https://lpdaac.usgs.gov/lpdaac/tools/ldope_tools})\cr\cr
Windows: coming soon
}

\details{
\tabular{ll}{
Package: \tab getMODIS\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-08-10\cr
License: \tab GPL (>=3)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Matteo Mattiuzzi; Institute of Surveying, Remote Sensing and Land Information (IVFL), BOKU Vienna 

Maintainer: Matteo Mattiuzzi \email{matteo.mattiuzzi@boku.ac.at}
}

\references{
TODO, MODIS stuff
}
\keyword{MODIS, time series}

