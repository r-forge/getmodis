\name{getMODIS-package}

\alias{getMODIS-package}

\docType{package}

\title{Functions for selective MODIS-GRID-data download or update. And pre-processing routines (cropping, reprojecting, mosaicing).}

\description{
Functions for selective mirroring of the global MODIS data pool (\url{ftp://e4ftl01u.ecs.nasa.gov/}). Simple parameters are defining: modis-product, collection, area/tile and period. The functions are performing the following tasks: if not existing it creates a local archive structure equivalent to the online storage (or a simplified structure), checks if files are already stored locally, if not download the needed file(s).

Installing additional software it is possible to further process downloaded data using all the functions (not yet implemented!):

- MODIS ReprojectionTool (MRT): format conversion, mosaicing, SDS-extraction, reprojection, resampling. (\url{https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool})

- LDOPE: format conversion, SDS-extraction, bit-extraction,... (\url{https://lpdaac.usgs.gov/lpdaac/tools/ldope_tools})

Windows: not tested yet.
}

\details{
\tabular{ll}{
Package: \tab getMODIS\cr
Type: \tab Package\cr
Version: \tab 0.1-10\cr
Date: \tab 2011-08-10\cr
License: \tab GPL (>=3)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Matteo Mattiuzzi; Institute of Surveying, Remote Sensing and Land Information (IVFL), BOKU Vienna\cr 
Anja Klisch; Institute of Surveying, Remote Sensing and Land Information (IVFL), BOKU Vienna 

Maintainer: Matteo Mattiuzzi \email{matteo.mattiuzzi@boku.ac.at}
}

\references{
MODIS data pool home page with list of data access possibilities: \url{https://lpdaac.usgs.gov/lpdaac/get_data/data_pool}\cr
MODIS data pool server: \url{ftp://e4ftl01u.ecs.nasa.gov/}\cr
MODIS home page: \url{http://modis.gsfc.nasa.gov/}\cr
}

\keyword{package}
\keyword{MODIS, time series}

