\name{getMODIS-package}
\alias{getMODIS-package}
\alias{getMODIS}
\docType{package}

\title{Functions for MODIS data download and local storage. Pre-processing routines (cropping, reprojecting, mosaicing,...).}

\description{
Description: Functions for partial mirroring of the MODIS-ftp data storage (\url{ftp://e4ftl01u.ecs.nasa.gov/}).
With simple parameters (or Parameterfile) one can select: MODIS-PRODUCT, COLLECTION, AREA, PERIOD to download and store locally. The functions here are doing the rest: Check if file(s) is already stored locally, if not download the needed file(s), creating a archive structure like the online storage or a simplefied one.\cr

Installing some additional software it is possible to further process downloaded data usin all the functions (not yet implemented!):\cr\cr
- MODIS ReprojectionTool (MRT): format conversion, mosaicing, SDS-extraction, reprojection, resampling. (\url{https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool})\cr\cr
- LDOPE: format conversion, SDS-extraction, bit-extraction,...(\url{https://lpdaac.usgs.gov/lpdaac/tools/ldope_tools})\cr\cr
(only for Windows: - dos2unix (), not ready!)
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
Matteo Mattiuzzi; Institute of Surveying, Remote Sensing and Land Information (IVFL), Vienna 

Maintainer: Matteo Mattiuzzi \email{matteo@mattiuzzi.com}
}

\references{
not yet
}

\keyword{ package }
\seealso{
not yet
}
\examples{
# getHDF()
# getXML() 
}

