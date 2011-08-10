\name{getMODIS-package}
\alias{getMODIS-package}
\alias{getMODIS}
\docType{package}

\title{keaping a used-defined MODIS ftp "mirror" and pre-processing of MODIS grid data}

\description{
Description: User can define which MODIS data to download. Which in the sense of MODIS-PRODUCT and VERSION, AREA, PERIOD. The functions here are doing the rest by creating a userdefined subset of the online MODIS-grid archive (ftp://e4ftl01u.ecs.nasa.gov/).

Installing some additional software it is possible to further process downloaded data with all their capabilities
 - MODIS ReprojectionTool (MRT): https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool
      (format conversion, mosaicing, SDS-extraction, reprojection, resampling)
 - LDOPE: https://lpdaac.usgs.gov/lpdaac/tools/ldope_tools
      (format conversion, SDS extraction, bit-extraction,...)
only Windows:
 - dos2unix ()
}
\details{
\tabular{ll}{
Package: \tab getMODIS\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-08-10\cr
License: \tab GPL (>=3)\cr
LazyLoad: \tab yes\cr
}
\
}
\author{
Matteo Mattiuzzi

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
getXML()
}
