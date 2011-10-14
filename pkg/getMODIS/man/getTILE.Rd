\name{getTILE}

\docType{methods}

\alias{getTILE}
\alias{getTILE,tileH,tileV,extent}

\title{Get MODIS tile id(s)}

\description{
Get MODIS tile id(s) for a geographic area.
}

\usage{getTILE(tileH, tileV, extent)}

\arguments{

  \item{tileH}{Integer. Horizontal tile number(s) (between 0 and 35). I.e.: \code{tileH = 19:25}}

  \item{tileV}{Integer. Vertical tile number(s) (between 0 and 17). I.e.: \code{tileV = 9:12}.}

  \item{extent}{Extent Informations see details.}
}

\details{
\item{extent}{If List: \code{myExtent <- list(lat_min=mumeric, lat_max=mumeric, lon_max=mumeric, lon_min=mumeric)}. If \code{character}, the country name from \code{mapdata}. Or one of the following objects \code{'extent','rasterLayer','rasterStack'} or \code{'rasterBrick'} from \code{package(raster)}.}
}

\value{List}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getHDF}}, raster package: \code{?extent,?raster,?stack} or \code{?brick}
}

\examples{
# with tileH and tileV
getTILE(tileH=18:19,tileV=4)

# with Extent of class 'list'
Austria <- list(lat_min=46.12,lat_max=49.3,lon_min=9.2,lon_max=17.47)
getTILE(extent=Austria)

# with Extent or raster* object from raster package
require(raster)

rasterObject <- raster(xmn=9.2,xmx=17.47,ymn=46.12,ymx=49.3)
# rasterObject
getTILE(extent=rasterObject)

ext <- extent(rasterObject)
# ex
getTILE(extent=ext)

require(mapdata)
getTILE(extent="austria")

}


