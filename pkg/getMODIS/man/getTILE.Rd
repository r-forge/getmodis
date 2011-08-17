\name{getTILE}

\docType{methods}

\alias{getTILE}
\alias{getTILE,tileH,tileV,extent}

\title{Get MODIS tile id(s)}

\description{
Get MODIS tile id(s). 
See: \url{http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm} to find your area.
}

\usage{getTILE(tileH, tileV, extent)}

\arguments{

  \item{tileH}{Integer. Horizontal tile number(s) (between 0 and 35). I.e.: \code{tileH = 19:25}}

  \item{tileV}{Integer. Vertical tile number(s) (between 0 and 17). I.e.: \code{tileV = 9:12}.}

  \item{extent}{If List: \code{myExtent <- list(lat_min=mumeric, lat_max=mumeric, lon_max=mumeric, lon_min=mumeric)}
  or one of the following objects \code{'extent','rasterLayer','rasterStack'} or \code{'rasterBrick'} from \code{package(raster)}.}
}

\value{List with 3 slots. With tile id(s) formatted for MODIS names (I.e.: "h09v12") }

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
getTILE(extent=Austria) # h18v04 and h19v04

# with Extent or raster* object from raster package
require(raster)

rasterObject <- raster(xmn=9.2,xmx=17.47,ymn=46.12,ymx=49.3)
# rasterObject
getTILE(extent=rasterObject)

ext <- extent(rasterObject)
# ex
getTILE(extent=ext)
}


