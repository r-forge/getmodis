\name{getTILE}

\docType{methods}

\alias{getTILE}
\alias{getTILE,tileH,tileV,extent}

\title{Get MODIS tile id(s)}

\description{
Get MODIS tile id(s). 
See: \url{http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm} to find your area.
Some more advanced methods coming soon.
}

\usage{getTILE(tileH,tileV,extent)}

\arguments{

  \item{tileH}{Integer. Horizontal tile number(s) (between 1 and 35). I.e.: \code{1ileH = 19:25}}

  \item{tileV}{Integer. Vertical tile number(s) (between 1 and 17). I.e.: \code{tileV = 9:12}.}

  \item{extent}{If List: \code{myExtent <- list(lat_min=mumeric,lat_max=mumeric,lon_max=mumeric,lon_min=mumeric)}
  or \code{extent} from \code{package(raster)}.}
}

\value{Character. Tile id(s) formatted for MODIS names (I.e.: "h09v12") }

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getHDF}} # add link to pkg raster
}

\examples{
# with tileH and tileV
# getTILE(tileH=19:22,tileV=5)

# with Extent (list)
# Austria <- list(lat_min=46.12,lat_max=49.3,lon_max=17.47,lon_min=9.2)
# getTILE(extent=Austria) # h18v04 and h19v04

# with Extent (from package raster)
# todo
}

