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

  \item{extent}{XXX.XXX.}
}

\value{Character. Tile id(s) formatted for MODIS names (I.e.: "h09v12") }

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link[getMODIS]{getHDF}}
}

\examples{
# getTILE(tileH=19:22,tileV=5)

# not implemented yet:
# getTILE(extent=XXX)

}

