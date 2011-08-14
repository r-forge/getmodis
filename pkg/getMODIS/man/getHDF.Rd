\name{getHDF}

\docType{methods}

\alias{getHDF}
\alias{getHDF,LocalArcPath,HdfName,product,startdate,enddate,tileH,tileV,extent,collection,quiet,wait,checkXML}

\title{Create or update a local, user-defined subset of the global online
  MODIS grid data pool.}

\description{
  Create or update a local user-defined subset of the global MODIS grid
  data archive (\url{ftp://e4ftl01u.ecs.nasa.gov/}). Based on
  user-specific parameters the function checks in the local archive for
  available data and downloads missing data from the online MODIS data
  pool. When run in a schedule job, the function manage the continuous
  update of local MODIS data archive.
}

\usage{getHDF(LocalArcPath,HdfName,product,startdate,enddate,tileH,tileV,extent,collection,quiet=FALSE,wait=1,checkXML=FALSE)}

\arguments{

  \item{LocalArcPath}{Character. Local path to look for and to download MODIS-HDF
    files. If missing the default archive "~/MODIS_ARC/" (under LINUX)
    is used. If not present it is created in \code{LocalrAcPath}.}

  \item{HdfName}{Character (vector or list). Full HDF file name(s) to download. If
    just few single (AND known!) files should be downloaded use
    \code{HdfName}. If \code{HdfName} is provided the other file-related
    Parameters are ignored: \code{startdate,enddate,tileID} and
    \code{Collection}. If many files have to be checked and downloaded
    it is simpler, safer and faster to use following parameters.}

  \item{product}{Character. (Ignored if \code{HdfName} is provided.) MODIS grid product to be downloaded. For 'MODIS TERRA Vegetation Indices at 250 m ground resolution' use: \code{MOD13Q1}. For vegetation indices from TERRA and AQUA use \code{MxD13Q1}. Combined Products "MCD" have not been tested and are recognised as an error (for now). See \url{https://lpdaac.usgs.gov/lpdaac/products/modis_products_table} for more Products.}   
    
  \item{collection}{Character or integer. (Ignored if \code{HdfName} is provided.) Refers to the MODIS Product Collection-version. Often '005', but see MODIS pages for more information.}
 
  \item{startdate}{Character. (Ignored if \code{HdfName} is provided.) Subset start date of MODIS time series (format: "YYYY.MM.DD").}

  \item{enddate}{Character. (Ignored if \code{HdfName} is provided.) Subset end date of MODIS time series (format: "YYYY.MM.DD"). Can be a future date, for continuous update in a schedule job.}

  \item{tileH}{Character vector. (Ignored if \code{HdfName} is provided.) Horizontal tile number(s) (between 1 and 35). I.e.: \code{tileH = 1:5}. Refer to \url{http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm} to find your area.}

  \item{tileV}{Character vector. (Ignored if \code{HdfName} is provided.) Vertical tile number(s) (between 1 and 17). I.e.: \code{tileV = 9:17}. Refer to \url{http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm} to find your area.}

  \item{extent}{If List: \code{myExtent <- list(lat_min=mumeric,lat_max=mumeric,lon_max=mumeric,lon_min=mumeric)} or an \code{extent} object from \code{package(raster)}.}
  
  \item{checkXML}{Logical. Default \code{FALSE}, MODIS-XML files are not downloaded with their MODIS-HDF file. If \code{TRUE} MODIS-HDF and corresponding MODIS-XML files are checked and downloaded. (At the moment it just checks MODIS-XML availability, will soon become a 'checksum' function for local MODIS-HDF files.)}
    
  \item{wait}{Numeric. Default \code{1}. Inserts a break (in seconds) after every internal \code{download.file()} or \code{getURL()} call. It reduces the chance of FTP connection error that often occurs after many requests. Requires the 'audio' package. (Hopefully a temporary solution!)}

  \item{quiet}{Logical. Refers to all internal \code{download.file()} actions. Default \code{FALSE}, if \code{TRUE} the \code{getHDF} becomes really boring.}
}

\value{
  NONE, a CSV file with reports, and the HDF archive
}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getXML}}
}

\examples{
# one spezific file (no regular erpression allowed here for now!)
# getHDF(HdfName="MOD13Q1.A2009209.h19v04.005.2009227222141.hdf", wait=0)

# DO NOT RUN THE NEXT EXAMPLE (unless you need this data)!!!
# Get all MODIS TERRA 13Q1 beginning from 01. January 2009 up to today.
# If run in a scheduled batch job it will update the data until end of 2015.
# getHDF(product="MOD13Q1",collection=5,startdate="2009.01.01",enddate="2015.12.31",tileH=19,tileV=4:5) 

# or the same with an (list) Extent
# Austria <- list(lat_min=46.12,lat_max=49.3,lon_max=17.47,lon_min=9.2)
# getHDF(product="MYD13Q1",collection=5,startdate="2009.01.01",enddate="2015.12.31",extent=Austria)

}

