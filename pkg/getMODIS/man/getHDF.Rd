\name{getHDF}

\docType{methods}

\alias{getHDF}
\alias{getHDF,getMODIS,function-method}

\title{Create and update a local user-defined subset of the global online MODIS grid data pool.}

\description{Create and update a local user-defined subset of the global MODIS grid data online archive (\url{ftp://e4ftl01u.ecs.nasa.gov/}).
The function checks a local archive for available data and downloads missing data from the online MODIS data pool based on user-specific parameters. When fed to a schedule job, the function can manage the continuous update of local MODIS data archive.}

\usage{getHDF(LocalArcPath,HdfName,Product,startdate,enddate,tileID,Collection,quiet=FALSE,wait=1,checkXML=FALSE)}

\arguments{

  \item{LocalArcPath}{Character. Local path to look for and to download MODIS-HDF files. If missing the default archive "~/MODIS_ARC/" (under LINUX) is used. If not present it is created in \code{LocalArcPath}.}

  \item{HdfName}{Character vector or list. Full HDF file name(s) to download. If just few single (AND known!) files should be downloaded use \code{HdfName}. If \code{HdfName} is provided the other file-related Parameters are ignored: \code{startdate,enddate,tileID} and \code{Collection}. If many files have to be checked and downloaded it is simpler, safer and faster to use following parameters.}

  \item{Product}{(If \code{HdfName} isn't provided.) Character. Product to be downloaded. For 'MODIS TERRA Vegetation Indices at 250 m ground resolution' use: \code{MOD13Q1}. For vegetation indices from TERRA and AQUA use \code{MxD13Q1}. Combined Products "MCD" have not been tested and are recognised as an error (for now). See \url{https://lpdaac.usgs.gov/lpdaac/products/modis_products_table} for more Products.}   
 
  \item{startdate}{(If \code{HdfName} isn't provided.) Character. Subset start date of MODIS time series (format: "YYYY.MM.DD").}
  \item{enddate}{(If \code{HdfName} isn't provided.) Character. Subset end date of MODIS time series (format: "YYYY.MM.DD"). Can be a future date, for continuous update in a schedule job.}

  \item{tileID}{(If \code{HdfName} isn't provided.) Character vector or list. Is the tile number. I.e.: \code{tileID = c("h19v05","h19v04")}.
  Refer to \url{http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm} to find your area. Some more advanced methods will come soon.}

  \item{Collection}{(If \code{HdfName} isn't provided.) Character or integer. Refers to the MODIS Product Collection-version. Usually '005', but see MODIS pages for more information.}

  \item{checkXML}{Logical. Default \code{FALSE}, MODIS-XML files are not downloaded with their MODIS-HDF file. If \code{TRUE} MODIS-HDF and corresponding MODIS-XML files are checked and downloaded. (At the moment it just checks MODIS-XML availability, will soon become a 'checksum' function for local MODIS-HDF files.)}
    
  \item{wait}{Numeric. Default \code{1}. Inserts a break (in seconds) after every internal \code{download.file()} or \code{getURL()} call. It reduces the chance of FTP connection error that often occurs after many requests. Requires the 'audio' package. (Hopefully a temporary solution!)}

  \item{quiet}{Logical. Refers to all internal \code{download.file()} actions. Default \code{FALSE}, if \code{TRUE} the \code{getHDF} becomes really boring.}
}

\value{NONE, a CSV file with reports, and the HDF archive}

\author{Matteo Mattiuzzi}

\seealso{getXML}

\examples{

# one spezific file (no regular erpression allowed here for now!)
# getHDF(HdfName="MOD13Q1.A2009209.h19v04.005.2009227222141.hdf", wait=0)

# DO NOT RUN THE NEXT EXAMPLE (unless you need this data)!!!
# Get all MODIS TERRA 13Q1 beginning from 01. January 2009 up to today.\cr
# If fed to a scheduled batch job it will update the data until end of 2015.
# getHDF(Product="MOD13Q1",startdate="2009.01.01",enddate="2015.12.31",\cr
# tileID=c("h19v05","h19v04")) 

}

