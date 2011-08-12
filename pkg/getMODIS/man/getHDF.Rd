\name{getHDF}

\docType{methods}

\alias{getHDF}
\alias{getHDF,getMODIS,function-method}

\title{
Creats and maintain a partial mirror of the NASA ftp server for MODIS grid Products subsetted by user parameters. 
}

\description{
Creats a local subset of the global MODIS-grid data online archive ("ftp://e4ftl01u.ecs.nasa.gov/").
By simple definition of subset parameters, the function checks local available data and downloads online stored data for the missing one. Feed to a schedule Job the function can manage the continuous update of local MODIS data.
}

\usage{getHDF(LocalArcPath,HdfName,Product,startdate,enddate,tileID,Collection,quiet=FALSE,wait=1,checkXML=FALSE)}

\arguments{

  \item{LocalArcPath}{Is the path to look for local and to download remote MODIS-hdf files. If left Blank the default archice "~/MODIS_ARC/" (under LINUX) is used (and if not present it is created there.}

  \item{HdfName}{Full HDF file name(s) to download. If just few single (AND known!) files should be downloaded use \code{HdfName}. If \code{HdfName} is provided the other file-related Parameters are ignored: \code{startdate, endate, tileID} and \code{Collection}. If many files have to be checked and downloaded it is simple, saver and faster to use the following parameters.}

  \item{Product}{(If \code{HdfName} isn't provided.) Which Product should be downloaded? I.e.: for MODIS TERRA Vegetation indices at 250 m ground resolution use: "MOD13Q1". For vegetation indices from both Patforms "MxD13Q1" (using an "x" instead of "O" or "Y"). Combined Products "MCD" have not been tested and are recognised as an error (for now). See \url{https://lpdaac.usgs.gov/lpdaac/products/modis_products_table} for more Products.}   
 
  \item{startdate}{(If \code{HdfName} isn't provided.) startdate "YYYY.MM.DD" is the date to start the MODIS Time Serie.}
  \item{enddate}{(If \code{HdfName} isn't provided.) enddate "YYYY.MM.DD" is the date to end the MODIS Time Serie. Can be even a future date...for update in a batch job}

  \item{tileID}{(If \code{HdfName} isn't provided.) Is the tile number, can be multiple tileIDs vector or list. i.e.: 'tileID = c("h19v05","h19v04")'.
  Refer to http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm to find your area. Some much more advanced methods will come soon.}

  \item{Collection}{(If \code{HdfName} isn't provided.) 'Collection' refers to the MODIS Product Collection-version. see: }

  \item{checkXML}{should the corresponding HDF file also been downloaded? With XML soon a kind of HDF 'checksum' will be implemented.}
    
  \item{wait}{Is a little addition from package 'audio'. After a file has been downloaded the function waits for the here defined time (default is 1 seconds). The problem is that MODIS ftp archive often refuses the connection, inserting a little 'wait' the connection is much more stable (My guess! I'll check it in a more accurate way as I am able).}

  \item{quiet}{Refers to all \code{download.file()} actions. See ?download.file. Default \code{quiet=FALSE}, set to \code{TRUE} can be really boring...}
}

\value{A CSV file with reports, and the HDF archive}

\author{Matteo Mattiuzzi}

\seealso{getXML}

\examples{

# one spezific file (no regular erpression allowed here for now!)
# getHDF(HdfName="MOD13Q1.A2009209.h19v04.005.2009227222141.hdf")

# DO NOT RUN THE NEXT EXAMPLE (unless you need this data)!!!
# getHDF(Product="MOD13Q1",startdate="2009.01.01",enddate="2015.12.31",tileID=c("h19v05","h19v04")) 

}

