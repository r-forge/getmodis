\name{getHDF}
\alias{getHDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creats and maintain a partial mirror the NASA ftp server for MODIS grid Products subsetted by user parameters. 
}
\description{
Each MODIS HDF file has a xml-metafile. This data contains usefull information about each MODIS-hdf file. Within that package, the xml-files are used to check the size of the downloaded hdf files, and correcting the so often broken downloads from the MODIS online archive. getXML checks localy for the presense of spezified xml files and if not available it retrieves them from the MODIS ftp server.}

\usage{getHDF <- function(LocalArcPath,HdfName,Product,startdate,enddate,tileID,quiet=TRUE,wait=1,checkXML=FALSE,...)}

\arguments{

  \item{LocalPathToHdf}{Is the path to look for local and to download remote MODIS-hdf files. If left Blank the default archice "~/MODIS_ARC/" (under LINUX) is used (and if not present it is created there.}

  \item{Product}{Which Product should be downloaded? I.e.: for MODIS TERRA Vegetation indices at 250 m ground resolution use: "MOD13Q1". For vegetation indices from both Patforms "MxD13Q1" (using an "x" ("X") instead of "O" or "Y"). Combined Products "MCD" have not been tested and are recognised as an error (for now).
  Refer to https://lpdaac.usgs.gov/lpdaac/products/modis_products_table for more Products.} 
  
  \item{HdfName}{If just few single (AND known!) Files should be downloaded (If HdfName is provided the rest is ignored (startdate, endate, tileID and Product)}
  
  \item{startdate}{startdate "YYYY.MM.DD" is the date to start the MODIS Time Serie.}
  \item{enddate}{enddate "YYYY.MM.DD" is the date to end the MODIS Time Serie. Can be even a future date...for update in a batch job}

  \item{tileID}{Is the tile number, can be multiple tileIDs vector or list. i.e.: 'tileID = c("h19v05","h19v04")'.
  Refer to http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm to find your area. Some much more advanced methods will come soon.}

  \item{checkXML}{should the corresponding HDF file also benn downloaded? With XML soon a kind of HDF 'checksum' will be implemented.}
    
  \item{wait}{Is a little addition from package 'audio'. After a file has been downloaded the function waits for the here defined time (default is 1 seconds). The problem is that MODIS ftp archive often refuses the connection, inserting a little 'wait' the connection is much more stable (My guess! I'll check it in a more professional way as I have some time).}

  \item{quiet}{Refert to all download.file() actions. See ?download.file}
}

\value{A CSV file with reports, and the HDF archive}

\author{Matteo Mattiuzzi}

\seealso{getXML}

\examples{

# getHDF(HdfName="MOD13Q1.A2009209.h19v04.005.2009227222141.hdf") # one spezific file (no regular erpression allowed here for now!)

# DO NOT RUN THE NEXT EXAMPLE (unless you need the data ;))!!!
# getHDF(Product="MOD13Q1",startdate="2009.01.01", enddate="2015.12.31",tileID=c("h19v05","h19v04")) 

}

