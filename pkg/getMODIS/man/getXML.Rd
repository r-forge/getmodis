\name{getXML}
\alias{getXML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mirroing corresponding xml-metadata-files for any MODIS-hdf files in the query. 
}
\description{Each MODIS HDF file has a xml-metafile. This data contains usefull information about each MODIS-hdf file. Within that package, the xml-files are used to check the size of the downloaded hdf files, and correcting the so often broken downloads from the MODIS online archive. getXML checks localy for the presense of spezified xml files and if not available it retrieves them from the MODIS ftp server.}

\usage{getXML(LocalPathToHdf = "", HdfName = "", wait = 1,comments=FALSE)}

\arguments{

  \item{LocalPathToHdf}{Is the path to look for MODIS-hdf files. If left Blank, the standard argument 'list.files(path=".")' is used. If 'HdfName' is not set, all underlaying MODIS-hdf files are checked for the xml presence and aquired if not available locally.}

  \item{HdfName}{Is a Filename or a pattern (regular expression) to check for xml file presens. If 'HdfName' is not set all found MODIS-hdf files are gathered.}

  \item{wait}{is a little addition from package 'audio'. after a xml file has been downloaded the function waits for the here defined time (default is 2 seconds). The problem is that MODIS ftp archive often refuses the connection, inserting a little 'wait' the connection is much more stable.}

  \item{comments}{Should function report the downloaded file?}
}

\value{none; look in the directory where the hdf files are stored to check for new xml files}

\author{Matteo Mattiuzzi}

\seealso{getHDF}

\examples{

getXML() # checks all HDF files under path="." for the xml-file

getXML(HdfName="MOD13Q1*") # check all MODIS Terra Vegetation indices 250 m (13Q1) under "."

getXML(HdfName="MYD09GA.A2011002.h19v05.005.2011007211429.hdf") # checks the xml availability of the given file

}

