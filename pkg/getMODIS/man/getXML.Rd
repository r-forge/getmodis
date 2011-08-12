\name{getXML}

\docType{methods}

\alias{getXML}
\alias{getXML,getMODIS,function-method}

\title{Mirroring corresponding xml-metadata-files for any MODIS-grid-hdf files in the query.}

\description{
Each MODIS HDF file has a xml-metafile. This data contains usefull information about each MODIS-hdf file. Within that package, the xml-files are used to check the size of the downloaded hdf files, and correcting the so often broken downloads from the MODIS online archive. getXML checks localy for the presense of spezified xml files and if not available it retrieves them from the MODIS ftp server.}

\usage{getXML(LocalArcPath="",HdfName="", wait=1, comments=FALSE)}

\arguments{

  \item{LocalArcPath}{Is the path to look for MODIS-hdf files. If left Blank, the standard argument \code{list.files(path=".")} is used. If 'HdfName' is not set, all underlaying MODIS-hdf files are checked for the xml presence and aquired if not available locally.}

  \item{HdfName}{Is a Filename or a pattern (regular expression) to check for xml file presens. If \code{HdfName} is not set all found MODIS-hdf files are gathered.}

  \item{wait}{is a little addition from package 'audio'. After a xml file has been downloaded (with \code{download.file()}) the function waits for the here defined time (default is 1 second. Set to 0 and library 'audio' will not be loaded!). The problem is that MODIS ftp archive often refuses the connection, inserting a little 'wait' the connection is much more stable. Recomanded values 1 to 3 secounds, you can set it in proportion with the amount of files. More Files higer value. (I'll try to solve the sproblem in a more elegant way soon)}

  \item{comments}{Should getXML report the downloaded file?}
}

\value{none; look in the directory where the hdf files are stored to check for new xml files}

\author{Matteo Mattiuzzi}

\seealso{getHDF}

\examples{

# getXML() # checks all HDF files under path="." for the xml-file

# getXML(HdfName="MOD13Q1*") # check all MODIS Terra Vegetation indices 250 m (13Q1) under "."

# getXML(HdfName="MYD09GA.A2011002.h19v05.005.2011007211429.hdf") # checks the xml availability of the given file

}

