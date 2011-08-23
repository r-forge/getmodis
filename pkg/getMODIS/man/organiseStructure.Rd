\name{organiseStructure}

\docType{methods}

\alias{organizeStructure}

\title{Change structure of local Archive}

\description{!Use this function with much care! Depending on the amount of files stored and how files are accessd the folder structure is different. This function changes the storage structure.}

\usage{(LocalArcPath,HdfName,to,remove=TRUE)}

\arguments{
  \item{LocalArcPath}{Character. Local path to look for and to download MODIS files. If missing the default archive "~/MODIS_ARC/" (under LINUX) is used.}

 \item{HdfName}{Character. Regular expression for file matching. If missing, all local MODIS grid data under \code{LocalArcPath} is considered.}

 \item{to}{Integer. Integer number between 1 and 3}

 \item{remove}{Logical. Default is \code{TRUE}, this means that files after copied are deleted (what basically 'move.file' sould do! But I did not found that function!). It \code{FALSE},files are copied and not moved!}
}

\details{
\tabular{rll}{
  \tab \code{to}  \tab If \code{1} than: \code{/LocalArcPath/product.collection/hdf-} and \code{xml-}files\cr
  \tab \code{to}  \tab If \code{2} than: \code{/LocalArcPath/product.collection/year/hdf-} and \code{xml-}files\cr
  \tab \code{to}  \tab If \code{3} than: \code{/LocalArcPath/product.collection/date/hdf-} and \code{xml-}files\cr

}
}
\value{
  NONE
}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getXML},\link{getHDF}}
}

\examples{
# MOVE all MODIS grid data to a MODIS data pool like structure
# organiseStructure(to=3,remove=TRUE)

# COPY all MOD13Q1 to folder "/LocalArcPath/MOD13Q1.collection/"
# organiseStructure(HdfName=MOD13Q1",to=1,remove=FALSE)
}


