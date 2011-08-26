\name{orgSTRUC}

\docType{methods}

\alias{orgSTRUC}

\title{Change structure of local Archive}

\description{This function changes the storage structure (see details). Depending on the amount of files stored and how files are accessd the folder structure can be changed. !Use this function with care! }

\usage{orgSTRUC(LocalArcPath,HdfName,to,move=TRUE)}

\arguments{
  \item{LocalArcPath}{Character. Local path to look for MODIS files. If missing the default archive '~/MODIS_ARC/' (LINUX), 'C:/Documents and Settings/username/My Documents/MODIS_ARC/' (Windows XP) and 'C:/Users/username/Documents/MODIS_ARC' (Vista/Windows 7) is used.}

 \item{HdfName}{Character. Regular expression (passed to \code{?list.files}) for file matching. If missing, all local MODIS grid data under \code{LocalArcPath} is considered.}

 \item{to}{Integer between \code{1} and \code{3}}

 \item{move}{Logical. Default is \code{TRUE}, this means that files are moved and multiple files are deleted. It \code{FALSE},files are just copied and remain in origin folder!}
}

\details{
\tabular{rll}{
  \tab If \code{to} is \code{1} than: \code{/LocalArcPath/product.collection/hdf-} and \code{xml-}files\cr
  \tab If \code{to} is \code{2} than: \code{/LocalArcPath/product.collection/year/hdf-} and \code{xml-}files\cr
  \tab If \code{to} is \code{3} than: \code{/LocalArcPath/product.collection/date/hdf-} and \code{xml-}files\cr

}
}
\value{
  NONE, (or just an information about moved/copied files)
}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getXML},\link{getHDF}}
}

\examples{
# MOVE all MODIS grid data to a MODIS data pool like structure
# orgSTRUC(to=3,move=TRUE)

# COPY all MOD13Q1 to folder "/LocalArcPath/MOD13Q1.collection/"
# orgSTRUC(HdfName="MOD13Q1",to=1,move=FALSE)
}


