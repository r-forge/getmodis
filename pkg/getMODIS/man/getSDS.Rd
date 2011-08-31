\name{getSDS}

\docType{methods}

\alias{getSDS}

\title{List SDS in a MODIS grid file}

\description{
  This function just lists the names of the SDS of a spezified MODIS grid HDF file}

\usage{getSDS(HdfName,MRTpath="check")}

\arguments{
  \item{HdfName}{Character. File name or better filename with path (faster) to extract SDS names.}
  \item{MRTpath}{Character. Default \code{MRTpath='check'} for automatic detection. See \code{?getPATH}. But you can insert a manual path to \code{mrt/bin/}} 
} 

\value{
  Caracter. Vector with SDS names in \code{HdfName}.
}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getPATH}}
}

\examples{
# getSDS(HdfName=XXX)
}


