% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{sample_paths}
\alias{sample_paths}
\title{Paths of the rasters used in documentation examples}
\format{
Each sample archive include 60 images acquired between 2020-01-01 and
2020-09-30 covering the \code{\link{sampleroi}} extent, provided in the
\href{https://sen2r.ranghetti.info/articles/outstructure.html}{sen2r output format}.
NDVI images are saved in integer format (values are scaled using a 10^4
factor).

Data size is very small (13x4 pixels per each image) in order not to be heavy
in terms of data size and to ensure examples to be rapidly runnable.
The vignette \href{../articles/workflow.html}{"The sen2rts workflow"} makes use
of a larger dataset in order to show a more plausible use case.
}
\usage{
sample_paths(prod = "NDVI")
}
\arguments{
\item{prod}{\code{"NDVI"} or \code{"SCL"}.}
}
\value{
\code{sample_paths()} returns the paths of the sample archive
(NDVI or SCL, depending on argument \code{prod}).
}
\description{
The sample dataset contains Sentinel-2 derived images
(NDVI index and SCL) used in the documentation examples of \code{\link[=extract_s2ts]{extract_s2ts()}}.
This archive is not provided as a raster stack using standard \code{data()} way
because \code{\link[=extract_s2ts]{extract_s2ts()}} accepts paths as input.
}
\examples{
sen2r_ndvi_paths <- sample_paths("NDVI")
head(sen2r_ndvi_paths)
sen2r_scl_paths <- sample_paths("SCL")
head(sen2r_scl_paths)
}
