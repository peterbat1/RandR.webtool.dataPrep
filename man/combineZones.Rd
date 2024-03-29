% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandR_webtool_combineZones.R
\name{combineZones}
\alias{combineZones}
\title{Combine spatial polygons into a single layer}
\usage{
combineZones(theseZones = NULL, outFilename = NULL, outputFormat = "shapefile")
}
\arguments{
\item{theseZones}{A character array with each element holding the full path a file storing a spatial polygon object}

\item{outFilename}{Character. Full path defining the file to which the combined spatial polygon object will be written}

\item{outputFormat}{Character. Specify which file format to use for the output: "shapefile" (default) or "geojson" are the only ones supported at present}
}
\description{
Combine spatial polygons into a single layer
}
