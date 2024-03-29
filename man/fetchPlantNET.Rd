% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandR_webtool_fetchPlantNET.R
\name{fetchPlantNET}
\alias{fetchPlantNET}
\title{Fetch PlantNET information for a taxon}
\usage{
fetchPlantNET(thisTaxon, synonyms = "", verbose = FALSE)
}
\arguments{
\item{thisTaxon}{Character string representign the name of the taxon to be fetched}

\item{verbose}{Logical. Should progress messages be written to the console? Default is FALSE}
}
\value{
{A named list with the following elements:
\item{nswName}{The taxonomic name used in NSW as reported by PlantNET}
\item{plantNETurl}{Ther recovered URL to the taxon entry on PlantNET}
\item{plantNETfullName}{Full taxonomic name (taxon plus author inforamtion) reproted by PlantNET}
\item{plantNETcommonNames}{A multivalued field of any common names supplied by PlantNET with elements separated by ';'.}
\item{plantNETfamily}{The name of the family used by PlantNET for this taxon concept}
\item{plantNETsubfamily}{The name of the sub-family (if present) used by PlantNET for this taxon concept}
}
}
\description{
Search for a matching taxon on PlantNET and, if found, return a named list containing useful information extracted from the return web page. No resolvable name returns "No_data" list elements.
}
\examples{
\dontrun{}
}
