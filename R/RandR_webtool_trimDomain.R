# R&R webtool: Prepare trimmed domain GIS layer for taxa with special zones
#
# This function requires that the special zone layer has been prepared by the function makeSpecialZonelayer().
#
# Peter D. Wilson
# Biodiversity Analyst
# Evolutionary Ecology Section
# Science and Conservation Branch
# The Royal Botanic Garden, Sydney
#
# 2018-12-04

#' Trim GDM model domain
#'
#' Trim a GDM model domain polygon with a special zone polygon layer
#'
#' @param thisDomain Character. Path to a geoJSON-format spatial polygon defining the confidence bounds od "domain" for an R&R GDM
#' @param thisZone Character. Path to an ESRI shapefile holding a polygon layer representing "special" zones to be excluded from the confidence bounds of the GDM pointed to by thisDomain
#' @param makePlot Logical. Make a plot of trimmed domain file on the standard plot device. Default is FALSE.
#'
#' @details { This function is designed to excise one or more polygons from the GDM bounds polygon pointed to by \emph{thisDomain}. These polygons represent areas for which it is inappropriate to produce an output from the webtool, and could include:
#'
#' \itemize{
#' \item The range of a threatened sub-taxon;
#' \item One or more regions of strongly differentiated populations not formally described but suggesting the need to sample carefully; or,
#' \item Areas of known or inferred ploidy.
#' }
#'
#' The intention is that if a user clicks inside one of the special regions, then an explanatory or warning popup or other message medium will be displayed.
#'
#' It is assumed that if there are multiple polygons to be excised, the polygon set has been merge into one multi-polygon shapefile using the companion function \link{combineZones} or in a GIS such as ArcMap or QGIS.
#' }
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{}
trim_GDM_domain <- function(thisDomainFile = NULL, specialZoneFile = NULL, makePlot = FALSE)
{
  cat("Trim an R&R GDM model domain polygon using special zone information:\n")

  if (file.exists(specialZoneFile))
  {
    fileType <- toupper(tools::file_ext(specialZoneFile))
    if (fileType == "GEOJSON")
    thisZone <- geojsonio::geojson_read(specialZoneFile, what = "sp")
    else
      thisZone <- rgdal::readOGR(specialZoneFile)
  }
  else
    stop("RandR_webtool_trimDomain: Cannot find specialZoneFile.")

  thisDomain <- geojson_read(thisDomainFile, what = "sp")

  cat("done.\nMake trimed domain with 1 special zone...")
  #### One special zone
  trimmedDomain <- gDifference(thisDomain, thisZone1)

  if (makePlot) plot(trimmedDomain, main = "Trimmed domain polygo", col = "plum")

  # Save trimmed domain as shapefile
  shapeName <- gsub(".geojson", "_trimmed.shp", thisDomainFile, fixed = TRUE)

  p.df <- data.frame(ID = 1:length(trimmedDomain))
  rgdal::writeOGR(SpatialPolygonsDataFrame(trimmedDomain, p.df),
           shapeName,
           layer = gsub(".shp","",basename(shapeName)),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)

  # Make a geojson version
  geojsonio::file_to_geojson(shapeName,
                  method = "local",
                  output = gsub(".shp", "", shapeName, fixed = TRUE),
                  parse = FALSE, encoding = "CP1250", verbose = FALSE)

  c("done.\n\n**** End of processing.\n")
}
