

#' Combine spatial polygons into a single layer
#'
#' @param theseZones A character array with each element holding the full path a file storing a spatial polygon object
#' @param outFilename Character. Full path defining the file to which the combined spatial polygon object will be written
#' @param outputFormat Character. Specify which file format to use for the output: "shapefile" (default) or "geojson" are the only ones supported at present
#'
#' @return
#' @export
#'
#' @examples
combineZones <- function(theseZones = NULL, outFilename = NULL, outputFormat = "shapefile")
{
  if (is.null(theseZones)) stop("combineZones: theseZones must be given a value")
  if (is.null(outFilename)) stop("combineZones: outFilename cannot be NULL")
  if (!(outputFormat %in% c("shapefile", "geojson"))) stop("combineZones: outputFormat must been one of 'shapefile' or 'geojson'")

  if (!all(file.exists(theseZones))) stop("combineZones: At least one polygon object listed in 'theseZones' cannot be found")

  combo <- rgdal::readOGR(theseZones[1])

  if (length(theseZones) > 1)
  {
    for (i in 2:length(theseZones))
      combo <- raster::bind(combo, rgdal::readOGR(theseZones[i]))
  }


  thisLayer <- gsub(tools::file_ext(basename(outFilename)), "", basename(outFilename))

  rgdal::writeOGR(combo, outFilename, layer = thisLayer, driver = ifelse(outputFormat == "shapefile", "ESRI Shapefile", "GeoJSON"), overwrite_layer = TRUE)
}
