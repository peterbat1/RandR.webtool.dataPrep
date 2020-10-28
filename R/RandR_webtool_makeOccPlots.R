
#library(geojson)
#library(geojsonio)
#library(leaflet)
#library(sp)
#library(mapview)


#' Make occurrence plot as a PNG file
#'
#' Produces a PNG image of occurrence records for a selected taxon suitable for use in the R&R webtool
#'
#' @param taxonNames Character array. One or more taxon names to be processed
#' @param extentStr Character. A flag indicating the bounds to which the map will be cropped. Must be one of "auto", "NSW" or "Aust" - see \emph{Details} for more information
#' @param baseDataPath Character. Path to the folder below which folders for each taxon will be found
#' @param useTaxonSubfolders Logical. Are files in sub-folders using taxonNames entries? Default is FALSE.
#' @param outputPath Character. Path to the folder into which output files will be written using a filename convention described in \emph{Details}
#'
#' @details { The function has been designed to work with a fixed structure and filename convention for input data. It is assumed that within the folder defined by the paramater \emph{baseDataPath}, there is a sub-folder named for each taxon listed in the parameter \emph{taxonNames}. Then, within each taxon sub-folder there is a file with a name structured as "genus_specificEpithet_herbariumRecords_filtered.csv" which has been produced by the functions \emph{fetchALA} and \emph{filterALA} in the package \pkg{RandR.data}.
#'
#' Output files are placed in the folder defined in paramater \emph{outputPath} with a name like "genus_specificEpithet_distibution.png". For example, "Neolitsea_dealbata_distribution.png".
#'
#' The spatial bounds of the output map are set by the parameter \emph{extentStr} which may have one of three values:
#' \itemize{
#'   \item \strong{auto} Bounds set to bounding box of the occurrence records with a 0.1 degree buffer
#'   \item \strong{NSW}  Bounds set to encompass NSW
#'   \item \strong{Aust} Bounds set to encompass Australia
#' }
#' }
#' @return NULL
#' @export
#'
#'
#' @examples
#' \dontrun{}
#'
makeOccPlot <- function(taxonNames = NULL,
                        extentStr = c("auto", "NSW", "Aust"),
                        baseDataPath = NULL,
                        useTaxonSubfolders = FALSE,
                        outputPath = NULL)
{
  if (is.null(taxonNames)) stop("makeOccPlot: No taxon names supplied.")
  if (!(extentStr %in% c("auto", "NSW", "Aust"))) stop("makOccPlot: Invalid value for extentStr.")

  if (is.null(baseDataPath)) stop("makeOccPlot: no value supplied for dataPath.")
  if (!dir.exists(baseDataPath)) stop("makeOccPlot: dataPath does not exist.")
  if (!dir.exists(outputPath)) stop("makeOccPlot: outPath does not exist.")

  if (is.null(outputPath)) stop("makeOccPlot: no value supplied for outPath.")

  cat("R&R webtool maintenance: Make occurrence plots (v0.2 2018-12-05)\n============================================================================\nMaking plots for", length(taxonNames),"taxa.\n")


  for (thisTaxon in taxonNames)
  {
    cat("    Processing:", thisTaxon, " ")

    this_Taxon <- sub(" ", "_", thisTaxon, fixed = TRUE)

    if (useTaxonSubfolders)
      dataFileName <- paste0(baseDataPath,"/", thisTaxon, "/", this_Taxon, "_herbariumRecords_filtered.csv")
    else
      dataFileName <- paste0(baseDataPath,"/", this_Taxon, "_herbariumRecords_filtered.csv")

    if (!file.exists(dataFileName))
      stop("makeOccPlot: Missing data file. Please check value passed in parameter 'baseDataPath' and that data exists for this taxon.")
    else
      cat("Loading occurrence data...")

    occ <- read.csv(dataFileName, stringsAsFactors = FALSE)

    # Patch colnames for latitude and longitude to account for variations in terms
    latColInd <- grep("LATITUDE", toupper(colnames(occ)))
    longColInd <- grep("LONGITUDE", toupper(colnames(occ)))

    if ((length(latColInd) == 0) || (length(longColInd) == 0))
      stop("Cannot identify latitude/longitude columns in source data.")
    else
    {
      colnames(occ)[c(latColInd, longColInd)] <- c("Latitude", "Longitude")
    }

    if (extentStr == "NSW")
    {
      minLong <- 141.5
      maxLong <- 153.0
      minLat <- -37.25
      maxLat <- -27
      cat("Map bounds set to NSW...")
    }
    else
    {
      if (extentStr == "Aust")
      {
        minLong <- 112.9
        maxLong <- 154
        minLat <- -44
        maxLat <- -9
        cat("Map bounds set to Australia...")
      }
      else # fall through to here without further checking...which should have been done above
      {
        minLong <- min(occ$Longitude) - 0.1
        maxLong <- max(occ$Longitude) + 0.1
        minLat <- min(occ$Latitude) - 0.1
        maxLat <- max(occ$Latitude) + 0.1
        cat("Map bounds set to bounding box for records...")
      }
    }

    cat("Making and saving map...")
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
      leaflet::fitBounds(lng1 = minLong, lat1 = minLat, lng2 = maxLong, lat2 = maxLat) %>%
      leaflet::addCircleMarkers(lng = occ$Longitude, lat = occ$Latitude, radius = 1, color = "#9030FF", fillOpacity = 0.8)

    if (extentStr == "")
      outFilename <- paste0(outputPath, "/", this_Taxon, "_distribution.png")
    else
      outFilename <- paste0(outputPath, "/", this_Taxon, "_distribution_",extentStr,".png")

    mapview::mapshot(m, file = outFilename)

    cat("Completed OK.\n")
  }
}
