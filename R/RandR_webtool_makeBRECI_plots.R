# Restore and Renew: Re-make of BRECI images for the R&R webtool data set
#
# Peter D. Wilson
# Biodiversity Analyst
# Evolutionary Ecology Research Section
# Science and Conservation Branch
# Royal Botanic Garden, Sydney
#
# 25 May 2018; Made into a function within the package RandR.webtool.dataPrep
# Improvements to this function plus incorporation of a version of the BRECI plot function in this R-package completed 2019-03-01

#' Make BRECI plots
#'
#' Make BRECI plots for a set of taxa contracting environmental suitability between current and future climates. By default, the scope of the computation is the NSW standard extent. If input rasters are larger than the standard NSW extent, they are cropped to size.
#'
#' @param theseTaxa A character array with the names of one or more taxa
#' @param currentFiles Character array. Full paths to ENM output rasters for \emph{current} climate conditions
#' @param futureFiles Character array. Full paths to ENM output rasters for \emph{future} climate conditions
#' @param outputFolder Character. Path to folder into which output PNG files will be placed
#' @param NSWscope Logical. Should the computation be made using the standard NSW extent? Default is TRUE.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{}
makeBRECIplots <- function(theseTaxa = NULL, currentFiles = NULL, futureFiles = NULL, outputFolder = NULL, NSWscope = TRUE)
{
  if (is.null(theseTaxa)) stop("makeBRECIplots: No taxa listed in parameter 'theseTaxa'")

  if (is.null(currentFiles)) stop("makeBRECIplots: Please provide a value for parameter 'currentFiles'")

  if (is.null(futureFiles)) stop("makeBRECIplots: Please provide a value for parameter 'futureFiles'")

  if ((length(theseTaxa) != length(currentFiles)) || ((length(theseTaxa) != length(futureFiles))))
    stop("makeBRECIplots: Parameters 'theseTaxa', 'currentFiles' and 'futureFiles' must be the same length")

  if (is.null(outputFolder)) stop("makeBRECIplots: Please provide a value for parameter 'outputFolder'")

cat("BRECI plots for RCP4.5 2050:\n")


#thisTaxon <- "Acacia suaveolens"
for (i in 1:length(theseTaxa))
{
  thisTaxon <- theseTaxa[i]

  cat("\t",thisTaxon,"\n")

  this_Taxon <- gsub(" ", "_", thisTaxon, fixed = TRUE)

  currRas <- raster::raster(currentFiles[i])
  if (!(raster::extent(currRas) == NSW_extent)) currRas <- raster::crop(currRas, NSW_extent)

  futureRas <- raster::raster(futureFiles[i])
  if (!(raster::extent(futureRas) == NSW_extent)) futureRas <- raster::crop(futureRas, NSW_extent)

  zz <- BRECI(currRas,
              futureRas,
              paste0(outputFolder,"/", this_Taxon, "_BRECI_Current-2050.png"),
              saveToFile = TRUE,
              plotTitle = thisTaxon,
              plotWidth = 400,
              plotHeight = 300)
}

cat("*** End of processing\n")}
