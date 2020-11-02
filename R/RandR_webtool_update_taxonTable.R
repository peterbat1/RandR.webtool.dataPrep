# R&R Webtool Data Preparation: Make updated webtool taxon table
#
#
# Peter D. Wilson
#
# 2018-12-02

#' Update the webtool taxon table
#'
#' Make a new version of the R&R webtool taxon table from the latest R&R database table \emph{taxonTable}.
#'
#' @param outputPath Character. Path to a folder into which the new version of \emph{taxonTable.csv} will be written. The folder is created if it does not exist.
#'
#' @return NULL
#' @export
#'
#' @details { The webtool taxon table referenced in \emph{taxonTablePath} is updated to included any taxa listed in \emph{newTaxa}. The functions in R-package \link{processALA} are used to provide:
#' \itemize{
#' \item Canonical accepted taxon names derived from the National Species List (NSL) hosted by the Atlas of Living Australia (ALA)
#' \item Synonyms for accepted names to allow users some flexibility in identifying the taxon of interest
#' \item Additional data for display in the webtool pages such as formatted full species name, the family to which each taxon belongs (according to the NSL), URLs to PlantNET and ALA pages for the taxon
#'}
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Update existing taxa:
#' makeTaxonTable("/home/folder/taxonTable.csv")
#'
#' # Add a new taxon:
#' makeTaxonTable("/home/folder/taxonTable.csv", "Pimelea spicata")
#'
#' # Add several taxa:
#' makeTaxonTable("/home/folder/taxonTable.csv", c("Pimelea spicata", "Doryphora sassafras", "Bidens pilosa"))
#' }
#'
#'
#'
makeTaxonTable <- function(taxonTablePath = NULL,
                           newTaxa = NULL)
{
  cat("makeTaxonTable\n========================================\n")

  if (is.null(taxonTablePath)) stop("makeTaxonTable: Please give a value for taxonTablePath.")

  if (!dir.exists(dirname(taxonTablePath)))
    stop("makeTaxonTable: the folder given in taxonTablePath does not exist.")
  else
    taxonTable <- read.csv(taxonTablePath, stringsAsFactors = FALSE)

  if (is.null(newTaxa))
  {
    cat("No new taxa to be processed: refreshing whole taxon table.\n")
    taxonList <- taxonTable$acceptedName
  }
  else
  {
    cat("New taxa will be added to the taxon table and existing taxa refreshed.\n")
    taxonList <- c(taxonTable$acceptedName, newTaxa)
  }

  cat("\nProcessing:\n")

  newTaxonTable <- NULL

  for (thisTaxon in taxonList)
  {
    cat(" ", thisTaxon, "\n")

    alaInfo <- processALA::checkTaxonName(thisTaxon)
    if (alaInfo$isValid[1] && alaInfo$isAccepted[1])
    {
      newEntry <- c(acceptedName = alaInfo$acceptedName[1],
                    genus = alaInfo$genus[1],
                    species = alaInfo$specise[1],
                    taxonAuthor = alaInfo$taxonAuthor[1],
                    nameFormatted = alaInfo$formattedAcceptedName[1],
                    apcURL = paste0("https://bie.ala.org.au/species/", alaInfo$acceptedFullGID[1]),
                    synonyms = alaInfo$synonyms[1],
                    APCfamily = alaInfo$apcFamily[1],
                    nswName = "",
                    plantNETurl = "",
                    plantNETfullName = "",
                    plantNETcommonNames = "",
                    plantNETfamily = "",
                    plantNETsubfamily = "",
                    hasSpecialZones = FALSE)

      newTaxonTable <- rbind(newTaxonTable, newEntry)
    }
    else
    {
      cat("   *** ALA says", thisTaxon, "is", ifelse(alaInfo$isValid[1], "Valid", "Not Valid"), "and is", ifelse(alaInfo$isAccepted[1], "Accepted", "Not accepted"), "\n")
    }
  }

  write.csv(newTaxonTable, taxonTablePath, row.names = FALSE)
}
