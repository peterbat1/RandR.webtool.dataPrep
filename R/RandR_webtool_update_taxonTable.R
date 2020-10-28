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

  if (!dir.exists(taxonTablePath))
    stop("makeTaxonTable: the folder given in taxonTablePath does not exist.")
  else
    taxonTable <- read.csv(taxonTablePath, stringsAsFactors = FALSE)

<<<<<<< HEAD
  db <- RSQLite::dbConnect(SQLite(), .sqlPath_default)
  db_taxonTable <- RSQLite::dbGetQuery(db, "SELECT acceptedName, genus, species, taxonAuthor, nameFormatted, apcURL, synonyms, APCfamily, nswName, plantNETurl, plantNETfullName, plantNETcommonNames, plantNETfamily, plantNETsubfamily FROM taxonTable WHERE rank = 'species';")
  RSQLite::dbDisconnect(db)
print(colnames(db_taxonTable))
  ###old_taxonTable <- read.csv("/home/peterw/RBG Projects/Restore and Renew/RandR_webtool_dev/LOCAL-rbgsyd-restore-and-renew/rbgsyd-restore-and-renew-data/data/taxonTable.csv", stringsAsFactors = FALSE)

  #db_taxonTable <- subset(db_taxonTable, db_taxonTable$rank == "species")

  # Patch synonyms to prepend formal synonomy with the accepted taxon name. This
  # will match the way the synonomy field was originally configured and reduce the
  # risk of any glitches.
 # db_taxonTable$synonymy <- paste(db_taxonTable$acceptedName, db_taxonTable$synonymy, sep = ";")
=======
  if (is.null(newTaxa))
  {
    cat("No new taxa to be processed: refreshing whole taxon table.\n")
    taxonList <- taxonTable$acceptedName
  }
  else
  {
    cat("New taxa will be added to the taxon table and existing taxa refreshed.\n")
    taxonList <- c(taxonTable$acceptedNames, newTaxa)
  }

  cat("\nProcessing:\n")

  newTaxonTable <- NULL

  for (thisTaxon in taxonList)
  {
    cat(" ", thisTaxon, "\n")
>>>>>>> 19038b692d7446fe94d723c597541bc1f1c50bff

    alaInfo <- processALA::checkTaxonName(thisTaxon)
    if (alaInfo$isValid[1] && alaInfo$isAccepted[1])
    {
      newEntry <- c(acceptedName = alaInfo$acceptedName[1],
                    genus = alaInfo$genus[1],
                    species = alaInfo$specise[1],
                    taxonAuthor = alaInfo$taxonAuthor[1],
                    nameFormatted = alaInfo$formattedAcceptedName[1],
                    apcURL = alaInfo$acceptedFullGID[1],
                    synonyms = alaInfo$synonyms[1],
                    APCfamily = alaInfo$apcFamily[1],
                    nswName = "",
                    plantNETurl = "",
                    plantNETfullName = "",
                    plantNETcommonNames = "",
                    plantNETfamily = "",
                    plantNETsubfamily = "",
                    hasSpecialZones = FALSE)

<<<<<<< HEAD
  new_taxonTable <- data.frame(db_taxonTable[, c("acceptedName", "genus", "species", "taxonAuthor","nameFormatted", "apcURL", "synonyms","APCfamily","nswName","plantNETurl", "plantNETfullName", "plantNETcommonNames", "plantNETfamily", "plantNETsubfamily"),],
                               "hasSpecialZones" = rep("FALSE", nrow(db_taxonTable)),
                               stringsAsFactors = FALSE)

  #emptyInd <- which(new_taxonTable$synonyms == "")
  #new_taxonTable$synonyms[emptyInd] <- new_taxonTable$acceptedName[emptyInd]

  write.csv(new_taxonTable, paste0(outputPath, "taxonTable.csv"), row.names = FALSE)
=======
      newTaxonTable <- rbind(newTaxonTable, newEntry)
    }
    else
    {
      cat("   *** ALA says", thisTaxon, "is", ifelse(alaInfo$isValid[1], "Valid", "Not Valid"), "and is", ifelse(alaInfo$isAccepted[1], "Accepted", "Not accepted"), "\n")
    }
  }

  write.csv(newTaxonTable, taxonTablePath, row.names = FALSE)
>>>>>>> 19038b692d7446fe94d723c597541bc1f1c50bff
}
