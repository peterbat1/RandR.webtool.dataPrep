
# Format occurrence data for the R&R Webtool

#' Update Webtool Herbarium occurrence files
#' @param theseTaxa Character. Vector of one or more taxon names to be processed. Default of 'all' causes all acceptedNames in the table pointed to by \emph{taxonTablePath} to be processed.
#' @param srcFolder Character. Path representing the source folder. See Details for more information.
#' @param addTaxonToPath Logical. Should the taxon name be added to the srcFolder. Default is FALSE.
#' @param dstFolder Character. Path representing the destination folder. See Details for more information.
#' @param taxonTablePath Character. Full path to the webtool taxon table to be used as a source of taxon names. Only required if \emph{theseTaxa} == 'all',
#'
#' @details {
#' The source path is interpreted as high-level folders within which a folder exists for each taxon listed in the current version of the R&R Webtool \emph{taxonTable} stored at /rbgsyd-restore-and-renew-git-repos/rbgsyd-restore-and-renew-data/data
#'
#' Source files are expected to be as produced by the function \emph{\link{filterALAdata}} in the package processALA. The fields in these files are stripped-down for the webtool version.
#'
#' The destination folder has no sub-folders; output files are written directly to the folder.
#' }
#'
#' @return NULL
#' @export
#'
#' @examples
update_occFiles <- function(theseTaxa = "all",
                            srcFolder = NULL,
                            addTaxontoPath = FALSE,
                            dstFolder = NULL,
                            taxonTablePath = NULL)
{
  if (is.null(srcFolder)) stop("srcFolder cannot be NULL")
  if (is.null(dstFolder)) stop("dstFolder cannot be NULL")
  if ((is.null(taxonTablePath)) && (theseTaxa[1] == "all")) stop("taxonTablePath cannot be NULL")

  cat("RandR Webtool: Update occurrence files\n==========================================\nProcessing:\n")

  if (theseTaxa[1] == "all")
  {
    taxonTable <- read.csv(taxonTablePath, stringsAsFactors = FALSE)
    theTaxa <- taxonTable$acceptedName
  }
  else
    theTaxa <- theseTaxa

  #theTaxa <- taxonTable$acceptedName

  for (thisTaxon in theTaxa)
  {
    cat("   ", thisTaxon, "\n")

    this_Taxon <- gsub(" ", "_", thisTaxon, fixed = TRUE)

    if (addTaxontoPath)
      inData <- read.csv(paste0(srcFolder,"/", thisTaxon, "/", this_Taxon, "_herbariumRecords_filtered.csv"), stringsAsFactors = FALSE)
    else
      inData <- read.csv(paste0(srcFolder,"/", this_Taxon, "_herbariumRecords_filtered.csv"), stringsAsFactors = FALSE)

    outData <- inData[, c("Catalogue.Number", "Longitude", "Latitude")]

    colnames(outData) <- c("catalogueNumber", "longitude", "latitude")

    write.csv(outData, paste0(dstFolder, "/", this_Taxon, "_herbariumRecords_filtered.csv"), row.names = FALSE)
  }

  cat("*** End of processing.\n")

}



