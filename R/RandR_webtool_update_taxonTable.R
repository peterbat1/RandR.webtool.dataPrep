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
#' @details { The R&R database table \emph{taxonTable} is used to make a new version of the table used to 'drive' the webtool by providing:
#' \itemize{
#' \item Canonical accepted taxon names derived from the National Species List (NSL) hosted by the Atlas of Living Australia (ALA)
#' \item Synonyms for accepted names to allow users some flexibility in identifying the taxon of interest
#' \item Addtional data for display in the webtool pages such as formatted full species name, the family to which each taxon belongs (according to the NSL), URLs to PlantNET and ALA pages for the taxon
#'}
#' The R&R database table is the upstream source for the webtool version, so adding a new taxon to the webtool must start with adding a new taxon to the R&R database version of \emph{taxonTable}. The function \emph{checkTaxonName()} in the package \emph{RandR.db}, with parameter setting 'commit == TRUE', should be used to do this.
#'
#' }
#'
#' @examples
#' \dontrun{makeTaxonTable("/home/folder")}
#'
makeTaxonTable <- function(outputPath = NULL)
{

  if (is.null(outputPath)) stop("makeTaxonTable: Please give a value for outputPath.")

  if (!dir.exists(outputPath)) stop("makeTaxonTable: the folder given in outputPath does not exist.")

  db <- dbConnect(SQLite(), .sqlPath_default)
  db_taxonTable <- dbReadTable(db, "taxonTable")
  dbDisconnect(db)

  old_taxonTable <- read.csv("/home/peterw/RBG Projects/Restore and Renew/RandR_webtool_dev/LOCAL-rbgsyd-restore-and-renew/rbgsyd-restore-and-renew-data/data/taxonTable.csv", stringsAsFactors = FALSE)

  db_taxonTable <- subset(db_taxonTable, db_taxonTable$rank == "species")

  # Patch synonyms to prepend formal synonomy with the accepted taxon name. This
  # will match the way the synonomy field was originally configured and reduce the
  # risk of any glitches.
  db_taxonTable$synonymy <- paste(db_taxonTable$acceptedName, db_taxonTable$synonymy, sep = ";")

  #c("acceptedName", "genus", "species", "taxonAuthor", "nameFormatted", "apcURL", "synonymy","isTrueRandR","APCfamily","nswName","plantNETurl", "plantNETfullName", "commonNames", "plantNETfamily", "plantNETsubfamily")

  new_taxonTable <- data.frame(db_taxonTable[, c("acceptedName", "nameFormatted", "apcURL", "synonymy","APCfamily","nswName","plantNETurl", "plantNETfullName", "plantNETcommonNames", "plantNETfamily", "plantNETsubfamily"),],
                               "hasSpecialZones" <- rep("FALSE", nrow(db_taxonTable)),
                               stringsAsFactors = FALSE)

  write.csv(new_taxonTable, "/home/peterw/RBG Projects/Restore and Renew/dbSandbox/Metadata/newWebtoolTaxonTable.csv", row.names = FALSE)
}
