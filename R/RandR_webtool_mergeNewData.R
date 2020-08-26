

#' Merge new data
#'
#' Merge new data into the locally cloned DATA repository.
#'
#' @param srcBaseFolder Character. Path to the base folder of the staged new data.
#' @param dstBaseFolder Character. Path to the base folder of the local clone of the GitLab R&R Webtool \emph{DATA} repository.
#' @param overwrite Logical. Should files in the destiantion folder be overwritten? Default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
mergeNewData <- function(srcBaseFolder = "/home/peterw/Restore and Renew/RandR-webtool-maintenance/New species staging",
                         dstBaseFolder = "/home/peterw/Restore and Renew/RandR_webtool_dev/rbgsyd-restore-and-renew-git-repos/rbgsyd-restore-and-renew-data/data",
                         overwrite = FALSE)
{
  cat("Merging new data files into cloned repository\n----------------------------------------------------------\n")

  # Merge herbarium records files
  srcFolder <- paste0(srcBaseFolder, "/herbarium_records")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/herbarium_records")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  Herbarium record files: Moved", length(srcFiles), "\n")

  # Merge domain files
  srcFolder <- paste0(srcBaseFolder, "/domain")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/domain")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  Domain files: Moved", length(srcFiles), "\n")

  # Merge gdm files
  srcFolder <- paste0(srcBaseFolder, "/gdm")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/gdm")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  GDM model files: Moved", length(srcFiles), "\n")

  # Merge climate change text
  srcFolder <- paste0(srcBaseFolder, "/climateChange")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/climateChange")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  Climate change info files: Moved", length(srcFiles), "\n")

  # Merge genetics info text
  srcFolder <- paste0(srcBaseFolder, "/geneticsInfo")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/geneticsInfo")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  Genetics info files: Moved", length(srcFiles), "\n")

  # Merge species info text
  srcFolder <- paste0(srcBaseFolder, "/speciesInfo")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/speciesInfo")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  Species info text files: Moved", length(srcFiles), "\n")

  # Merge BRECI images
  srcFolder <- paste0(srcBaseFolder, "/images/BRECI")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/images/BRECI")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  BRECI image files: Moved", length(srcFiles), "\n")

  # Merge ENMmaps images
  srcFolder <- paste0(srcBaseFolder, "/images/ENMmaps")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/images/ENMmaps")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  ENM map files: Moved", length(srcFiles), "\n")

  # Merge species and distribution maps images + caption text
  srcFolder <- paste0(srcBaseFolder, "/images/species")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/images/species")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  Species and distribution maps images + caption text files: Moved", length(srcFiles), "\n")

  # Merge qData files
  srcFolder <- paste0(srcBaseFolder, "/qData/eastOZ")
  srcFiles <- list.files(srcFolder, "*.*", full.names = TRUE)
  dstFolder <- paste0(dstBaseFolder, "/qData/eastOZ")
  dstFiles <- paste0(dstFolder, "/", basename(srcFiles))
  file.copy(srcFiles, dstFiles, overwrite = overwrite)
  cat("  qData files: Moved", length(srcFiles), "\n")

  cat("\n\n*** File merge completed.\n")
}
