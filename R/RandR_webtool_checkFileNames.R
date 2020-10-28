

#' Data integrity check
#'
#' Checks for each file type having the required basic file name structure and that the species name component of filenames in each file type matches.
#'
#' @param baseFolder Character. Path to the folder below which the staging data sub-foddlers are found. The default value is the path to the staging folder on Linux PC Desert OEH34596.
#' @param taxonTablePath Character. Path to the current version of the webtool taxon table (ideally, recently cloned or pulled fom the GitLab repo)
#' @return
#' @export
#'
#' @examples
checkFileNames <- function(baseFolder = "/home/peterw/Restore and Renew/RandR-webtool-maintenance/New species staging",
                           taxonTablePath = "/home/peterw/Restore and Renew/RandR_webtool_dev/rbgsyd-restore-and-renew-git-repos/rbgsyd-restore-and-renew-data/data/taxonTable.csv")
{
  allTaxa <- NULL
  category <- NULL
  failure <- FALSE

  cat("RandR Webtool Data Preparation: Data integrity checks\n=======================================================\n")

  cat("Name structure tests\n---------------------\n")
  cat("  Herbarium records: ")
  herbRecNames <- list.files(paste0(baseFolder,"/herbarium_records"), "*.*")
  nHerbRecFiles <- length(herbRecNames)

  cat("Found", nHerbRecFiles, "file(s): ")

  # All names have the correct type tag and file extension?
  if (all(grepl("_herbariumRecords_filtered.csv", herbRecNames)))
  {
    cat("PASSED\n")
    herbRecTaxa <- sort(unique(gsub("_herbariumRecords_filtered.csv", "", herbRecNames, fixed = TRUE)))
    allTaxa <- c(allTaxa, herbRecNames)
  }
  else
  {
    cat("FAILED: the following file(s) are not correctly named:\n")
    print(herbRecNames[which(!grepl("_herbariumRecords_filtered.csv", herbRecNames))])
    failure <- TRUE
  }

  ###############################
  cat("  Domain geojson files: ")
  domainNames <- list.files(paste0(baseFolder,"/domain"), "*.*")
  nDomainFiles <- length(domainNames)

  cat("Found", nDomainFiles, "file(s): ")

  # All names have the correct type tag and file extension?
  if (all(grepl("_domain.geojson", domainNames)))
  {
    cat("PASSED\n")
    domainTaxa <- sort(unique(gsub("_domain.geojson", "", domainNames, fixed = TRUE)))
  }
  else
  {
    cat("FAILED: the following file(s) are not correctly named:\n")
    print(domainNames[which(!grepl("_domain.geojson", domainNames))])
    failure <- TRUE
  }

  ###############################
  cat("  GDM files: ")
  gdmNames <- list.files(paste0(baseFolder,"/gdm"), "*.*")
  nGDM_Files <- length(gdmNames)

  cat("Found", nGDM_Files, "file(s): ")

  # All names have the correct type tag and file extension?
  if (all(grepl("_genetic_model.Rd", gdmNames)))
  {
    cat("PASSED\n")
    gdmTaxa <- sort(unique(gsub("_genetic_model.Rd", "", gdmNames, fixed = TRUE)))
  }
  else
  {
    cat("FAILED: the following file(s) are not correctly named:\n")
    print(gdmNames[which(!grepl("_genetic_model.Rd", gdmNames))])
    failure <- TRUE
  }

  ###############################
  cat("  Species Information files: ")
  sppInfoNames <- list.files(paste0(baseFolder,"/speciesInfo"), "*.*")
  nSppInfoFiles <- length(sppInfoNames)

  cat("Found", nSppInfoFiles, "file(s): ")

  # All names have the correct type tag and file extension?
  if (all(grepl("_info.txt", sppInfoNames)))
  {
    cat("PASSED\n")
    sppInfoTaxa <- sort(unique(gsub("_info.txt", "", sppInfoNames, fixed = TRUE)))
  }
  else
  {
    cat("FAILED: the following file(s) are not correctly named:\n")
    print(sppInfoNames[which(!grepl("_info.txt", sppInfoNames))])
    failure <- TRUE
  }

  ###############################
  cat("  Images - species - distribution map files: ")
  distributionNames <- list.files(paste0(baseFolder,"/images/species"), "*_distribution.png")
  nDistributionFiles <- length(distributionNames)

  cat("Found", nDistributionFiles, "file(s): ")

  # All names have the correct type tag and file extension?
  if (all(grepl("_distribution.png", distributionNames)))
  {
    cat("PASSED\n")
    distributionTaxa <- sort(unique(gsub("_distribution.png", "", distributionNames, fixed = TRUE)))
  }
  else
  {
    cat("FAILED: the following file(s) are not correctly named:\n")
    print(distributionNames[which(!grepl("_distribution.png", distributionNames))])
    failure <- TRUE
  }

  ###############################
  cat("  Images - species - ID images: ")
  imageNames <- list.files(paste0(baseFolder,"/images/species"), "*.jpg")
  nImageFiles <- length(imageNames)

  cat("Found", nImageFiles, "ID image file(s); ")

  #cat("  Images - species - ID image captions: ")
  captionNames <- list.files(paste0(baseFolder,"/images/species"), "*.txt")
  nCaptionFiles <- length(captionNames)

  cat("Found", nCaptionFiles, "ID image caption file(s): ")

  baseImageNames <- gsub(".jpg", "", imageNames, fixed = TRUE)
  baseCaptionNames <- gsub(".txt", "", captionNames, fixed = TRUE)

  if (nImageFiles > nCaptionFiles)
  {
    cat("FAILED: More image files than captions: Orphaned image files:\n")
    print(imageNames[which(!(baseImageNames %in% baseCaptionNames))])
    failure <- TRUE
  }
  else
  {
    if (nImageFiles < nCaptionFiles)
    {
      cat("FAILED: More captions than image files: Orphaned caption files:\n")
      print(captionNames[which(!(baseCaptionNames %in% baseImageNames))])
      failure <- TRUE
    }
    else
    {
      cat("PASSED\n")
      imageTaxa <- sort(unique(gsub("_\\d", "", baseImageNames)))
      captionTaxa <- sort(unique(gsub("_\\d", "", baseCaptionNames)))
    }
  }

  ###############################
  if (failure)
  {
    cat("**** Taxon name matching cannot proceed until file name problems are fixed.\n")
  }
  else
  {

    cat("\nTaxon name matching\n---------------------\n")

    categoryList <- c("herbariumRecords", "domain", "gdm", "speciesInfo",
                      "distribution", "image", "caption")

    # Neat trick to test equality of numeric values in a vector from a post by John on StackOverflow post
    # https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
    # Same number of taxa in each data type:
    numTaxa <- c(length(herbRecTaxa), length(domainTaxa), length(gdmTaxa),
                 length(sppInfoTaxa), length(distributionTaxa),
                 length(imageTaxa), length(captionTaxa))

    names(numTaxa) <- categoryList

    ans <- all(abs(numTaxa - mean(numTaxa)) == 0)
    #print(ans)

    if (ans)
    {
      cat("  Same number of taxa present in all categories\n")

      allTaxa <- c(herbRecTaxa, domainTaxa, gdmTaxa,
                   sppInfoTaxa, distributionTaxa,
                   imageTaxa, captionTaxa)

      taxonList <- sort(unique(allTaxa))

      category <- NULL
      for (i in 1:length(numTaxa))
      {
        tmpVec <- rep(1, numTaxa[i])
        category <- c(category, rep(categoryList[i], numTaxa[i]))
      }

      df <- data.frame(taxa = allTaxa, category = category, stringsAsFactors = FALSE)

      taxonNameFailure <- FALSE
      for (thisTaxon in taxonList)
      {
        taxonSet <- subset(df, taxa == thisTaxon)
        if (!all(taxonSet$category %in% categoryList))
        {
          cat("FAILED: ", thisTaxon, "is missing from:\n")
          print(categoryList[which(!categoryList %in% taxonSet$category)])
          taxonNameFailure <- TRUE
        }
      }

      if (!taxonNameFailure)
      {
        cat("  Taxon names are consistent across all data categories  \n")

        # So far, so good. Now test that all names are in the taxon table...
        taxonList <- gsub("_", " ", taxonList, fixed = TRUE)

        taxonTable <- read.csv(taxonTablePath)

        if(all(taxonList %in% taxonTable$acceptedName))
        {
          cat("  All taxon names are found in the current version of the webtool taxon table\n")
        }
        else
        {
          cat("  FAILED: These taxa are missing from the current version of the webtool taxon table:\n")
          print(taxonList[which(!(taxonList %in% taxonTable$acceptedName))])
          cat("\n  Please run an update of the webtool taxon table following the instructions in the webtool maintenance manual.\n")
        }
      }
    }
    else
    {
      cat("  WARNING: Number of taxa in categories is not constant:\n")
      print(numTaxa)
    }
  }


  ###############################
  # Which gdm models have associated Q files?
  cat("\nChecking ancestry coefficient (Q-files)\n----------------------------------------\n")

  q_List <- list.files(paste0(baseFolder, "/qData/eastOZ"), "*.tif")

  for (thisGDM in gdmNames)
  {
    load(paste0(baseFolder,"/gdm/", thisGDM))
    if (md$Q)
    {
      cat("  Q-file(s) expected for", thisGDM, ":\n") #, paste0(md$qdata, collapse = "; "),"\n")
      if (is.null(md$qdata))
        cat("    WARNING: Q-file object is missing from GDM object!\n\n")
      else
      {
        theFiles <- unlist(lapply(md$qdata@layers, function(el) {basename(el@file@name)}))

        if (!all(theFiles %in% q_List))
        {
          cat("    FAILED: Missing q-files\n\n")
        }
        else
        {
          cat("    PASSED: all q-files located\n\n")
        }
      }
    }
  }
}
