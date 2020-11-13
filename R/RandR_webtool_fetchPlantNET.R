###############################################################
#' Fetch PlantNET information for a taxon
#'
#' Search for a matching taxon on PlantNET and, if found, return a named list containing useful information extracted from the return web page. No resolvable name returns "No_data" list elements.
#'
#' @param thisTaxon Character string representign the name of the taxon to be fetched
#' @param verbose Logical. Should progress messages be written to the console? Default is FALSE
#'
#' @return
#' {A named list with the following elements:
#' \item{nswName}{The taxonomic name used in NSW as reported by PlantNET}
#' \item{plantNETurl}{Ther recovered URL to the taxon entry on PlantNET}
#' \item{plantNETfullName}{Full taxonomic name (taxon plus author inforamtion) reproted by PlantNET}
#' \item{plantNETcommonNames}{A multivalued field of any common names supplied by PlantNET with elements separated by ';'.}
#' \item{plantNETfamily}{The name of the family used by PlantNET for this taxon concept}
#' \item{plantNETsubfamily}{The name of the sub-family (if present) used by PlantNET for this taxon concept}
#' }
#' @export
#'
#' @examples
#' \dontrun{}
fetchPlantNET <- function(thisTaxon, verbose = FALSE)
{
  if (verbose) cat("Fetching PlantNET record\n")

  if (verbose) cat("    Original taxon:", thisTaxon,"\n")

  # Trim interim names to 2 parts as PlantNET cannot handle infra-specific taxa
  nameBits <- unlist(strsplit(thisTaxon, " "))
  if (length(nameBits) > 2)
    thisTaxon <- paste(nameBits[1:2], collapse = " ")

  if (verbose) cat("    Trimmed taxon:", thisTaxon, "\n")

  thisURL <- paste0("http://plantnet.rbgsyd.nsw.gov.au/cgi-bin/NSWfl.pl?page=nswfl&lvl=sp&name=", gsub(" ", "~", thisTaxon, fixed = TRUE))
  if (verbose) cat("    PlanrNET URL:", thisURL, "\n")

  stuff <- httr::GET(thisURL)

  responseCode <- stuff$status_code
  if (verbose) cat("    Response code =", responseCode, "\n")

  if (responseCode == 200)
  {
    nswName <- thisTaxon

    guts <- httr::content(stuff)

    if (grepl("No entry in Flora of NSW", guts, fixed = TRUE))
    {
      if (verbose) cat("      Found 'No entry in Flora of NSW' in response\n")
      nswName <- "Not_accepted"
      thisURL <- "No_data"
      #plantNETurl <- "No_data"
      plantNETcommonNames <- "No_data"
      plantNETfamily <- "No_data"
      plantNETsubfamily <- "No_data"
      plantNETfullname <- "No_data"
    }
    else
    {
      if (verbose) cat("      Processing content in response\n")

      topSplit <- xml2::xml_children(guts)
      tables <- xml2::xml_find_all(topSplit[2], ".//table")
      usefulStuff <- xml2::xml_contents(xml_contents(tables[5])[1])
      part2 <- xml_find_all(xml2::xml_find_all(usefulStuff, ".//table")[2], ".//tr")
      strBits <- strsplit(xml2::xml_text(part2), "Family", fixed = TRUE)
      plantNETfullname <- stringr::str_squish(strBits[[1]][1])

      familyBits <- trimws(unlist(strsplit(strBits[[1]][2], "Subfamily ", fixed = TRUE)))

      plantNETfamily <- familyBits[1]

      plantNETsubfamily <- "No_data"
      if (length(familyBits) == 2)
      {
        plantNETsubfamily <- familyBits[2]
      }

      divs <- xml2::xml_find_all(topSplit[2], ".//div")

      if (length(divs) == 2)
      {
        plantNETcommonNames <- gsub(",", ";", stringr::str_to_title(trimws(sub("Common name: ", "", xml2::xml_text(divs[1]), fixed = TRUE))), fixed = TRUE)
      }
      else
      {
        plantNETcommonNames <- "No_data"
      }
    }
  }
  else
  {
    if (verbose) cat("    Failed response from PlantNET: HTML code =",responseCode,"\n")
    nswName <- "Not_accepted"
    thisURL <- "No_data"
    #plantNETurl = "No_data"
    plantNETcommonNames = "No_data"
    plantNETfamily <- "No_data"
    plantNETsubfamily <- "No_data"
    plantNETfullname <- "No_data"
  }

  PlantNET_result <- list("nswName" = nswName,
                          "plantNETurl" = thisURL,
                          "plantNETfullName" = plantNETfullname,
                          "plantNETcommonNames" = plantNETcommonNames,
                          "plantNETfamily" = plantNETfamily,
                          "plantNETsubfamily" = plantNETsubfamily)

  return(PlantNET_result)
}

