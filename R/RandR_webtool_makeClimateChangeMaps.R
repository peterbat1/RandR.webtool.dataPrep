# Restore and Renew
#
# Make png images of MaxEnt models for potential use on RBG Restore & Renew website
#
# Peter D. Wilson
# Honorary Research Associate
# Evolutionary Ecology Section
# Science and Conservation Branch
# Royal Botanic Gardens Sydney
#
# 20 December 2015; modified over a few weeks in January 2016 to apply
# Maurizio's preferred colourscheme, map furniture etc
# 13 January 2016: A version to produce a pdf with one species per page + info
# tables etc. This is intended to be used for "eye-balling" areas of interest
# or issues to be resolved as part of sampling planning.
# 3 March 2017: New website image rendition.
# 6 March 2017: Fuse latest current and projected 2070 maps.
# 17 March 2017: Re-run with revised data set.
# 25 May 2018: Apply to rcp45 2050 projections
# 18 Septemebr 2019: Adapted to process some new taxa

#require(raster)
#require(rgdal)
#require(imager)

#source("/home/peterw/R-scripts/Includes/gClip.R")
#source("/home/peterw/R-scripts/Includes/StandardExtents.R")



#' Make PNG Images of Current and Future ENM models
#'
#' @param theseTaxa Character array of taxon names
#' @param currentRasters Character array of full path names to rasters of current climate ENMs corresponding to the order of taxon names in \emph{theseTaxa}.
#' @param futureRasters Character array of full path names to rasters of future climate ENMs corresponding to the order of taxon names in \emph{theseTaxa}.
#' @param pngOutputPath Character object giving teh path to the folder into which PNG files will be written.
#' @param nswScope Logical. Should the rasters be clipped to the standard NSW extent? Default is TRUE.
#'
#' @return NULL
#' @export
#'
#' @examples
makeClimateChangeMaps <- function(theseTaxa = NULL, currentRasters = NULL, futureRasters = NULL, pngOutputPath = NULL, nswScope = TRUE)
{
  if (nswScope) gazetteer <- subset(gazetteer, gazetteer$scopeNSW == "Yes")

  cat("Restore & Renew: Making nice NSW maps for R&R webtool and R&R website:\n")

  for (thisTaxon in theseTaxa)
  {
    cat("\t",thisTaxon,"\n")

    ii <- which(theseTaxa == thisTaxon)

    this_Taxon <- sub(" ", "_", thisTaxon, fixed = TRUE)

    occData <- read.csv(paste0("/home/peterw/Restore and Renew/RandR-webtool-maintenance/New species staging/ENM fitting/", thisTaxon, "/", this_Taxon, "_herbariumRecords_filtered.csv"), stringsAsFactors=FALSE)

    # Somehow NA-lat/long values appear in some occurrence data files, so we must filter...
    badRows <- which(is.na(occData$Latitude))
    if (length(badRows) > 0) occData <- occData[-badRows, ]

    tissueSamples <- NULL

    if (file.exists(currentRasters[ii]))
    {
      maxentRaster <- raster(currentRasters[ii])

      if (nswScope && (!(raster::extent(maxentRaster) == NSW_extent)))
      {
        maxentRaster <- crop(maxentRaster, NSW_extent)
        #cat("********* cropping Current\n")
      }

      #### Current map
      png(paste0(pngOutputPath, "/", this_Taxon, "_Current.png"), width = 800, height = 600, units = "px")
      op <- par(oma = c(0, 0, 0, 1), mar = c(4.1, 4.1, 2, 1), xpd = TRUE)
      plot(maxentRaster, legend = FALSE, zlim = c(0, 1)) #,col=rasColours(256)
      plot(maxentRaster, axis.args = list(at = seq(0, 1, 0.2)), legend.args = list(text = "Environmental suitability", side = 4, line = 2.5), zlim = c(0, 1)) # ,col=rasColours(256)
      plot(coast.clipped, add = TRUE)

      plot(BarwonRiver, col = riverColour, add = TRUE)
      plot(DarlingRiver, col = riverColour, add = TRUE)
      plot(MacIntyreRiver, col = riverColour, add = TRUE)
      plot(MurrumbidgeeRiver, col = riverColour, add = TRUE)
      plot(MurrayRiver, col = riverColour, add = TRUE)
      plot(LachlanRiver, col = riverColour, add = TRUE)
      plot(MacquarieRiver, col = riverColour, add = TRUE)
      plot(ClarenceRiver, col = riverColour, add = TRUE)
      plot(HunterRiver, col = riverColour, add = TRUE)
      plot(GoulburnRiver, col = riverColour, add = TRUE)
      plot(ShoalhavenRiver, col = riverColour, add = TRUE)

      mtext(expression(paste("Longitude (",degree,")")), 1, line = 2.5)
      mtext(expression(paste("Latitude (",degree,")")), 2, line = 2.5)
      points(occData[,c("Longitude","Latitude")], pch = 21, bg = occColour, cex=0.7)
      if (!is.null(tissueSamples)) points(tissueSamples[,2:3], pch = 22, bg = dnaColour, cex=0.7)
      mtext(thisTaxon, line = 0.5, font = 4, cex = 1.2)
      mtext("Current climate", line = 0.5, font = 2, cex = 1.2, adj = 1)
      #text(152.25,-36.5,paste0("Model quality (test AUC): ",round(modelResults[thisTaxon,"testAUC"],2)),cex=0.65)
      #if (is.null(tissueSamples))
      legend(151.5, -35, "Occurrence records", pch = 21, pt.bg = occColour)
      #else
      #  legend(151.5, -35, c("Occurrence records", "DNA samples"), pch =c (21, 22), pt.bg = c(occColour, dnaColour))
      text(153,-37.5,"\uA9 2019 Royal Botanic Gardens\n & Domain Trust",col="grey20",cex=0.7)

      arrows(153.85, -31.75, 153.85, -31, lwd = 2)
      text(153.85, -30.85, "N")
      scalebar(200, c(152, -37), type = "bar", divs = 4, below = "km", lonlat = TRUE)

      text(gazetteer$DecLong, gazetteer$DecLat, gazetteer$Location, pos = gazetteer$pos)
      points(gazetteer$DecLong, gazetteer$DecLat, pch = gazetteer$Symbol, bg = gazetteer$Colour)

      par(op)
      dev.off()

      #### Future map
      futureMaxEnt <- raster::raster(meanRas_future_name[ii])

      if (nswScope && (!(raster::extent(futureMaxEnt) == NSW_extent)))
      {
        futureMaxEnt <- crop(futureMaxEnt, NSW_extent)
        #cat("********* cropping Future\n")
      }

      pngFile <- paste0(pngOutputPath, "/", this_Taxon, "_mean2050.png")
      png(pngFile, width = 800, height = 600, units = "px") # 800 x 600
      op <- par(oma = c(0, 0, 0, 1), mar = c(4.1, 4.1, 2, 1), xpd = TRUE)
      plot(futureMaxEnt, breaks = rasBreaks, legend = FALSE, zlim = c(0, 1)) # ,col=rasColours(256)
      plot(futureMaxEnt,axis.args=list(at=seq(0,1,0.2)),legend.args=list(text = "Environmental suitability", side = 4, line = 2.5), zlim = c(0, 1)) # ,col=rasColours(256)
      plot(coast.clipped,add=TRUE)

      plot(BarwonRiver, col = riverColour, add = TRUE)
      plot(DarlingRiver, col = riverColour, add = TRUE)
      plot(MacIntyreRiver, col = riverColour, add = TRUE)
      plot(MurrumbidgeeRiver, col = riverColour, add = TRUE)
      plot(MurrayRiver, col = riverColour, add = TRUE)
      plot(LachlanRiver, col = riverColour, add = TRUE)
      plot(MacquarieRiver, col = riverColour, add = TRUE)
      plot(ClarenceRiver, col = riverColour, add = TRUE)
      plot(HunterRiver, col = riverColour, add = TRUE)
      plot(GoulburnRiver, col = riverColour, add = TRUE)
      plot(ShoalhavenRiver, col = riverColour, add = TRUE)

      mtext(expression(paste("Longitude (",degree,")")),1,line=2.5)
      mtext(expression(paste("Latitude (",degree,")")),2,line=2.5)
      #points(occData[,2:3],pch=21,bg=symbolColour,cex=0.8)
      mtext(thisTaxon,line=0.5,font=4,cex=1.2)
      mtext("2050 climate",line=0.5,font=2,cex=1.2,adj=1)
      text(153,-37.5,"\uA9 2019 Royal Botanic Gardens\n & Domain Trust",col="grey20",cex=0.7)
      arrows(153.85,-31.75,153.85,-31,lwd=2)
      #segments(153.75,-31.75,153.95,-31.75,lwd=2)
      text(153.85,-30.85,"N")
      scalebar(200,c(152,-37),type="bar",divs=4,below="km",lonlat=TRUE)
      text(gazetteer$DecLong,gazetteer$DecLat,gazetteer$Location,pos=gazetteer$pos)
      points(gazetteer$DecLong,gazetteer$DecLat,pch=gazetteer$Symbol,bg=gazetteer$Colour)
      par(op)
      dev.off()
    }

    # Make 450px-wide image versions
    cat("  Resizing current image...")
    in_pngFolder <- paste0("/home/peterw/Restore and Renew/RandR_webtool_dev/ClimateChangeMaps_remake/")
    out_pngFolder <- paste0("/home/peterw/Restore and Renew/RandR_webtool_dev/ClimateChangeMaps_remake_450/")

    inFile <- paste0(in_pngFolder, "/", this_Taxon, "_Current.png")
    inImage <- imager::load.image(inFile)

    outFile <- paste0(out_pngFolder, "/", this_Taxon, "_Current_450.png")

    outImage <- imager::resize(inImage, size_x = 450, size_y = 338, interpolation_type = 5)

    imager::save.image(outImage, outFile, quality = 0.8)
    cat("done\n  Resizing future image...")

    inFile <- paste0(in_pngFolder, "/", this_Taxon, "_mean2050.png")
    inImage <- imager::load.image(inFile)

    outFile <- paste0(out_pngFolder, "/", this_Taxon, "_mean2050_450.png")

    outImage <- imager::resize(inImage, size_x = 450, size_y = 338, interpolation_type = 5)

    imager::save.image(outImage, outFile, quality = 0.8)
    cat("done\n")
  }

  cat("*** End of processing.\n")
}
