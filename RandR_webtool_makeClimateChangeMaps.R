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

require(raster)
require(rgdal)
require(imager)

source("/home/peterw/R-scripts/Includes/gClip.R")
source("/home/peterw/R-scripts/Includes/StandardExtents.R")

#pngFolder <- paste0("/home/peterw/Restore and Renew/newWebsite/")
pngFolder <- paste0("/home/peterw/Restore and Renew/RandR_webtool_dev/ClimateChangeMaps_remake/")

occColour <- "yellow"
dnaColour <- "red"
riverColour <- "cornflowerblue"

coast.clipped <- readOGR("/home/peterw/MapData/Australia/RandR_clipped_coast.shp", verbose = FALSE)

gazetteer <- read.csv("/home/peterw/Restore and Renew/Metadata/RestoreRenew_Gazetteer.csv", stringsAsFactors = FALSE)
gazetteer <- subset(gazetteer,gazetteer$scopeNSW == "Yes")

#NSWbounds <- extent(c(140.5,154.5,-38,-27.5))

rasBreaks <- seq(0,1,1/256)

puddlePath <- "/home/peterw/MapData/Watercourses/"

#theseTaxa <- readLines("/home/peterw/Restore and Renew/Metadata/taxonList.csv")
#theseTaxa <- theseTaxa[which(theseTaxa == "Orites excelsus"):length(theseTaxa)]
#theseTaxa <- readLines("/home/peterw/Restore and Renew/dbSandbox/Metadata/taxonList.csv")
theseTaxa <- c("Angophora hispida", "Corymbia eximia", "Elaeocarpus reticulatus")

#bestModels <- read.csv("/media/peterw/D2/Metadata/bestModels.csv", row.names = 1, stringsAsFactors = FALSE)

# Instantiate objet d'river
BarwonRiver <- readOGR(paste0(puddlePath,"BarwonRiver-simple.shp"), verbose = FALSE)
DarlingRiver <- readOGR(paste0(puddlePath,"DarlingRiver-simple.shp"), verbose = FALSE)
MurrayRiver <- readOGR(paste0(puddlePath,"MurrayRiver-simple.shp"), verbose = FALSE)
# Clip Murray River to raster bounds
MurrayRiver <- gClip(MurrayRiver,as.matrix(NSW_extent))
LachlanRiver <- readOGR(paste0(puddlePath,"LachlanRiver-simple.shp"), verbose = FALSE)
MurrumbidgeeRiver <- readOGR(paste0(puddlePath,"MurrumbidgeeRiver-simple.shp"), verbose = FALSE)
MacquarieRiver <- readOGR(paste0(puddlePath,"MacquarieRiver-simple.shp"), verbose = FALSE)
MacIntyreRiver <- readOGR(paste0(puddlePath,"MacIntyreRiver-simple.shp"), verbose = FALSE)
ShoalhavenRiver <- readOGR(paste0(puddlePath,"ShoalhavenRiver-simple.shp"), verbose = FALSE)
HunterRiver <- readOGR(paste0(puddlePath,"HunterRiver-simple.shp"), verbose = FALSE)
GoulburnRiver <- readOGR(paste0(puddlePath,"GoulburnRiver-simple.shp"), verbose = FALSE)
ClarenceRiver <- readOGR(paste0(puddlePath,"ClarenceRiver-simple.shp"), verbose = FALSE)

rasColours <- colorRampPalette(c("white","darkolivegreen3")) #### meOliveGreen

cat("Restore & Renew: Making nice NSW maps for R&R webtool and R&R website:\n")

selectedRegMult <- c(2, 1, 1)
names(selectedRegMult) <- theseTaxa




#thisTaxon <- "Acacia binervata"
for (thisTaxon in theseTaxa)
{
  cat("\t",thisTaxon,"\n")

  this_Taxon <- sub(" ", "_", thisTaxon, fixed = TRUE)

  #bestRegStr <- sub(".","_",bestModels[thisTaxon,"bestReg"],fixed=TRUE)
  bestRegStr <- "reg_1"

  #occData <- read.csv(paste0("/home/peterw/Restore and Renew/ALA-records/SpeciesData/",thisTaxon,"/",thisTaxon,"_filtered_ALA_records_2017-02-28.csv"),stringsAsFactors=FALSE)
  #occData <- read.csv(paste0("/home/peterw/Restore and Renew/ALA-records/SpeciesData/",thisTaxon,"/",this_Taxon,"_herbariumRecords_filtered.csv"),stringsAsFactors=FALSE)
  occData <- read.csv(paste0("/home/peterw/Restore and Renew/RandR-webtool-maintenance/New species staging/ENM fitting/", thisTaxon, "/", this_Taxon, "_herbariumRecords_filtered.csv"), stringsAsFactors=FALSE)

  # Somehow NA-lat/long values appear in some occurrence data files, so we must filter...
  badRows <- which(is.na(occData$Latitude))
  if (length(badRows) > 0) occData <- occData[-badRows, ]

  tissueSamples <- NULL

  #meanRas_current_name <- paste0("/media/peterw/D3/ENM output/",thisTaxon,"/thinned_0/filteredVars/reg_1/",this_Taxon,"_mxe_avg_NSW_logistic.tif")

  meanRas_current_name <- paste0("/home/peterw/Restore and Renew/RandR-webtool-maintenance/New species staging/ENM fitting/", thisTaxon, "/", thisTaxon, "/basicFit/reg_", selectedRegMult[thisTaxon], "/", this_Taxon, "_basicFit_reg_", selectedRegMult[thisTaxon],".tif")

  if (file.exists(meanRas_current_name))
  {

    maxentRaster <- raster(meanRas_current_name)

    if (!(extent(maxentRaster) == NSW_extent))
    {
      maxentRaster <- crop(maxentRaster, NSW_extent)
      cat("********* cropping Current\n")
    }
    #maxentResults <- read.csv(paste0("/media/peterw/D3/ENM output/",thisTaxon,"/thinned_0/filteredVars/",bestRegStr,"/maxentResults.csv"), stringsAsFactors = FALSE)
    # meanEntropy <- maxentResults[11,"Entropy"]
    # values(maxentRaster) <- transform_Raw2Logistic(values(maxentRaster),meanEntropy)

    #######occData <- removeDuplicates2(occData,maxentRaster, byGrid = TRUE, quiet = TRUE)


    #### Current map
    png(paste0(pngFolder, "/", this_Taxon, "_Current.png"), width = 800, height = 600,units = "px")
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
    text(153,-37.5,"\uA9 2017 Royal Botanic Gardens\n & Domain Trust",col="grey20",cex=0.7)

    arrows(153.85,-31.75,153.85,-31,lwd=2)
    text(153.85,-30.85,"N")
    scalebar(200,c(152,-37),type="bar",divs=4,below="km",lonlat=TRUE)

    text(gazetteer$DecLong, gazetteer$DecLat, gazetteer$Location, pos = gazetteer$pos)
    points(gazetteer$DecLong, gazetteer$DecLat, pch = gazetteer$Symbol, bg = gazetteer$Colour)

    par(op)
    dev.off()

    #### Future map
    #futureFile <- paste0("/media/peterw/D3/ENM output/",thisTaxon,"/thinned_0/filteredVars/",bestRegStr,"/proj2070/",sub(" ","_",thisTaxon,fixed=TRUE),"_mean_2070.tif")
    #meanRas_future_name <- paste0("/media/peterw/D3/ENM output/",thisTaxon,"/thinned_0/filteredVars/reg_1/proj2050_rcp45/",this_Taxon,"_2050_grandMean.tif")

    meanRas_future_name <- paste0("/home/peterw/Restore and Renew/RandR-webtool-maintenance/New species staging/ENM fitting/", thisTaxon, "/", thisTaxon, "/basicFit/projMean2050_rcp45/", this_Taxon, "basicFit_reg_", selectedRegMult[thisTaxon],"_mean2050_rcp45.tif")

    futureMaxEnt <- raster(meanRas_future_name)

    if (!(extent(futureMaxEnt) == NSW_extent))
    {
      futureMaxEnt <- crop(futureMaxEnt, NSW_extent)
      cat("********* cropping Future\n")
    }


    pngFile <- paste0(pngFolder, "/", this_Taxon, "_mean2050.png")
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
    text(153,-37.5,"\uA9 2017 Royal Botanic Gardens\n & Domain Trust",col="grey20",cex=0.7)
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

