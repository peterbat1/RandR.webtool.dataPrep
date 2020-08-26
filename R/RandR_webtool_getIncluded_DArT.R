
# load("/home/peterw/Downloads/EucaPipe_DE19-4659_2.rda")
# ii <- which(dart_data$meta$analyses$Samplesforanalysis == "Include")
# coords <- data.frame(catalogNumber = dart_data$meta$sample_names[ii], longitude = dart_data$meta$long[ii], latitude = dart_data$meta$lat[ii])
# write.csv(coords, "/home/peterw/Restore and Renew/ALA-records/SpeciesData/Eucalyptus piperita/Ecualyptus_piperita_RandR_cleaned.csv", row.names = FALSE)

#
load("/home/peterw/Downloads/EucaSieb_DE19-4759_1.rda")
ii <- which(dart_data$meta$analyses$Samples.for.analysis == "Include")
coords <- data.frame(catalogNumber = dart_data$meta$sample_names[ii], longitude = dart_data$meta$long[ii], latitude = dart_data$meta$lat[ii])
write.csv(coords, "/home/peterw/Restore and Renew/ALA-records/SpeciesData/Eucalyptus sieberi/Eucalyptus_sieberi_RandR_cleaned.csv", row.names = FALSE)
