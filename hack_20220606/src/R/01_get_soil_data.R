source("hack_20220606/src/R/parse_json.R")

library(magrittr)


sampleMetaData <- data.table::fread("hack_20220606/data/sample_coordinates.csv")


soilDataLs <- list()

for (i in 1:nrow(sampleMetaData)) {
    message("Processing sample: ", sampleMetaData$Sample[i])
    tmpSg <- getSoilData(sampleMetaData$LON[i], sampleMetaData$LAT[i], configParams$acceptedProps)
    tmpSg$sample_id <- sampleMetaData$Sample[i]

    soilDataLs[[i]] <- tmpSg
}

soilDataLs <- do.call("rbind", soilDataLs)
