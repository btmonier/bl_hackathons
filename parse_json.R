

testURL <- "https://rest.isric.org/soilgrids/v2.0/properties/query?lon=%f&lat=%f&property=clay&property=soc&depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm&value=Q0.05&value=Q0.5&value=Q0.95&value=mean&value=uncertainty"


formatSoilGrid <- function(
    json = NULL
) {
    if (is.null(json)) stop("Missing JSON data.")
    if (!is.list(json)) stop("This does not appear to be JSON data.")
    if (is.null(json$properties$layers)) stop("This does not appear to be SoilGrid data.")

    tmpProc <- lapply(seq_len(length(json$properties$layers$depths)), function(i) {
        tmpDf      <- json$properties$layers$depths[[i]]

        tmpDf <- cbind(tmpDf$label, tmpDf$values)

        tmpDf$name <- json$properties$layers$name[i]
        tmpDf$unit <- jsonReq$properties$layers$unit_measure$mapped_units[i]
        tmpDf$lon  <- json$geometry$coordinates[1]
        tmpDf$lat  <- json$geometry$coordinates[2]

        colnames(tmpDf) <- c(
            "depth", "q_5", "q_50", "q_95",
            "mean", "uncertainty", "property",
            "unit", "lon", "lat"
        )
        # return(tmpDf[, 4:ncol(tmpDf)])
        # return(do.call("rbind", tmpDf))
        return(tmpDf)
    })

    return(do.call("rbind", tmpProc))
}


getSoilData <- function(
    lon = 0,
    lat = 0,
    properties = c("clay", "soc")
) {
    if (is.null(url)) stop("Missing URL signature.")

    url <- testURL

    finalURL <- sprintf(url, lon, lat)

    message("Getting query...")

    getReq  <- httr::GET(finalURL)
    getReq2 <- httr::content(getReq, "text", encoding = "ISO-8859-1")

    jsonReq <- jsonlite::fromJSON(getReq2)

    message("Finished (", round(jsonReq$query_time_s, 3), "s)")

    return(formatSoilGrid(jsonReq))
}


myData <- getSoilData(-72, -9)

print(myData)


