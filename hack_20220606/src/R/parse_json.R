

configParams <- list(
    urlTemplate = "https://rest.isric.org/soilgrids/v2.0/properties/query?lon=%f&lat=%f%s&depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm&value=Q0.05&value=Q0.5&value=Q0.95&value=mean&value=uncertainty",
    acceptedProps = c(
        "bdod",
        "cec",
        "clay",
        "cfvo",
        "nitrogen",
        "ocd",
        "ocs",
        "phh2o",
        "sand",
        "silt",
        "soc"
    )
)

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
        tmpDf$unit <- json$properties$layers$unit_measure$mapped_units[i]
        tmpDf$lon  <- json$geometry$coordinates[1]
        tmpDf$lat  <- json$geometry$coordinates[2]

        colnames(tmpDf) <- c(
            "depth", "q_5", "q_50", "q_95",
            "mean", "uncertainty", "property",
            "unit", "lon", "lat"
        )
        return(tmpDf)
    })

    return(do.call("rbind", tmpProc))
}


#' Get soil property data from SoilGrids
#' @param lon Longitude
#' @param lat Latitude
#' @param properties A SoilGrid property
#' @return A `data.frame` object
getSoilData <- function(
    lon = 0,
    lat = 0,
    properties = "clay",
    verbose = TRUE
) {
    if (is.null(url)) stop("Missing URL signature.")

    if (any(!properties %in% configParams$acceptedProps)) stop("Incorrect properties.")

    url <- configParams$urlTemplate
    propString <- paste("&property=", properties, collapse = "", sep = "")

    finalURL <- sprintf(url, lon, lat, propString)

    if (verbose) message("Getting query...")

    getReq  <- httr::GET(finalURL)
    getReq2 <- httr::content(getReq, "text", encoding = "ISO-8859-1")

    jsonReq <- jsonlite::fromJSON(getReq2)

    if (verbose) message("Finished (", round(jsonReq$query_time_s, 3), "s)")

    return(formatSoilGrid(jsonReq))
}


## Test return information ----
testSoilGridData <- function() {
    testData <- getSoilData(-79, -9, "clay", FALSE)
    expectedNames <- c(
        "depth", "q_5", "q_50", "q_95",
        "mean", "uncertainty", "property",
        "unit", "lon", "lat"
    )

    stopifnot(nrow(testData) == 6)
    stopifnot(all(expectedNames %in% colnames(testData)))
}

testSoilGridData()















