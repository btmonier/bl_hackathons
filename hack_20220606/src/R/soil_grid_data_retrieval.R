#--------------------------------------------------------------------
# Script Name:   soil_grid_data_retrieval.R
# Description:   Functions and tests to return SoilGrid data
# Author:        Brandon Monier
# Created:       2022-09-06 at 15:42:14
# Last Modified: 2022-09-06 at 16:00:23
#--------------------------------------------------------------------

## NOTE: Needs R >= 4.0 to run

# === Preamble ======================================================
library(jsonlite, include.only = c("fromJSON"))
library(httr, include.only = c("content", "GET"))
library(testthat)



# === Utility =======================================================

## General parameters (that can be integrated into funtion) ----
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


## ----
#' Format SoilGrid JSON data into data frame
#'
#' @param json SoilGrid JSON data
#'
#' @return A \code{data.frame} object
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



# === Main function =================================================

## ----
#' Get soil property data from SoilGrids
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param properties A SoilGrid property. Defaults to \code{clay}. Possible
#' @param verbose Report messages to console? Defaults to \code{FALSE}.
#'
#' @return A `data.frame` object
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @export
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

    getReq  <- GET(finalURL)
    getReq2 <- content(getReq, "text", encoding = "ISO-8859-1")

    jsonReq <- fromJSON(getReq2)

    if (verbose) message("Finished (", round(jsonReq$query_time_s, 3), "s)")

    return(formatSoilGrid(jsonReq))
}



# === TESTS =========================================================

## Test return information ----
test_that("`getSoilData()` returns correct data", {
    testDataA <- getSoilData(-79, -9, "clay", FALSE)
    testDataB <- getSoilData(-79, -9, c("clay", "sand"))

    expectedNames <- c(
        "depth", "q_5", "q_50", "q_95",
        "mean", "uncertainty", "property",
        "unit", "lon", "lat"
    )

    expect_s3_class(testDataA, "data.frame")
    expect_equal(nrow(testDataA), 6)
    expect_equal(nrow(testDataB), 12)
    expect_equal(ncol(testDataA), 10)
    expect_setequal(colnames(testDataA), expectedNames)
})


## Test for errors ----
test_that("`getSoilData()` returns correct errors", {
    expect_error(
        object = getSoilData(properties = "Hay"),
        regexp = "Incorrect properties."
    )

    expect_error(
        object = getSoilData(properties = c("Hay", "Air")),
        regexp = "Incorrect properties."
    )
})

testthat::test_that("`formatSoilGrid()` returns correct errors", {
    expect_error(
        object = formatSoilGrid(),
        regexp = "Missing JSON data."
    )

    expect_error(
        object = formatSoilGrid(letters),
        regexp = "This does not appear to be JSON data."
    )
})


