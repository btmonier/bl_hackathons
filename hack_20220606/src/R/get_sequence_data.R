library(rJava)
library(rPHG)
library(magrittr)
library(Biostrings)


## NOTE: this is just a backup for rPHG source methods to pull sequence information

## SETUP ----
params <- list(
    configPath = "../../Temporary/test_rphg/data/configSQLite_20220609.txt"
)
params$configPath %>% showPHGMethods()
myPhg <- graphBuilder(params$configPath, "GATK_PIPELINE", includeSequence = TRUE)


## ----
#' Get full DNAString set ----
asDNAStringSet <- function(phg) {

    if (class(phg) != "PHGDataSet") {
        stop("Data object must of class PHGDataSet")
    }

    jObj <- metadata(phg)$jObj
    myStringSet <- Biostrings::DNAStringSet()
    nRefRanges <- jObj$referenceRanges()$asList()$size()
    nTaxa <- jObj$totalNumberTaxa()

    stringIdx <- 1
    for (i in seq_len(nRefRanges)) {
        refRange <- jObj$nodes(jObj$referenceRanges()$asList()$get(as.integer(i - 1)))
        for (j in seq_len(nTaxa)) {
            hapId   <- refRange$get(as.integer(j - 1))$id()
            element <- refRange$get(as.integer(j - 1))
            taxon   <- element$taxaList()$get(0L)$getName()
            seq     <- element$haplotypeSequence()$sequence()

            myStringSet[[stringIdx]] <- seq
            names(myStringSet)[stringIdx] <- paste0(hapId, "_", taxon)

            stringIdx <- stringIdx + 1
        }
    }

    return(myStringSet)
}



## ----
#' Get specific sequences (by ref range) ----
getSequencesByRefRange <- function(
    phgObj,
    refRanges = NULL,
    gRanges = NULL
) {
    if (class(phgObj) != "PHGDataSet") {
        stop("Data object must of class PHGDataSet")
    }
    jPhgObj <- S4Vectors::metadata(phgObj)$jObj

    if (is.null(refRanges) && is.null(gRanges)) {
        stop("Need to specify either specific ref ranges or a GRanges object")
    }

    # if (!is.numeric(refRanges) || !is.null(refRanges)) {
    #     stop("Ref range values must be numeric.")
    # }

    if (is.null(gRanges)) {
        refRanges <- as.integer(refRanges)
    } else {
        if (class(gRanges) != "GRanges") {
            stop("gRanges parameter needs to be a GRanges object")
        }
        expectRanges <- rowRanges(phgObj)
        validRows <- unique(
            S4Vectors::subjectHits(
                IRanges::findOverlaps(
                    query = testRanges,
                    subject = expectRanges
                )
            )
        )
        if (length(validRows) == 0) {
            stop("No data returned")
        }
        filterRanges <- expectRanges[validRows]
        refRanges <- as.integer(gsub("R", "", filterRanges$refRange_id))
        # print(refRanges)
    }

    # Get Java method
    rJC <- rJava::.jnew("net/maizegenetics/pangenome/pipelineTests/GenerateRForPHG")
    seqRes <- rJC$graphToHapsInRefRangeVectors(
        jPhgObj, rJava::.jarray(refRanges),
        TRUE,
        FALSE
    )

    # Return Biostrings data
    if (length(seqRes$hapIds) == 0) {
        message("No sequences found for given parameters.")
        return(NULL)
    } else {
        myStrings <- Biostrings::DNAStringSet(seqRes$sequence)
        names(myStrings) <- seqRes$hapIds
        return(myStrings)
    }
}



## ----
#' Get specific sequences (by hap ID) ----
getSequencesByHapId <- function(
    phgObj,
    hapId
) {
    if (class(phgObj) != "PHGDataSet") {
        stop("Data object must of class PHGDataSet")
    }
    jPhgObj <- S4Vectors::metadata(phgObj)$jObj

    if (!is.numeric(hapId)) {
        stop("Haplotype ID values must be numeric.")
    }
    hapId <- as.integer(hapId)

    rJC <- rJava::.jnew("net/maizegenetics/pangenome/api/RPHGMethodsKotlin")
    seqRes <- rJC$getHapIdSeq(jPhgObj, .jarray(hapId))

    if (seqRes$size() == 0) {
        message("No sequences found for given parameters.")
        return(NULL)
    } else {
        # @TODO - get method to parse data class...
        # @TODO - vectorize this on the JVM end...
        myStringSet <- Biostrings::DNAStringSet()
        for (i in seq_len(seqRes$size())) {
            tmpRow <- seqRes$get(as.integer(i - 1))
            myStringSet[[i]] <- tmpRow$getSequence()
            names(myStringSet)[i] <- tmpRow$getHapId()
        }
        return(myStringSet)
    }
}


## TESTS ----

### Ref ranges
rrSeqTest <- getSequencesByRefRange(myPhg, c(1, 4))
print(rrSeqTest)

### Ref ranges with GRanges objects
expectRanges <- rowRanges(myPhg)
testRanges <- GRanges(
    seqnames = c("1", "1"),
    ranges = IRanges(start = c(1, 2500), end = c(2000, 15000))
)
getSequencesByRefRange(myPhg, gRanges = testRanges)

### Hap IDs
hapSeqTest <- getSequencesByHapId(myPhg, 31)
print(hapSeqTest)


