package net.maizegenetics.pangenome.api

import org.apache.log4j.Logger
import khttp.get
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import java.awt.PageAttributes.MediaType

/**
 * Class for holding static Kotlin methods to be used by the rPHG package.
 * The intention for this class is to:
 *   * house methods to be executed against a PHG graph object
 *   * house methods for returning data from BrAPI URLs:
 *     + URLs will be called from `BrapiCon` objects from the rPHG package
 *     + future optimization in data returns vs native R/HTTR package calls
 *   * additional helper methods that are not found in the API,
 *     but could be of use for rPHG returns.
 * @author Brandon Monier
 */
class RPHGMethodsKotlin {
    // Set up logging object ----
    private val myLogger = Logger.getLogger(RPHGMethodsKotlin::class.java)

    /**
     * Data class for storing haplotype ID / sequence strings
     */
    data class HaplotypeSequences(val hapId: Int, val sequence: String)

    /**
     * Return sequence information from haplotype ID input
     * @param phgObj A PHG object
     * @param hapIds An array of haplotype ID values as integers
     * @return       A list of `HapSequences` data objects
     * @see          HaplotypeSequences
     */
    fun getHapIdSeq(phgObj: HaplotypeGraph, hapIds: IntArray): List<HaplotypeSequences> {
        myLogger.info("Running getHapIdSeq method...")
        val hapSeqs = mutableListOf<HaplotypeSequences>()

        val allHapIds = phgObj.nodeStream().map { it.id() }

        // Check each node in stream ----
        phgObj.nodeStream().forEach { node ->
            // If node ID from stream is found in array of give user IDs add to list object
            // as HapSequence object ----
            if (hapIds.contains(node.id())) {
                myLogger.info("Node found: ${node.id()}")
                hapSeqs.add(HaplotypeSequences(node.id(), node.haplotypeSequence().sequence()))
            }
        }

        return hapSeqs
    }

    public fun getRefRangesFromBrapi(urlRefRange: String, pageSize: Int = 500): Array<Array<String?>> {
        // Pagination - dynamically append `pageSize` value to URL
        val pattern = Regex("/variants$")
        val connector = if (pattern.containsMatchIn(urlRefRange)) "?" else "&"
        val pageSizeSuffix = "${connector}pageSize=${pageSize}"

        // Get JSON object
        val jsonReferenceRange = Json.parseToJsonElement(get("${urlRefRange}${pageSizeSuffix}", timeout = 0.0).text)
        val metadata = jsonReferenceRange.jsonObject["metadata"]?.jsonObject?.get("pagination")

        val totalCount = metadata?.jsonObject?.get("totalCount").toString().toInt()
        val totalPages = metadata?.jsonObject?.get("totalPages").toString().toInt()

        // Initialize return array [chr, start, end, dbID]
        val rrArray = Array(4) { arrayOfNulls<String?>(totalCount) }
        var pageSizeInc = 0

        for (page in 0 until totalPages) {
//            if (page % 10 == 0) println("On page: $page")
            val currentURL = "$urlRefRange$pageSizeSuffix&page=$page"
            val currentJsonArray = Json.parseToJsonElement(get(currentURL, timeout = 0.0).text)
                .jsonObject["result"]
                ?.jsonObject?.get("data")
                ?.jsonArray

            for (i in 0 until currentJsonArray!!.size) {
                rrArray[0][i + pageSizeInc] = currentJsonArray[i].jsonObject["referenceName"].toString().removeSurrounding("\"")
                rrArray[1][i + pageSizeInc] = currentJsonArray[i].jsonObject["start"].toString()
                rrArray[2][i + pageSizeInc] = currentJsonArray[i].jsonObject["end"].toString()
                rrArray[3][i + pageSizeInc] = currentJsonArray[i].jsonObject["variantDbId"].toString().removeSurrounding("\"")
            }
            pageSizeInc += pageSize
        }
        return rrArray
    }

    public fun getHapIndexArrayFromBrapi(urlTable: String, pageSize: Int = 1000): Array<IntArray> {
        val pattern = Regex("/table$")
        val connector = if (pattern.containsMatchIn(urlTable)) "?" else "&"
        val pageSizeSuffix = "${connector}pageSize=${pageSize}"

        val jsonTable = Json.parseToJsonElement(get("${urlTable}${pageSizeSuffix}", timeout = 0.0).text)
        val metadata = jsonTable.jsonObject["metadata"]?.jsonObject?.get("pagination")
        val totalTaxa = jsonTable.jsonObject["result"]?.jsonObject?.get("genotypes")?.jsonArray?.get(0)?.jsonArray?.size

        val totalCount = metadata?.jsonObject?.get("totalCount").toString().toInt()
        val totalPages = metadata?.jsonObject?.get("totalPages").toString().toInt()

//        println("total count: $totalCount")
//        println("total pages: $totalPages")
//        println("page size:   $pageSize")
//        println("total taxa:  $totalTaxa")
//        println("page suffix: $urlTable$pageSizeSuffix")
//        println("--- ---- ---")

        val hapArray = Array(totalCount) { IntArray(totalTaxa!!) }
        var pageSizeInc = 0

        for (page in 0 until totalPages) {
//            if (page % 10 == 0) println("On page: $page")
            val currentURL = "$urlTable$pageSizeSuffix&page=$page"
            val currentJsonArray = Json.parseToJsonElement(get(currentURL, timeout = 0.0).text)
                .jsonObject["result"]
                ?.jsonObject?.get("genotypes")
                ?.jsonArray

            for (i in 0 until currentJsonArray!!.size) {
                for (j in 0 until totalTaxa!!) {
                    var cell = currentJsonArray[i].jsonArray[j].toString()
                        .removeSurrounding("\"")
                        .split("/")[0]

                    if (cell == ".") cell = "-1"

                    hapArray[i + pageSizeInc][j] = cell.toInt()
                }
            }
            pageSizeInc += pageSize
        }
        return hapArray
    }

    public fun getUniqueHapIdsFromVariants(urlVariant: String, pageSize: Int): MutableList<IntArray> {
        val varPattern = Regex("/variants$")
        val varConnector = if (varPattern.containsMatchIn(urlVariant)) "?" else "&"
        val varPageSizeSuffix = "${varConnector}pageSize=${pageSize}"

        val jsonVariantArray = Json.parseToJsonElement(get("${urlVariant}${varPageSizeSuffix}", timeout = 0.0).text)
            .jsonObject["result"]
            ?.jsonObject?.get("data")
            ?.jsonArray

        val metadata = Json.parseToJsonElement(get("${urlVariant}${varPageSizeSuffix}", timeout = 0.0).text)
            .jsonObject["metadata"]
            ?.jsonObject
            ?.get("pagination")

        val totalCount = metadata?.jsonObject?.get("totalCount").toString().toInt()
        val totalPages = metadata?.jsonObject?.get("totalPages").toString().toInt()


        val hapIDList = mutableListOf<IntArray>()

        for (page in 0 until totalPages) {
//            if (page % 10 == 0) println("On page: $page")
            val currentURL = "$urlVariant$varPageSizeSuffix&page=$page"
            val currentJsonArray = Json.parseToJsonElement(get(currentURL, timeout = 0.0).text)
                .jsonObject["result"]
                ?.jsonObject?.get("data")
                ?.jsonArray

            currentJsonArray?.forEach {
                val curr = it.jsonObject["alternateBases"]?.jsonArray?.map { j ->
                    j.toString().removeSurrounding("\"").toInt()
                }!!.toIntArray()
                hapIDList.add(curr)
            }
        }

        return hapIDList
    }

    public fun getHapIdArrayFromBrapi(urlTable: String, urlVariant: String, pageSize: Int = 1000): Array<IntArray> {
        val pattern = Regex("/table$")
        val connector = if (pattern.containsMatchIn(urlTable)) "?" else "&"
        val pageSizeSuffix = "${connector}pageSize=${pageSize}"

        val jsonTable = Json.parseToJsonElement(get("${urlTable}${pageSizeSuffix}", timeout = 0.0).text)
        val metadata = jsonTable.jsonObject["metadata"]?.jsonObject?.get("pagination")
        val totalTaxa = jsonTable.jsonObject["result"]?.jsonObject?.get("genotypes")?.jsonArray?.get(0)?.jsonArray?.size

        val totalCount = metadata?.jsonObject?.get("totalCount").toString().toInt()
        val totalPages = metadata?.jsonObject?.get("totalPages").toString().toInt()

        val hapIDList = getUniqueHapIdsFromVariants(urlVariant, 1000)

        val hapArray = Array(totalCount) { IntArray(totalTaxa!!) }
        var pageSizeInc = 0

        for (page in 0 until totalPages) {
//            if (page % 10 == 0) println("On page: $page")
            val currentURL = "$urlTable$pageSizeSuffix&page=$page"

            val currentJsonArray = Json.parseToJsonElement(get(currentURL, timeout = 0.0).text)
                .jsonObject["result"]
                ?.jsonObject?.get("genotypes")
                ?.jsonArray

            for (i in 0 until currentJsonArray!!.size) {
                for (j in 0 until totalTaxa!!) {
                    val cell = currentJsonArray[i].jsonArray[j].toString()
                        .removeSurrounding("\"")
                        .split("/")[0]


                    if (cell == ".") {
                        hapArray[i + pageSizeInc][j] = -1
                    } else {
                        hapArray[i + pageSizeInc][j] = hapIDList[i + pageSizeInc][cell.toInt() - 1]
                    }
                }
            }
            pageSizeInc += pageSize
        }
        return hapArray
    }
}





