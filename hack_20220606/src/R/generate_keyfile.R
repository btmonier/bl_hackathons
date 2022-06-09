# find <path/to/file> -type f > keyfile_357.txt (SAMs)
# fine <path/to/file> -type f > keyfile_357_assemblies.txt (contigs)
# from keyfile_357.txt...

library(data.table)
library(magrittr)

## Parameters ----
params <- list(
    keyFilePath  = "../../Downloads/keyfile_357.txt",
    assemblyPath = "../../Downloads/keyfile_357_assemblies.txt",
    outPath      = "../../Downloads/keyfile_357_final.txt"
)


## Read in data ----
keyFile         <- data.table::fread(params$keyFilePath, header = FALSE)
keyFileAssembly <- data.table::fread(params$assemblyPath, header = FALSE)


## Add taxa columns ----
keyFile$id <- keyFile$V1 %>% gsub("^.*/|\\..*$", "", .)
keyFileAssembly$id <- keyFileAssembly$V1 %>% gsub("^.*/|\\..*$", "", .)


## Reorder ----
keyFile <- keyFile[, c("id", "V1")]
keyFileAssembly <- keyFileAssembly[, c("id", "V1")]


## JOIN ----
setkey(keyFile, id)
setkey(keyFileAssembly, id)

keyFileFinal <- keyFile[keyFileAssembly, nomatch = 0]


## Write to disk ----
fwrite(keyFileFinal, params$outPath, sep = "\t", col.names = FALSE)


