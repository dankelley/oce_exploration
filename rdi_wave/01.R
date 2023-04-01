library(oce)
debug <- 1
file <- "DPLSP000.000"
fileSize <- file.info(file)$size
N <- 1000 # just look a bit
buf <- readBin(file, "raw", N)
i0 <- 0
if (buf[i0 + 1] == 0x7f && buf[i0 + 2] == 0x79) {
    # See table 16
    checksumOffset <- readBin(buf[i0 + 3:4], "integer", size=2, signed=FALSE, endian="little")
    oceDebug(debug, vectorShow(checksumOffset))
    numberOfDataTypes <- as.integer(buf[i0 + 6])
    oceDebug(debug, vectorShow(numberOfDataTypes))
    offset <- readBin(buf[seq(i0+7, length.out=2*255)], "integer", n=255, size=2, endian="little")
    #offset <- readBin(buf[seq(from=i0+7, by=1, length.out=2)], size=2, "integer", endian="little")
    buf[seq(from=i0+7, by=1, length.out=2)]
    oceDebug(debug, vectorShow(offset))
}

