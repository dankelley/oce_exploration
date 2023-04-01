library(oce)
debug <- 1
file <- "DPLSP000.000"
fileSize <- file.info(file)$size
N <- 1e4 # do not read whole file
buf <- readBin(file, "raw", N)
i0 <- 1
oceDebug(debug, "i0 is now ", i0, "\n")
t0 <- Sys.time()
if (buf[i0] == 0x7f && buf[i0 + 1] == 0x79) {
    # See table 16
    oceDebug(debug, "0x7f 0x79 at byte ", paste(i0 + 1:2, collapse=","), "\n")
    oceDebug(debug, vectorShow(buf[i0+0:10], n=11))
    checksumOffset <- readBin(buf[i0 + 2:3], "integer",
        size=2, n=1, signed=FALSE, endian="little")
    oceDebug(debug, "    ", vectorShow(checksumOffset))
    numberOfDataTypes <- as.integer(buf[i0 + 4])
    oceDebug(debug, "    ", vectorShow(numberOfDataTypes))
    offset <- readBin(buf[seq(i0+5, length.out=2L*numberOfDataTypes)], "integer",
        size=2, n=numberOfDataTypes, endian="little")
    buf[seq(from=i0+6, by=1, length.out=2)]
    oceDebug(debug, "    ", vectorShow(offset))
    # first leader, starts 0x03 0x01
    if (buf[i0 + 8] == 0x03 && buf[i0 + 9] == 0x01) {
        oceDebug(debug, "'first leader' header at byte ", i0+8, "\n")
        firmwareVersionMajor <- as.numeric(buf[i0 + 10])
        firmwareVersionMinor <- as.numeric(buf[i0 + 11])
        oceDebug(debug, "    ", vectorShow(firmwareVersionMajor))
        oceDebug(debug, "    ", vectorShow(firmwareVersionMinor))
        configuration <- readBin(buf[i0 + 12:13], "integer", size=2, endian="little")
        oceDebug(debug, "    ", vectorShow(configuration, postscript="(bit field)"))
        Nbins <- as.integer(buf[i0 + 14])
        oceDebug(debug, "    ", vectorShow(Nbins))
        WaveRecPings <- readBin(buf[i0 + 15:16], "integer", size=2, endian="little")
        oceDebug(debug, "    ", vectorShow(WaveRecPings))
        BinLength <- 0.01*readBin(buf[i0 + 17:18], "integer", size=2, endian="little")
        oceDebug(debug, "    ", vectorShow(BinLength, postscript="cm"))
        TBP <- 0.01 * readBin(buf[i0 + 19:20], "integer", size=2, endian="little")
        oceDebug(debug, "    ", vectorShow(TBP, postscript="s"))
        TBB <- readBin(buf[i0 + 21:22], "integer", size=2, endian="little")
        oceDebug(debug, "    ", vectorShow(TBB, postscript="s"))
        DistMidBin1 <- readBin(buf[i0 + 23:24], "integer", size=2, endian="little")
        oceDebug(debug, "    ", vectorShow(DistMidBin1, postscript="cm"))
        BinsOut <- as.integer(buf[i0 + 25])
        oceDebug(debug, "    ", vectorShow(BinsOut))
        oceDebug(debug, "    ", "NEXT: read more in table 17 on p 60\n")
    }
}

chunk <- 1
while (TRUE) {
    i0 <- i0 + 2 + checksumOffset
    checksumOffset <- readBin(buf[i0 + 2:3], "integer", size=2, signed=FALSE, endian="little")
    if (0 == length(checksumOffset) || checksumOffset==0) {
        t1 <- Sys.time()
        stop("EOF at i0=", i0, " time=", as.numeric(t1)-as.numeric(t0), "s")
    }
    oceDebug(debug, "chunk ", chunk, ", i0 ", i0, ", buf 0x", paste(buf[i0 + 0:3], collapse=","),
        ", checksumOffset ",checksumOffset,"\n")
    chunk <- chunk + 1
}

