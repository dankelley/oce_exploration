#0x7f79 = HEADER
#0x0301 = FIRST LEADER
#0x0302 = wave ping
#0x0304 = heading, pitch, roll

dir <- "~/Downloads/wetransfer_dplsp000-000_2023-03-20_0847"
pd <- "DPLSP000.000"

library(oce)
debug <- 0
d <- function(...) if (debug > 0) cat(..., sep="")
firstLeader <- list(burst=NULL, time=NULL, temperature=NULL)
if (!exists("buf")) {
    file <- paste0(dir, "/", pd)
    fileSize <- file.info(file)$size
    N <- fileSize
    #N <- 800 # do not read whole file
    buf <- readBin(file, "raw", N)
}
t0 <- Sys.time()
i0 <- 1
d("i0 is now ", i0, "\n")
chunk <- 1
if (buf[i0] == 0x7f && buf[i0 + 1] == 0x79) {
    # See table 16
    d("HEADER (0x7f 0x79) starting at byte ", i0, "\n")
    d(vectorShow(buf[i0+0:10], n=11))
    checksumOffset <- readBin(buf[i0 + 2:3], "integer",
        size=2, n=1, signed=FALSE, endian="little")
    # skip "spare", which is 1 byte
    d("    ", vectorShow(checksumOffset))
    numberOfDataTypes <- as.integer(buf[i0 + 5])
    d("i0:", i0, "; numberOfDataTypes:", numberOfDataTypes, "\n")
    offset <- readBin(buf[seq(i0+6, length.out=2L*numberOfDataTypes)], "integer",
        size=2, n=numberOfDataTypes, endian="little")
    #?buf[seq(from=i0+6, by=1, length.out=2)]
    d("    ", vectorShow(offset))
    if (debug > 0) {
        cat("chunk:", chunk, "; i0:", i0, "; buf:0x", paste(buf[i0 + 0:3], collapse=","),
            "; checksumOffset:",checksumOffset,"; numberOfDataTypes:", numberOfDataTypes, " (", sep="")
        for (ioffset in seq_along(offset)) {
            if (ioffset > 1) cat(" 0x") else cat("0x")
            cat(buf[i0+offset[ioffset]])
            cat(buf[i0+offset[ioffset]+1], sep="")
        }
        cat(")\n")
    }
    # first leader, starts 0x03 0x01
    if (buf[i0 + 8] == 0x03 && buf[i0 + 9] == 0x01) {
        firmwareVersionMajor <- as.numeric(buf[i0 + 10])
        firmwareVersionMinor <- as.numeric(buf[i0 + 11])
        firmwareVersion <- paste(firmwareVersionMajor, ".", firmwareVersionMinor, sep="")
        d("    ", vectorShow(firmwareVersionMajor))
        d("    ", vectorShow(firmwareVersionMinor))
        d("    ", vectorShow(firmwareVersion))
        configuration <- readBin(buf[i0 + 12:13], "integer", size=2, endian="little")
        d("    ", vectorShow(configuration, postscript="(bit field)"))
        Nbins <- as.integer(buf[i0 + 14])
        d("    ", vectorShow(Nbins))
        WaveRecPings <- readBin(buf[i0 + 15:16], "integer", size=2, endian="little")
        d("    ", vectorShow(WaveRecPings))
        BinLength <- 0.01*readBin(buf[i0 + 17:18], "integer", size=2, endian="little")
        d("    ", vectorShow(BinLength, postscript="cm"))
        TBP <- 0.01 * readBin(buf[i0 + 19:20], "integer", size=2, endian="little")
        d("    ", vectorShow(TBP, postscript="s"))
        TBB <- readBin(buf[i0 + 21:22], "integer", size=2, endian="little")
        d("    ", vectorShow(TBB, postscript="s"))
        DistMidBin1 <- readBin(buf[i0 + 23:24], "integer", size=2, endian="little")
        d("    ", vectorShow(DistMidBin1, postscript="cm"))
        BinsOut <- as.integer(buf[i0 + 25])
        d("    ", vectorShow(BinsOut))
        # Read other things.  I don't know the format, and I'm not using the entries,
        # but both things can be added later.  For now, this is just like skipping N
        # bytes.
        selectedData <- readBin(buf[i0 + 26:27], "integer", size=2, endian="little") # ?
        DWSBins <- readBin(buf[i0 + 28:43], "integer", n=8, size=2, endian="little") # 16 bytes
        VelBins <- readBin(buf[i0 + 44:59], "integer", n=8, size=2, endian="little") # 16 bytes
        StartTime <- readBin(buf[i0 + 60:67], "integer", n=8, size=1) # 16 bytes
        time <- ISOdatetime(100*StartTime[1] + StartTime[2], StartTime[3],
            StartTime[4], StartTime[5], StartTime[6], StartTime[7]+0.01*StartTime[8],
            tz="UTC")
        BurstNumber <- readBin(buf[i0 + 68:71], "integer", n=1, size=4, endian="little")
        # skip 8 for serial number
        Temp <- 0.01 * readBin(buf[i0 + 80:81], "integer", n=1, size=2, endian="little")
        firstLeader$burst <- c(firstLeader$burst, BurstNumber)
        firstLeader$time <- c(firstLeader$time, time)
        firstLeader$temperature <- c(firstLeader$temperature, Temp)
        if (debug > 0)
            cat("chunk=", chunk, ", type=FIRST_LEADER(0x030x01), i0+8=", i0+8, ", time=",
                format(time), ", BurstNumber=", BurstNumber, ", Temp=", Temp, "\n", sep="")

        # 2 bytes left
    }
}
chunk <- chunk + 1

while (TRUE) {
    i0 <- i0 + 2 + checksumOffset
    checksumOffset <- readBin(buf[i0 + 2:3], "integer", size=2, signed=FALSE, endian="little")
    numberOfDataTypes <- as.integer(buf[i0 + 5])
    StartTime <- readBin(buf[i0 + 60:67], "integer", n=8, size=1) # 16 bytes
    if (buf[i0 + 8] == 0x03 && buf[i0 + 9] == 0x01) {
        time <- ISOdatetime(100*StartTime[1] + StartTime[2], StartTime[3],
            StartTime[4], StartTime[5], StartTime[6], StartTime[7]+0.01*StartTime[8],
            tz="UTC")
        BurstNumber <- readBin(buf[i0 + 68:71], "integer", n=1, size=4, endian="little")
        Temp <- 0.01 * readBin(buf[i0 + 80:81], "integer", n=1, size=2, endian="little")
        firstLeader$burst <- c(firstLeader$burst, BurstNumber)
        firstLeader$temperature <- c(firstLeader$temperature, Temp)
        firstLeader$time <- c(firstLeader$time, time)
        if (debug > 0) {
            cat("chunk=", chunk, ", type=FIRST_LEADER(0x030x01), i0+8=", i0+8, ", time=",
                format(time), ", BurstNumber=", BurstNumber, ", Temp=", Temp, "\n", sep="")
        }
    }
    offset <- readBin(buf[seq(i0+6, length.out=2L*numberOfDataTypes)], "integer",
        size=2, n=numberOfDataTypes, endian="little")
    if (0 == length(checksumOffset) || checksumOffset==0) {
        break
    } else if (debug > 0) {
        cat("chunk:", chunk, "; i0:", i0, "; buf:0x", paste(buf[i0 + 0:3], collapse=","),
            "; checksumOffset:",checksumOffset,"; numberOfDataTypes:", numberOfDataTypes, " (", sep="")
        for (ioffset in seq_along(offset)) {
            if (ioffset > 1) cat(" 0x") else cat("0x")
            cat(buf[i0+offset[ioffset]])
            cat(buf[i0+offset[ioffset]+1], sep="")
        }
        cat(")\n")
    }
    chunk <- chunk + 1
    #if (chunk == 10000) stop()
}
t1 <- Sys.time()
message("Finished in ", as.numeric(t1)-as.numeric(t0), "s\n")
firstLeader$time <- numberAsPOSIXct(firstLeader$time)
save(firstLeader, file="05.rda")
