bytesH <- as.raw(c(0x7f, 0x79)) # HEADER (group header)
bytesFL <- as.raw(c(0x03, 0x01)) # FIRST LEADER (time, pressure etc)
bytesW <- as.raw(c(0x03, 0x02)) # * WAVE PING
bytesLL <- as.raw(c(0x03, 0x03)) # LAST LEADER (time, pressure etc)
bytesHPR <- as.raw(c(0x03, 0x04)) # * HEADING, PITCH, ROLL

dir <- "~/Downloads/wetransfer_dplsp000-000_2023-03-20_0847"
pd <- "DPLSP000.000"

library(oce)
debug <- 0
countW <- 0
countHPR <- 0
countOTHER <- 0
countFL <- 0
countLL <- 0
countWEIRD <- 0

firstLeader <- list(burst=NULL, time=NULL, temperature=NULL)
HPR <- list(heading=NULL, pitch=NULL, roll=NULL)

file <- paste0(dir, "/", pd)
fileSize <- file.info(file)$size
N <- fileSize
N <- 10e6 # 10% of file
pb <- txtProgressBar(1, N, style=3)
buf <- readBin(file, "raw", N)

t0 <- Sys.time()
i0 <- 1
oceDebug(debug, "i0 is now ", i0, "\n")
chunk <- 1
# look at first chunk in detail
if (buf[i0] == bytesH[1] && buf[i0 + 1] == bytesH[2]) {
    # See table 16
    oceDebug(debug, "HEADER (0x7f 0x79) starting at byte ", i0, "\n")
    oceDebug(debug, vectorShow(buf[i0+0:10], n=11))
    checksumOffset <- readBin(buf[i0 + 2:3], "integer",
        size=2, n=1, signed=FALSE, endian="little")
    # skip "spare", which is 1 byte
    oceDebug(debug, "    ", vectorShow(checksumOffset))
    numberOfDataTypes <- as.integer(buf[i0 + 5])
    oceDebug(debug, "i0:", i0, "; numberOfDataTypes:", numberOfDataTypes, "\n")
    offset <- readBin(buf[seq(i0+6, length.out=2L*numberOfDataTypes)], "integer",
        size=2, n=numberOfDataTypes, endian="little")
    #?buf[seq(from=i0+6, by=1, length.out=2)]
    oceDebug(debug, "    ", vectorShow(offset))
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
    if (buf[i0 + 8] == bytesFL[1] && buf[i0 + 9] == bytesFL[2]) { # FIRST LEADER
        firmwareVersionMajor <- as.numeric(buf[i0 + 10])
        firmwareVersionMinor <- as.numeric(buf[i0 + 11])
        firmwareVersion <- paste(firmwareVersionMajor, ".", firmwareVersionMinor, sep="")
        oceDebug(debug, "    ", vectorShow(firmwareVersionMajor))
        oceDebug(debug, "    ", vectorShow(firmwareVersionMinor))
        oceDebug(debug, "    ", vectorShow(firmwareVersion))
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
    if (checksumOffset <= 0) {
        cat("\nat i0=", i0, ", chunk=", chunk, ": illegal checksumOffset=", checksumOffset, "\n")
        break
    }
    numberOfDataTypes <- as.integer(buf[i0 + 5])
    if (buf[i0 + 8] == bytesFL[1] && buf[i0 + 9] == bytesFL[2]) { # FIRST LEADER
        StartTime <- readBin(buf[i0 + 60:67], "integer", n=8, size=1) # 16 bytes
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
    if (0 == length(checksumOffset) || checksumOffset==0)
        break
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
    for (ioffset in seq_along(offset)) {
        b1 <- buf[i0 + offset[ioffset]]
        b2 <- buf[i0 + offset[ioffset] + 1L]
        if (b1 == bytesFL[1] && b2 == bytesFL[2]) {
            #message(" >> FIRST LEADER       (0x03 0x01)")
            countFL <- countFL + 1
        } else if (b1 == bytesW[1] && b2 == bytesW[2]) {
            # Table 18: ID, ping#, TimeSinceStart, Pressure, Dis2Surf, Velocity
            #message(" >> WAVE PING          (0x03 0x02) Table 18 {ID, ping#, TimeSinceStart, Pressure, Dis2Surf, Velocity}")
            countW <- countW + 1
        } else if (b1 == bytesLL[1] && b2 == bytesLL[2]) {
            #message(" >> LAST LEADER        (0x03 0x03)")
            countLL <- countLL + 1
        } else if (b1 == bytesHPR[1] && b2 == bytesHPR[2]) {
            #message(" >> HPR                (0x03 0x04) Table 20 {ID, heading, pitch, roll}")
            H <- 0.01 * readBin(buf[i0 + offset[ioffset] + 2:3], "integer", n=1, size=2, signed=TRUE, endian="little")
            P <- 0.01 * readBin(buf[i0 + offset[ioffset] + 4:5], "integer", n=1, size=2, signed=TRUE, endian="little")
            R <- 0.01 * readBin(buf[i0 + offset[ioffset] + 6:7], "integer", n=1, size=2, signed=TRUE, endian="little")
            HPR$heading <- c(HPR$heading, H)
            HPR$pitch <- c(HPR$pitch, P)
            HPR$roll <- c(HPR$roll, R)
            countHPR <- countHPR + 1
            # Table 20: heading pitch roll
        #} else if (b1 == 0x4e && b2 == 0x00) {
        #    count4e00 <- count4e00 + 1
        #    message("HUHFIXME: 0x", b1, ", 0x", b2)
        } else {
            countOTHER <- countOTHER + 1
            if (debug > 1)
                cat("OTHER: 0x", b1, ", 0x", b2, "\n", sep="")
        }
    }
    chunk <- chunk + 1
    #if (chunk == 10000) stop()
    setTxtProgressBar(pb, i0)
}
close(pb)
t1 <- Sys.time()
message("Finished in ", as.numeric(t1)-as.numeric(t0), "s\n")
firstLeader$time <- numberAsPOSIXct(firstLeader$time)
message(vectorShow(i0, showNewline=FALSE))
message(vectorShow(chunk, showNewline=FALSE))
message(vectorShow(countW, showNewline=FALSE))
message(vectorShow(countHPR, showNewline=FALSE))
message(vectorShow(countFL, showNewline=FALSE))
message(vectorShow(countLL, showNewline=FALSE))
message(vectorShow(countOTHER, showNewline=FALSE,
        postscript="(e.g.: 0x0000 0x0001 0x0002 0x0003 0x0004 0x8000)"))
cat("firstLeader: ", str(firstLeader))
cat("HPR: ", str(HPR))
save(firstLeader, HPR, file="06.rda")
