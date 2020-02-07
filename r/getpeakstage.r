getpeakstage <-
function (hy, stations, match = T) 
{
    allpeaks <- NULL
    allqual <- NULL
    alltimes <- NULL
    for (STN in stations) {
        stn <- tolower(STN)
        STN <- toupper(STN)
        data <- read_flo(stn, hy)
        sd <- read.allstorms(stn, hy, match)
        nstorms <- dim(sd)[[1]]
        if (nstorms == 0) 
            stop("No storm dates found for ", STN, "\n")
        peaks <- numeric(nstorms)
        qual <- numeric(nstorms)
        time <- numeric(nstorms)
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            peaks[i] <- max(data$stg[storm])
            maxima <- (data$stg[storm] == peaks[i])
            qual[i] <- min(data$stgcode[storm][maxima])
            time[i] <- min(data$chr[storm][maxima])
            if (qual[i] %in% c(5, 6, 7)) 
                peaks[i] <- -peaks[i]
        }
        cat(STN, " ", peaks, "\n")
        allpeaks <- c(allpeaks, peaks)
        allqual <- c(allqual, qual)
        alltimes <- c(alltimes, time)
    }
    if (length(allpeaks) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    peak.mtx <- matrix(allpeaks, nr = nstorms, dimnames = dn)
    qual.mtx <- matrix(allqual, nr = nstorms, dimnames = dn)
    time.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)
    list(peak = peak.mtx, qual = qual.mtx, time = chron(time.mtx))
}
