read.lab <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".isc", sep = "")
    rawdir <- paste("raw", zfill(hy, 2), sep = "")
    full.name <- paste(ttshome, stn, rawdir, file, sep = "/")
    if (!file.exists(full.name)) {
        print(paste(full.name, "does not exist"))
        return(NULL)
    }
    print(paste("Reading file", full.name))
    data <- try(read.table(full.name, strip.white = T, sep = ","))
    if (inherits(data, "try-error")) 
        return(data)
    ncols <- dim(data)[2]
    if (ncols == 3) {
        names(data) <- c("dump", "bottle", "ssc")
        data$labcodes <- rep(0, dim(data)[1])
    }
    else if (ncols >= 4) {
        data <- data[, 1:4]
        names(data) <- c("dump", "bottle", "ssc", "labcodes")
    }
    else {
        stop("Need at least 3 columns: dump, bottle, and ssc")
    }
    find.factors(data)
    data
}