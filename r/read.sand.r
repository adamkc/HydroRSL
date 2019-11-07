read.sand <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".sand", sep = "")
    rawdir <- paste("raw", zfill(hy, 2), sep = "")
    full.name <- paste(ttshome, stn, rawdir, file, sep = "/")
    print(full.name)
    data <- read.table(full.name, strip.white = T, sep = ",")
    ncols <- dim(data)[2]
    if (ncols == 4) {
        names(data) <- c("dump", "bottle", "sand", "totssc")
        data$labcodes <- rep(0, dim(data)[1])
    }
    else if (ncols >= 5) {
        data <- data[, 1:5]
        names(data) <- c("dump", "bottle", "sand", "totssc", 
            "labcodes")
    }
    else {
        stop("Need at least 4 columns: dump, bottle, sand, and totssc")
    }
    find.factors(data)
    data
}
