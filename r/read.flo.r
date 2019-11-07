read.flo <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
    abs.path <- paste(ttshome, stn, file, sep = "/")
    columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "stg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    if (!file.exists(abs.path)) {
        print(paste(abs.path, "does not exist"))
        return(NULL)
    }
    print(paste("Reading file", abs.path))
    data <- try(read.table(abs.path, strip.white = T, sep = ",")[, 
        1:length(columns)])
    if (inherits(data, "try-error")) 
        return(data)
    names(data) <- columns
    data$chr <- make.chr(data$year, data$mo, data$dy, data$time)
    find.factors(data[, c("chr", "dump", "bottle", "stg", "stgcode", 
        "q", "turb", "turbcode")])
    data <- data[, c("chr", "dump", "bottle", "codes", "stg", 
        "stgcode", "q", "turb", "turbcode")]
    attr(data, "stn") <- stn
    data
}
