read.stages <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
    abs.path <- paste(ttshome, stn, file, sep = "\\")
    print(abs.path)
    columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "corstg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    data <- read.table(abs.path, strip.white = T, sep = ",")[, 
        1:length(columns)]
    names(data) <- columns
    data$chr <- make.chr(data$year, data$mo, data$dy, data$time)
    find.factors(data[, c("chr", "dump", "rawstg", "corstg")])
    data[, c("chr", "dump", "rawstg", "corstg")]
}
