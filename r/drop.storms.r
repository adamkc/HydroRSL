drop.storms <-
function (stn, hy, match = F, source = "filesystem", loc = paste(getTTSenv("TTSHOME"), 
    "/work/hy", hy, sep = "")) 
{
    hy <- zfill(hy, 2)
    data <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    if (source == "filesystem") 
        storm <- read.allstorms(stn, hy, match, loc)
    else storm <- getStormdates(stn, hy)
    keep <- rep(T, dim(data)[1])
    for (i in 1:length(storm$number)) keep[data$chr >= storm$schron[i] & 
        data$chr <= storm$echron[i]] <- F
    print("The following periods were dropped to create interstorm data:")
    print(storm)
    data <- data[keep, c("chr", "q", "turb", "turbcode")]
    attr(data, "stn") <- stn
    data
}
