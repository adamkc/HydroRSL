drop.storm2 <-
function (stn, hy, match = F, loc = paste(Sys.getenv("WATER"),"\\Sediment\\SuspendedSediment\\MethodsTesting_Analyses\\SuspendedSedLoads\\LoadEstimation\\hy",hy, sep = "")) 
{
    hy <- zfill(hy, 2)
    data <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    storm <- read.allstorms(stn, hy, match, loc)
    keep <- rep(T, dim(data)[1])
    for (i in 1:length(storm$number)) keep[data$chr >= storm$schron[i] & 
        data$chr <= storm$echron[i]] <- F
    data[keep, c("q", "stgcode", "turb", "turbcode", "codes")]
}
