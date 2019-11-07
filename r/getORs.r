getORs <-
function (stn, st, en, ttshome = getTTSenv("TTSHOME")) 
{
    for (i in st:en) {
        hy <- i
        file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
        abs.path <- paste(ttshome, stn, file, sep = "/")
        print(abs.path)
        columns <- c("yr", "mth", "day", "time", "dump", "bot", 
            "botcodes", "stg", "cstg", "stgcode", "Q", "turb", 
            "cturb", "tcode", "origQ", "S_QUAL", "S_TYPE")
        qdata <- read.csv(abs.path, header = FALSE, strip.white = T, 
            sep = ",")[, 1:length(columns)]
        names(qdata) <- columns
        ndata <- qdata[qdata$S_TYPE == 1, ]
        if (nrow(ndata) > 0) {
            ndata <- cbind(ndata, NA1 = NA, NA2 = NA, NA3 = NA)
            sdata <- ndata[, c("yr", "mth", "day", "time", "dump", 
                "NA1", "NA2", "stg", "NA3")]
            if (stn == "sfc") {
                sdata <- ndata[, c("yr", "mth", "day", "time", 
                  "dump", "NA1", "NA2", "stg", "NA3")]
            }
            else {
                if (stn == "nfc" || stn == "que") {
                  sdata <- ndata[, c("yr", "mth", "day", "time", 
                    "dump", "NA1", "stg", "NA2", "NA3")]
                }
                else {
                  sdata <- ndata[, c("yr", "mth", "day", "time", 
                    "dump", "stg", "NA1", "NA2", "NA3")]
                }
            }
            sdata$day <- zfill(sdata$day, 2)
            sdata$mth <- zfill(sdata$mth, 2)
            sdata$time <- zfill(sdata$time, 4)
            fileout1 <- paste(stn, zfill(hy, 2), ".or", sep = "")
            out.path1 <- paste(ttshome, stn, fileout1, sep = "/")
            write.table(sdata, out.path1, quote = FALSE, row.names = FALSE, 
                col.names = FALSE, sep = ",")
        }
        else {
            cat(stn, " had no observer record codes listed in the streamflow file for HY ", 
                hy, sep = "")
        }
    }
}
