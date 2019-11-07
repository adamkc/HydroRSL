staffnames <-
function (stn) 
{
    ttshome <- getTTSenv("TTSHOME")
    filepath <- paste(ttshome, "stationinfo.txt", sep = "/")
    if (!file.exists(filepath)) {
        cat("stationinfo.txt not found in", ttshome, "\n")
        cat("Assuming just one staff plate exists\n")
        return("staff")
    }
    info <- scan(filepath, what = list(stn = "", area = "", units = "", 
        or1 = "", or2 = "", or3 = "", or4 = "", or5 = ""), sep = ",", 
        fill = T, na.strings = "")
    if (!(stn %in% info$stn)) {
        cat(stn, "not found in stationinfo.txt\n")
        cat("Assuming just one staff plate exists\n")
        return("staff")
    }
    info.df <- as.data.frame(info)
    info <- info.df[info.df$stn == stn, ]
    info <- info[!is.na(info)]
    info[-(1:3)]
}
