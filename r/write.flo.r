write.flo <-
function (data, filename, ttshome = "C:/Users/Jack/My Documents/TTS/data") 
{
    stn <- substr(filename, 1, 3)
    filepath <- paste(ttshome, stn, filename, sep = "/")
    outdata <- data
    outdata$year <- as.numeric(as.character(years(data$chr)))
    outdata$mo <- zfill(as.numeric(months(data$chr)), 2)
    outdata$dy <- zfill(as.numeric(days(data$chr)), 2)
    outdata$mt <- zfill(msm2mt(round(1440 * (as.numeric(data$chr)%%1))), 
        4)
    outdata <- outdata[, c(12:15, 2:11)]
    write.table(outdata, filepath, quote = FALSE, row.names = F, 
        col.names = F, sep = ",")
    data
}
