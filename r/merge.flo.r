merge.flo <-
function (stn, hy, all.lab = F, all.flo = F, na.rm = T) 
{
    lab <- eval(as.name(paste(stn, zfill(hy, 2), ".lab", sep = "")))
    flo <- eval(as.name(paste(stn, zfill(hy, 2), ".flo", sep = "")))
    flo <- flo[flo$bottle > 0, ]
    newdata <- suppressWarnings(merge(lab, flo, by = c("dump", 
        "bottle"), all.x = all.lab, all.y = all.flo))
    codes <- zfill(newdata$labcodes, 2)
    newdata$labcode1 <- substring(codes, 1, 1)
    newdata$labcode2 <- substring(codes, 2, 2)
    newdata <- newdata[, c("chr", "dump", "bottle", "ssc", "turb", 
        "turbcode", "q", "labcode1", "labcode2")]
    newdata <- newdata[order(newdata$dump, newdata$bottle), ]
    row.names(newdata) <- 1:dim(newdata)[1]
    attr(newdata, "stn") <- stn
    if (na.rm) 
        newdata[!is.na(newdata$ssc), ]
    else newdata
}
