write.sed <-
function (file, stn, hy, path = paste("H:/DataManagement/CASPARCREEK/Sediment/SuspendedSediment/Data/ProcessedData/WriteSed", stn, sep = "/")) 
{
# Writes a neatly formatted text file from the annual sed object"                                                                             
# Inserts rawcodes, stage, and stage code from .flo object"                                                                                   
# Output the year with 4 digits instead of the default of 2"
# Modified 14DEC2018: path changed to HYDRO file system, no longer to website_image 
    options(chron.year.abb = FALSE)
    on.exit(options(chron.year.abb = TRUE))
    hy <- zfill(hy, 2)
    seddata <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    flodata <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    row.names(flodata) <- format(flodata$chr)
    date <- format(dates(seddata$chr))
    time <- format.times(seddata$chr - trunc(seddata$chr))
    time <- substring(time, 1, 5)
    dump <- zfill(seddata$dump, 2)
    bot <- zfill(seddata$bottle, 2)
    key <- format(seddata$chr)
    rawcode <- flodata[key, "codes"]
    stg <- format(flodata[key, "stg"], nsmall = 3, width = 6)
    stgcode <- flodata[key, "stgcode"]
    q <- q0 <- seddata$q
    q[is.na(q0)] <- "        NA"
    q[!is.na(q0)] <- scientific(q0[!is.na(q0)], 3, 2)
    turb <- t0 <- seddata$turb
    turb[is.na(t0)] <- "  NA"
    turb[!is.na(t0)] <- zfill(t0[!is.na(t0)], 4)
    turbcode <- seddata$turbcode
    ssc <- scientific(seddata$ssc, 3, 2)
    labcodes <- paste(seddata$labcode1, seddata$labcode2, sep = "")
    outdata <- data.frame(date, time, dump, bot, rawcode, stg, 
        stgcode, q, turb, turbcode, ssc, labcodes)
    col.names <- c("DATE", "TIME", "DUMP", "BOT", "RAWCODE", 
        "STG", "STGCODE", "Q", "TURB", "TURBCODE", "SSED", "TRK/LABCODE")
    names(outdata) <- col.names
    outfile <- paste(path, file, sep = "/")
    if (!file.exists(path)) {
        dir.create(path)
        if (!file.exists(path)) 
            dir.create(path, recursive = T)
        if (!file.exists(path)) 
            stop(paste("Unable to create", path))
        else cat("Created folder", path, "\n")
    }
    else if (file.exists(outfile)) 
        cat("Rewriting file", outfile, "\n")
    else cat("Writing sample data to", outfile, "\n")
    write.table(outdata, outfile, col.names = T, row.names = F, 
        quote = FALSE)
}
