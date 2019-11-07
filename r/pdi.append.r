pdi.append <-
function (stn, hy, ttshome = ".", outpath = "K:/water/caspar/website_image/sediment/samples") 
{
    hy <- zfill(hy, 2)
    disfile <- paste(stn, hy, ".dis", sep = "")
    rawdir <- paste("raw", hy, sep = "")
    full.name <- paste(ttshome, stn, rawdir, disfile, sep = "/")
    if (!file.exists(full.name)) {
        cat("No data to append.", disfile, "does not exist.\n")
        return(invisible())
    }
    else {
        cat("Reading", full.name, "\n")
    }
    columns <- c("year", "mo", "dy", "DUMP", "BOT", "ssc", "labcodes")
    disdata <- read.csv(full.name, col.names = columns, header = F)
    sedfile <- paste(stn, "sed", hy, ".txt", sep = "")
    sedpath <- paste(outpath, stn, sep = "/")
    sed.name <- paste(sedpath, sedfile, sep = "/")
    if (!file.exists(sed.name)) {
        cat(sed.name, "does not exist.\nYou may need to:\n", 
            "(1) run write.sed(), or\n", "(2) rename sed file as", 
            sedfile, ", or\n", "(3) move", sedfile, "to", sedpath, 
            "\n")
        return(invisible())
    }
    else {
        cat("Reading", sed.name, "\n")
    }
    seddata <- read.table(sed.name, header = T)
    seddata.rec <- scan(sed.name, what = "", sep = ",", skip = 1)
    seddata2 <- data.frame(DUMP = seddata$DUMP, BOT = seddata$BOT, 
        REC = seddata.rec)
    mergedata <- merge(seddata2, disdata, by = c("DUMP", "BOT"))
    nmatches <- dim(mergedata)[1]
    if (nmatches == 0) {
        cat("No matching bottles found\n")
        return(invisible())
    }
    else {
        cat(nmatches, "matches found\n")
    }
    mergedata <- mergedata[order(mergedata$DUMP, mergedata$BOT), 
        ]
    dssc <- scientific(mergedata$ssc, 3, 2)
    dcodes <- zfill(mergedata$labcodes, 2)
    outdata <- data.frame(mergedata$REC, dssc, dcodes)
    pdifile <- paste(stn, "pdi.txt", sep = "")
    pdi.name <- paste(sedpath, pdifile, sep = "/")
    bckfile <- paste(stn, "pdi.bck", sep = "")
    bck.name <- paste(sedpath, bckfile, sep = "/")
    if (file.exists(pdi.name)) {
        if (file.exists(bck.name)) 
            cat("Replacing backup", bckfile, "\n")
        else cat("Creating backup", bckfile, "\n")
        file.copy(pdi.name, bck.name, overwrite = TRUE)
        pdidata <- read.table(pdi.name, header = T)
        yr <- as.numeric(substring(pdidata$DATE, 9, 10))
        mo <- as.numeric(substring(pdidata$DATE, 1, 2))
        wy <- ifelse(mo > 7, yr + 1, yr)
        nfound <- sum(wy == as.numeric(hy))
        if (nfound == 0) 
            cat("Appending to", pdi.name, "\n")
        else {
            message <- paste(pdifile, "already contains", nfound, 
                "records from", "water year", hy, ". Do you want to replace them (y/n)? ")
            ans <- readline(message)
            if (tolower(ans) == "y") {
                pdidata2 <- scan(pdi.name, what = "", sep = ",")
                oldpdi <- pdidata2[wy < as.numeric(hy)]
                write.table(oldpdi, pdi.name, col.names = F, 
                  row.names = F, quote = F, append = F)
                cat("Reappending to", pdi.name, "\n")
            }
            else {
                cat("Data not appended\n")
                return(invisible())
            }
        }
        write.table(outdata, pdi.name, col.names = F, row.names = F, 
            quote = F, append = T)
    }
    else {
        ans <- readline(paste(pdi.name, " not found.\nCreate it (y/n)? "))
        if (tolower(ans) == "y") {
            write.table(outdata, pdi.name, col.names = F, row.names = F, 
                quote = F, append = F)
            cat("Created", pdi.name, "\n")
        }
        else {
            cat("Data not appended\n")
            return(invisible())
        }
    }
    outdata
}
