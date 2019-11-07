read.crbasic <-
function (pathname) 
{
    basicnames <- c("TIMESTAMP", "TMSTAMP", "RECORD", "RECNBR", 
        "dumpCount", "bottle", "cumbot", "threshCode", "sampleCode", 
        "stage", "medianTurb", "waterTemp", "airTemp", "rain")
    Rnames <- c("timestamp", "timestamp", "recnum", "recnum", 
        "dump", "bottle", "cumbot", "code1", "code2", "stg", 
        "turb", "wtemp", "atemp", "rain")
    names(Rnames) <- basicnames
    firstline <- scan(pathname, what = "", sep = ",", nlines = 1)
    stn <- substring(firstline[2], 0, 3)
    headerlen <- switch(firstline[1], TOACI1 = 2, TOA5 = 4)
    headernames <- scan(pathname, skip = 1, nlines = 1, what = "", 
        sep = ",")
    message <- "Warning:"
    if (!all(headernames %in% basicnames)) {
        unfound <- headernames[!(headernames %in% basicnames)]
        message <- paste(message, "\nUnrecognized variable names found in header: ", 
            unfound)
    }
    varnames <- Rnames[headernames]
    varcounts <- count.fields(pathname, skip = headerlen, sep = ",")
    count.table <- table(varcounts)
    nvars <- length(varnames)
    if (length(count.table) > 1) {
        which.short <- which(varcounts < nvars)
        if (length(which.short) > 0) {
            message <- paste(message, "\nThe following lines are incomplete:", 
                paste(which.short + headerlen, collapse = ", "))
        }
        which.long <- which(varcounts > nvars)
        if (length(which.long) > 0) {
            message <- paste(message, "\nThe following lines have extra data:", 
                paste(which.long + headerlen, collapse = ", "))
        }
    }
    else if (nvars != as.numeric(names(count.table))) {
        message <- paste(message, "\nThe number of fields does not match the variables listed in the header line.")
    }
    minstg <- NA
    station.info <- try(read.table("labels.txt", sep = ",", fill = T, 
        header = F))
    if (inherits(station.info, "try-error")) {
        message <- paste(message, "\nUnable to find labels.txt file.")
    }
    else {
        stationlabels <- as.character(station.info[, 2])
        matches <- (tolower(stationlabels) == tolower(stn))
        if (sum(matches) == 0) 
            message <- paste(message, "\n", stn, " not found in labels.txt file.")
        if (sum(matches) > 1) 
            message <- paste(message, "\n", stn, " found multiple times in labels.txt file.")
        if (sum(matches) == 1) {
            labels <- drop(as.matrix(station.info[matches, ]))
            labels <- labels[labels != ""]
            labels <- tolower(labels)
            minstg <- labels[4]
            labels <- labels[5:length(labels)]
            if (length(labels) != length(varnames) || any(labels != 
                varnames)) 
                message <- paste(message, "\nVariables in header do not match labels.txt.")
        }
    }
    if (message != "Warning:") 
        winDialog(type = c("ok"), message)
    dump <- read.table(pathname, skip = headerlen, sep = ",", 
        col.names = varnames, fill = T)
    dump$chron <- crbasic.date(dump$timestamp)
    dump$time <- paste(substring(dump$timestamp, 12, 13), substring(dump$timestamp, 
        15, 16), sep = "")
    attr(dump, "stn") <- stn
    attr(dump, "minstg") <- as.numeric(minstg)
    if (!("bottle" %in% varnames) && ("cumbot" %in% varnames)) {
        bot.dif <- c(0, diff(dump$cumbot))
        dump$bottle <- dump$cumbot
        dump$bottle[bot.dif == 0] <- 0
    }
    dump[!is.na(dump$chron), ]
}
