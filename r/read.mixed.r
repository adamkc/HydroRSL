read.mixed <-
function (pathname) 
{
    library(chron)
    array.id <- zfill(scan(pathname, what = "", sep = ",", n = 1), 
        3)
    station.info <- scan("labels.txt", what = "")
    matches <- station.info[substring(station.info, 1, 3) == 
        array.id]
    if (length(matches) == 0) 
        stop("Array ID not found in labels.txt")
    if (length(matches) > 1) 
        stop("Array ID found multiple times in labels.txt")
    varnames <- unlist(strsplit(matches, ","))
    station <- varnames[2]
    minstg <- as.numeric(varnames[4])
    varnames <- varnames[5:length(varnames)]
    varcounts <- count.fields(pathname, sep = ",")
    count.table <- table(varcounts)
    count.sorted <- rev(sort(count.table))
    nvars <- as.numeric(names(count.sorted[1]))
    message <- "Warning:"
    if (length(count.table) > 1) {
        which.short <- which(varcounts < nvars)
        if (length(which.short) > 0) 
            message <- paste(message, "\nThe following lines are incomplete:", 
                paste(which.short, collapse = ", "))
        which.long <- which(varcounts > nvars)
        if (length(which.long) > 0) 
            message <- paste(message, "\nThe following lines have extra data:", 
                paste(which.long, collapse = ", "))
    }
    if (nvars != length(varnames)) {
        message <- paste(message, "\nThe number of fields does not agree with labels.txt.")
        while (nvars > length(varnames)) {
            newname <- paste("V", length(varnames) + 1, sep = "")
            varnames <- c(varnames, newname)
        }
    }
    if (message != "Warning:") 
        winDialog(type = c("ok"), message)
    dump <- read.table(pathname, sep = ",", col.names = varnames, 
        fill = T)
    dump$chron <- campbell.date(dump$year, dump$date, dump$time)
    attr(dump, "stn") <- station
    attr(dump, "minstg") <- minstg
    if (!("bottle" %in% varnames) && ("cumbot" %in% varnames)) {
        bot.dif <- c(0, diff(dump$cumbot))
        dump$bottle <- dump$cumbot
        dump$bottle[bot.dif == 0] <- 0
    }
    dump[!is.na(dump$chron), ]
}
