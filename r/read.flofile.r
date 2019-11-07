read.flofile <-
function (flopath, extravars) 
{
    basic.columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "stg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    varnames <- c(basic.columns, extravars)
    nbasicvars <- length(basic.columns)
    nextravars <- length(extravars)
    nvars <- nbasicvars + nextravars
    varcounts <- count.fields(flopath, sep = ",")
    count.table <- table(varcounts)
    counts <- as.numeric(names(count.table))
    ncols <- max(counts)
    if (length(count.table) > 1) {
        change <- min(which(varcounts != varcounts[1]))
        message <- paste("\nThe number of fields in the flo file changed at line", 
            change)
        if (ncols == nvars) {
            message <- paste(message, "\nShort lines will be padded with blank fields.")
        }
        winDialog(type = c("ok"), paste("Warning:", message))
    }
    if (ncols <= nvars) {
        data <- read.csv(flopath, strip.white = T, fill = T, 
            col.names = varnames[1:ncols])
    }
    else {
        data <- read.csv(flopath, strip.white = T, fill = T)[, 
            1:nvars]
        names(data) <- varnames
    }
    data$chron <- make.chr(data$year, data$mo, data$dy, data$time)
    if (nextravars == 0) {
        if (ncols > nvars) {
            message <- paste("\nUnidentified extra variables will be ignored.")
            winDialog(type = c("ok"), paste("Warning:", message))
        }
        return(data[, c("chron", "bottle", "stg", "q", "turb")])
    }
    else if (ncols == nvars) {
        return(data[, c("chron", "bottle", "stg", "q", "turb", 
            extravars)])
    }
    else {
        message <- "\nThe number of extra fields is inconsistent with labels.txt."
        message <- paste(message, "\nPlease correct labels.txt and try again.")
        winDialog(type = c("ok"), paste("Aborting:", message))
        stop(message)
    }
}
