parseDate <-
function (str) 
{
    dt <- unlist(strsplit(str, " "))
    date <- dt[1]
    time <- dt[2]
    if (nchar(time) == 5) 
        time <- paste(time, "00", sep = ":")
    chr <- suppressWarnings(try(chron(date, time), silent = TRUE))
    if (inherits(chr, "try-error")) 
        stop(paste("Invalid date format:", str))
    else return(chr)
}
