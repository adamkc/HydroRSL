parse.format <-
function (format, year.abb = getOption("chron.year.abb"), ...) 
{
    abb <- TRUE
    mon.abb <- FALSE
    if (is.null(year.abb)) 
        year.abb <- TRUE
    if ((nf <- nchar(format)) == 5) {
        sep <- substring(format, 2, 2)
        fmt <- substring(format, first = c(1, 3, 5), last = c(1, 
            3, 5))
    }
    else if (nf == 3) {
        sep <- ""
        fmt <- substring(format, first = 1:3, last = 1:3)
    }
    else {
        abb <- FALSE
        sep <- " "
        fmt <- unlist(unpaste(format, sep = sep))
        mon.abb <- if (any(fmt == "month")) 
            FALSE
        else TRUE
    }
    periods <- substring(tolower(fmt), 1, 1)
    return(list(abb = abb, sep = sep, periods = periods, mon.abb = mon.abb, 
        year.abb = year.abb))
}
