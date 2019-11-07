hy.default <-
function () 
{
    today <- date()
    mon <- substring(today, 5, 7)
    nc <- nchar(today)
    year <- as.numeric(substring(today, nc - 3, nc))
    ifelse(mon %in% c("Aug", "Sep", "Oct", "Nov", "Dec"), year + 
        1, year)
}
