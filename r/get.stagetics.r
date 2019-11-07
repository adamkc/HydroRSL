get.stagetics <-
function (q, stn, hy, nticks = 4) 
{
    stg <- 0
    min.q <- min(q)
    max.q <- max(q)
    if (all(is.na(qcalc(stn, c(1, 10, 100), hy)))) 
        return(NULL)
    while (qcalc(stn, stg, hy) < min.q) stg <- stg + 1
    min.stg <- stgcalc(min.q, stg - 1, stg + 0.1, stn, hy)
    while (qcalc(stn, stg, hy) < max.q) stg <- stg + 1
    max.stg <- stgcalc(max.q, stg - 1, stg + 0.1, stn, hy)
    if (is.na(min.stg)) 
        min.stg <- 0
    if (is.na(max.stg)) 
        return(NULL)
    pretty(c(min.stg, max.stg), n = nticks)
}
