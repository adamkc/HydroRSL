subtime <-
function (frame, sdate, stime, edate, etime) 
{
    if (missing(sdate)) {
        schron <- min(frame$chr)
    }
    else {
        if (missing(stime)) 
            stime <- 0
        schron <- make.chron(sdate, stime)
    }
    if (missing(edate)) {
        echron <- max(frame$chr)
    }
    else {
        if (missing(etime)) 
            etime <- 2400
        echron <- make.chron(edate, etime)
    }
    keepers <- (frame$chr >= schron & frame$chr <= echron)
    if (sum(keepers) == 0) 
        print("No records found between specified dates")
    frame[keepers, ]
}
