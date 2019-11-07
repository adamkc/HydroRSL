subtime2 <-
function (frame, sdate, stime, edate, etime) 
{
    sdate <- dates(as.character(sdate), format = "ymd")
    stime <- mt2msm(stime)
    edate <- dates(as.character(edate), format = "ymd")
    etime <- mt2msm(etime)
    frame$msm <- mt2msm(frame$time)
    keep1 <- frame$date > sdate | (frame$date == sdate & frame$msm >= 
        stime)
    keep2 <- frame$date < edate | (frame$date == edate & frame$msm <= 
        etime)
    if (sum(keep1) == 0 && sum(keep2) == 0) 
        print("No records found between specified dates")
    frame[keep1 & keep2, ]
}
