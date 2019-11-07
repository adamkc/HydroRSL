make.chron <-
function (date, time, out = "day-mon-year", origin) 
{
    if (missing(origin)) 
        origin <- options()$chron.origin
    ymd2date(date, out = out, origin = origin) + mt2msm(time)/1440
}
