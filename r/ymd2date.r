ymd2date <-
function (date, out = "m/d/y", origin = c(1, 1, 1970)) 
{
    date <- zfill(date, 6)
    chron(date, format = "ymd", out = out, origin = origin)
}
