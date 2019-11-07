hydro.year <-
function (chr, start = "Aug") 
{
    yr <- fac2num(years(chr))
    ifelse(months(chr) < start, yr, yr + 1)
}
