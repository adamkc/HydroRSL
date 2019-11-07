scientific <-
function (x, mandigits, expdigits) 
{
    x <- formatC(x, format = "E", digits = mandigits)
    splitnum <- strsplit(x, "E")
    mantissa <- unlist(lapply(splitnum, function(x) x[1]))
    exp <- unlist(lapply(splitnum, function(x) x[2]))
    abs.exp <- substring(exp, 2, nchar(exp))
    sign.exp <- substring(exp, 1, 1)
    new.exp <- padstring(abs.exp, expdigits, "0", right = F, 
        chop = T)
    new.exp <- paste(sign.exp, new.exp, sep = "")
    paste(mantissa, new.exp, sep = "E")
}
