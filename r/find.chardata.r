find.chardata <-
function (x) 
{
    lev <- levels(x)
    lev <- lev[lev != ""]
    oldopts <- options(warn = -1)
    lev.indices <- which.na(as.numeric(lev))
    options(warn = oldopts$warn)
    chardata <- lev[lev.indices]
    x.indices <- which(x %in% chardata)
    list(values = chardata, indexes = x.indices)
}
