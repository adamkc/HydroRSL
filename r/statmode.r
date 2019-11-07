statmode <-
function (x) 
{
    xtab <- table(x)
    xmax <- which(xtab == max(xtab))
    xmode <- names(xmax)
    if (is.numeric(x)) 
        as.numeric(xmode)
    else xmode
}
