ftrcalc <-
function (stg, hy = hy.default()) 
{
    if (hy <= 2001) {
        a <- 68.349
        b <- 1.677
        return(a * stg^b)
    }
    else {
        a <- 39.217
        b <- 34.423
        c <- -8.5
        return(a * stg^2 + b * stg + c)
    }
}
