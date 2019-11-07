crit.loess <-
function (h, x, y, degree = 2) 
{
    n <- length(y)
    l <- loess(y ~ x, span = h, degree = degree)
    df <- l$trace.hat
    if (df >= n - 2) 
        Inf
    else log(sum((predict(l, x) - y)^2)/n) + (1 + df/n)/(1 - 
        df/n - 2/n)
}
