get.thresh <-
function (tstart, tmax, n, f = sqrt, finv = function(x) x^2) 
{
    dx <- (f(tmax) - f(tstart))/(n - 1)
    x <- seq(from = f(tstart), by = dx, length = n)
    round(finv(x))
}
