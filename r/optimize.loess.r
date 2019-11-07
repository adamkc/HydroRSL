optimize.loess <-
function (xvar, yvar, min, max, by = 0.05, degree = 1) 
{
    span <- seq(min, max, by)
    aic <- sapply(span, function(s, x, y, d) crit.loess(s, x, 
        y, d), x = xvar, y = yvar, d = degree)
    span[aic == min(aic)]
}
