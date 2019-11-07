logline <-
function (x, y, ...) 
{
    fit <- lm(log(y) ~ log(x), na.action = na.omit)
    x0 <- range(x, na.rm = T)
    y0 <- exp(coef(fit)[1] + coef(fit)[2] * log(x0))
    lines(x0, y0, ...)
    fit
}
