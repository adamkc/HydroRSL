duan.cbrt <-
function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    result <- yhat^3 + 3 * yhat * mean(res^2)
    list(naive = yhat^3, corrected = result)
}
