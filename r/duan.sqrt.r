duan.sqrt <-
function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    result <- yhat^2 + 2 * yhat * mean(res) + mean(res^2)
    result[yhat < 0] <- 0
    list(naive = yhat^2, corrected = result)
}
