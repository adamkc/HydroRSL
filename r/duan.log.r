duan.log <-
function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    naive <- exp(yhat)
    result <- naive * mean(exp(res))
    list(naive = naive, corrected = result)
}
