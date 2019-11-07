predict.simple <-
function (x, y, xnew) 
{
    fit <- lm(y ~ x)
    predict(fit, newdata = data.frame(x = xnew))
}
