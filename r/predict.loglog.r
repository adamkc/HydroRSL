predict.loglog <-
function (x, y, xnew, ...) 
{
    fit <- lm(log(y) ~ log(x), na.action = na.omit, ...)
    pred <- predict(fit, newdata = data.frame(x = xnew))
    sigma <- summary(fit)$sigma
    if (is.na(sigma)) 
        exp(pred)
    else exp(0.5 * sigma^2 + pred)
}
