bands <-
function (fit, x, log = F, lty = 2, col = 1, type = "conf", conf = 0.95) 
{
    n <- length(x)
    xmin <- range(x)[1]
    xmax <- range(x)[2]
    xpts <- seq(xmin, xmax, length = 20)
    if (log) {
        xbar <- mean(log(x))
        xssq <- sum((log(x) - xbar)^2)
        logxpts <- log(xpts)
    }
    else {
        xbar <- mean(x)
        xssq <- sum((x - xbar)^2)
        logxpts <- xpts
    }
    s <- summary(fit)$sigma
    df <- n - 2
    p <- 1 - 0.5 * (1 - conf)
    t <- qt(p, df)
    term <- 1/n + ((logxpts - xbar)^2)/xssq
    if (type == "pred") 
        term <- 1 + term
    halfwidth <- t * s * sqrt(term)
    a <- coef(fit)[1]
    b <- coef(fit)[2]
    yhat <- a + b * logxpts
    lower <- yhat - halfwidth
    upper <- yhat + halfwidth
    if (log) {
        lower <- exp(lower)
        upper <- exp(upper)
    }
    lines(xpts, lower, lty = lty, col = col, err = -1)
    lines(xpts, upper, lty = lty, col = col, err = -1)
}
