weightedmean.se <-
function (fit, x, x0, y) 
{
    n <- length(x)
    xbar <- mean(x)
    xssq <- sum((x - xbar)^2)
    s <- summary(fit)$sigma
    term <- 1/n + ((x0 - xbar)^2)/xssq
    halfwidth <- s * sqrt(term)
    sum(y * halfwidth)/sum(y)
}
