aic.lm <-
function (lmfit) 
{
    if (class(lmfit) != "lm") 
        stop("Need 'lm' object")
    resids <- resid(lmfit)
    n <- length(resids)
    p <- lmfit$rank
    K <- p + 1
    sig2 <- sum(resids^2)/n
    aic <- n * log(sig2) + 2 * K
    aic.c <- aic + (2 * K * (K + 1))/(n - K - 1)
    c(aic = aic, aic.c = aic.c)
}
