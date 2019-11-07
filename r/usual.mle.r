usual.mle <-
function (xsam, ysam, qpop, var = T, interval = 10) 
{
    k <- 0.06 * interval
    n <- length(xsam)
    N <- length(qpop)
    x1 <- cbind(rep(1, n), xsam)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysam
    yhat <- x1 %*% betahat
    resid <- ysam - yhat
    rsquare <- (cor(ysam, yhat))^2
    if (n == 2) 
        s2 <- 0
    else s2 <- sum(resid^2)/(n - 2)
    poppredssc <- betahat[1] + betahat[2] * qpop
    good <- poppredssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    est.load <- sum(k * qpop[good] * poppredssc[good])
    if (var) {
        x2 <- cbind(k * qpop, k * qpop * qpop)
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Time series too long, cannot compute covariance matrix")
            var <- F
        }
    }
    if (var) {
        sumV <- sum(V)
        estvar <- s2 * sumV
        RMSE <- sqrt(estvar)
    }
    else RMSE <- NA
    list(predssc = poppredssc, est.load = est.load, rsquare = rsquare, 
        betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
}
