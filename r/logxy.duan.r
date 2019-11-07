logxy.duan <-
function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
{
    k <- 0.06 * interval
    xsample <- log(xsam)
    ysample <- log(ysam)
    n <- length(ysample)
    N <- length(qpop)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysample
    yhat <- x1 %*% betahat
    resid <- ysample - yhat
    rsquare <- (cor(ysample, yhat))^2
    if (n == 2) 
        s2 <- 0
    else s2 <- sum(resid^2)/(n - 2)
    x2 <- cbind(rep(1, N), log(xpop))
    if (var) {
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Covariance matrix too large: not computed")
            var <- F
        }
    }
    if (var) {
        V.diag <- diag(V)
        tmp1 <- matrix(V.diag, ncol = N, nrow = N)
        tmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)
        newyhat <- drop(x2 %*% betahat)
        tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
        tmp4 <- tmp3 * tmp2 * t(tmp3)
        Vsam <- x1 %*% invxx %*% t(x1)
        Vsam.diag <- diag(Vsam)
        tmp5 <- (1 - Vsam.diag) * s2/2
        ebcf <- mean(exp(tmp5))
        tmp6 <- matrix(Vsam.diag, ncol = n, nrow = n)
        tmp7 <- exp((2 - 2 * Vsam - tmp6 - t(tmp6)) * s2/2)
        diag(tmp7) <- 0
        ebcf2 <- (sum(exp(4 * tmp5)) + sum(tmp7))/(n * n)
        EXY <- tmp4 * ebcf2
        murc <- exp(newyhat + (V.diag * s2/2))
        musmear <- murc * ebcf
        muumve <- exp(newyhat + s2/2)
        predssc <- exp(newyhat) * mean(exp(resid))
        flux <- k * sum(qpop * predssc)
        bias <- k * sum(qpop * (musmear - muumve))
        EX <- matrix(musmear, ncol = N, nrow = N)
        EY <- t(EX)
        COVXY <- EXY - EX * EY
        qmat <- k * matrix(qpop, ncol = N, nrow = N)
        COVXY2 <- qmat * COVXY * t(qmat)
        RMSE <- sqrt(sum(COVXY2) + bias^2)
    }
    else {
        newyhat <- drop(x2 %*% betahat)
        predssc <- exp(newyhat) * mean(exp(resid))
        flux <- k * sum(qpop * predssc)
        RMSE <- NA
    }
    list(predssc = predssc, est.load = flux, rsquare = rsquare, 
        betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
}
