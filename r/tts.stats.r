tts.stats <-
function (sam, pop, interval = 10, var = T, units = "cfs") 
{
    k <- 0.06 * interval
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    x1 <- cbind(rep(1, n), sam$turb)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% sam$ssc
    poppredssc <- b[1] + b[2] * pop$turb
    good <- poppredssc >= 0
    yhat <- sum(k * popq[good] * poppredssc[good])
    sampredssc <- b[1] + b[2] * sam$turb
    resid <- sampredssc - sam$ssc
    r <- cor(sam$ssc, sampredssc)
    s2 <- sum(resid^2)/(n - 2)
    N <- length(popq)
    if (var) {
        x2 <- cbind(k * popq, k * popq * pop$turb)
        V <- x2 %*% invxx %*% t(x2)
        sumV <- sum(V)
        estvar <- s2 * sumV
        cv <- (100 * sqrt(estvar))/yhat
    }
    else cv <- NULL
    list(yhat = yhat, N = N, n = n, negs = sum(!good), coef = b, 
        r2 = r^2, s = sqrt(s2), cv = cv)
}
