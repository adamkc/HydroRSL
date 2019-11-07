mvue <-
function (mydata, sam = which(!is.na(mydata$ssc)), x = "turb", 
    interval = 10) 
{
    conf <- intersect(names(mydata), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in mvue:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    if (!missing(mydata)) 
        attach(mydata)
    k <- 0.06 * interval
    lnx <- log(get(x))
    y <- log(ssc)
    ysample <- y[sam]
    n <- length(ysample)
    N <- length(y)
    q <- q/35.3147
    x1 <- cbind(rep(1, n), lnx[sam])
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysample
    yhat <- x1 %*% betahat
    resid <- ysample - yhat
    rsquare <- (cor(ysample, yhat))^2
    s2 <- sum(resid^2)/(n - 2)
    x2 <- cbind(rep(1, N), lnx)
    V <- x2 %*% invxx %*% t(x2)
    tmp1 <- matrix(diag(V), ncol = N, nrow = N)
    ttmp1 <- t(tmp1)
    tmp2 <- exp(((tmp1 + ttmp1 + 2 * V) * s2)/2)
    newyhat <- drop(x2 %*% betahat)
    tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
    tmp4 <- tmp3 * tmp2 * t(tmp3)
    m <- n - 2
    tmp5 <- exp(((2 - ttmp1 - tmp1) * s2)/2)
    tmp6 <- finney(m, ((1 - tmp1) * (1 - ttmp1) * (s2^2) * (m + 
        1))/(2 * m^2))
    mvue <- k * q * exp(newyhat) * finney(m, ((1 - diag(V)) * 
        s2 * (m + 1))/(2 * m))
    EXY <- tmp4 * tmp5 * tmp6
    mumvue <- exp(newyhat + s2/2)
    EX <- matrix(mumvue, ncol = N, nrow = N)
    EY <- t(EX)
    COVXY <- EXY - EX * EY
    qmat <- k * matrix(q, ncol = N, nrow = N)
    COVXY2 <- qmat * COVXY * t(qmat)
    MSE <- sum(COVXY2)
    if (!missing(mydata)) 
        detach(2)
    list(est.load = sum(mvue), betahat = betahat, rsquare = rsquare, 
        est.rmse = sqrt(MSE))
}
