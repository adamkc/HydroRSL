cor.matrix <-
function (interval, popq, xsample, xpop, def = 0.75) 
{
    k <- 0.06 * interval
    n <- length(xsample)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    x2 <- cbind(k * popq, k * popq * xpop)
    V <- try(x2 %*% invxx %*% t(x2))
    if (inherits(V, "try-error")) {
        print("Cannot compute covariance matrix")
        print(paste("Using default correlation of", def))
        N <- length(xpop)
        defmatrix <- matrix(def, nc = N, nr = N)
        diag(defmatrix) <- 1
        return(defmatrix)
    }
    else {
        vars <- diag(V)
        sigma <- sqrt(vars)
        cormatrix <- t(V/sigma)/sigma
        return(cormatrix)
    }
}
