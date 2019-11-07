weighthalfwidth <-
function (fit, x, x0, y, intercept = TRUE) 
{
    if (data.class(x) != data.class(x0)) 
        error("x and x0 not of same data class")
    if (is.vector(x)) {
        n <- length(x)
        n0 <- length(x0)
    }
    else if (is.matrix(x)) {
        dim1 <- dim(x)
        dim0 <- dim(x0)
        if (dim1[2] != dim0[2]) 
            stop("Number of columns of x and x0 must be the same")
        n <- dim1[1]
        n0 <- dim0[1]
    }
    else stop("x must be a vector or matrix")
    if (intercept) {
        x1 <- cbind(rep(1, n), x)
        x01 <- cbind(rep(1, n0), x0)
    }
    else {
        x1 <- x
        x01 <- x0
    }
    midterm <- solve(t(x1) %*% x1)
    term <- apply(x01, 1, function(x) {
        xmat <- matrix(x)
        t(x) %*% midterm %*% x
    })
    s <- summary(fit)$sigma
    halfwidth <- s * sqrt(term)
    sum(y * halfwidth)/sum(y)
}
