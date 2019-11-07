finney <-
function (m, z) 
{
    N <- length(z)
    if (length(m) == 1) 
        m <- rep(m, N)
    result <- rep(1, N)
    goodones <- (abs(z) < 50) & (m > 0)
    if (sum(goodones) > 0) 
        result[goodones] <- finney2(m[goodones], z[goodones])
    result
}
