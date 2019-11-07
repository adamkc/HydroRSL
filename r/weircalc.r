weircalc <-
function (s) 
{
    a <- 4.33
    b <- 2.5
    q <- numeric(length(s))
    s1 <- s[s <= 2]
    q[s <= 2] <- a * s1^b
    s2 <- s[s > 2]
    rd <- s2 - 2
    q[s > 2] <- (1 + 0.2489 * (rd/(s2 + 1))^2) * (61.656 - 0.668 * 
        s2) * rd^1.47 + a * (s2^b - rd^b)
    q
}
