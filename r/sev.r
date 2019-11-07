sev <-
function (ssc, dur, a, b, c) 
{
    result <- a + b * log(dur) + c * log(ssc)
    result[dur == 0] <- 0
    result
}
