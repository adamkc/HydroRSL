sumdays <-
function (months, leap) 
{
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if (leap) 
        days[2] <- 29
    sum(days[months])
}
