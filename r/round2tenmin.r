round2tenmin <-
function (x, dir = "nearest") 
{
    interval <- 144 * as.numeric(x)
    if (dir == "nearest") {
        t <- round(interval)
    }
    else if (dir == "down") {
        t <- floor(interval)
    }
    else if (dir == "up") {
        t <- ceiling(interval)
    }
    else stop("dir must be 'nearest', 'up', or 'down'")
    chron(t/144)
}
