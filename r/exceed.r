exceed <-
function (x, plevels = c(0.001, 0.01, 0.02, 0.05, 0.1), varname = "ssc", 
    ssclevels = c(25, 50, 100, 200, 500, 1000), interval = 10, 
    months, leap) 
{
    daysperyear <- sumdays(months, leap)
    nx <- length(x)
    px <- nx/daysperyear/(1440/interval)
    if (any(px > 1)) 
        stop("Period of x exceeds specified number of days per year")
    q <- quantile(x, 1 - plevels/px)
    names(q) <- plevels
    counts <- sapply(ssclevels, function(ssc, data) sum(x > ssc), 
        data = x)
    hrs <- counts * interval/60
    names(hrs) <- ssclevels
    xlogical <- sapply(ssclevels, function(level, x) x >= level, 
        x = x)
    runlen <- apply(xlogical, 2, rle)
    max.hrs <- sapply(runlen, function(rln) {
        true.length <- rln$lengths[rln$values]
        if (length(true.length) == 0) 
            return(0)
        else return(max(rln$lengths[rln$values]) * interval/60)
    })
    names(max.hrs) <- ssclevels
    result <- list(ssc.exceeded.at.given.p = q, hours.exceeded.at.given.ssc = hrs, 
        max.continuous.hours = max.hrs)
    if (varname != "ssc") {
        name1 <- paste(varname, "exceeded.at.given.p", sep = ".")
        name2 <- paste("hours.exceeded.at.given", varname, sep = ".")
        name3 <- names(result)[3]
        names(result) <- c(name1, name2, name3)
    }
    result
}
