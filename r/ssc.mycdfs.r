ssc.mycdfs <-
function (xlist, title, months, log = "x", zero = 0.1, leap = F, 
    interval = 10, grid = "days", yvar = "SSC", ...) 
{
    if (!is.list(xlist)) 
        ssc.mycdf(xlist, title, log, zero, leap, interval, ...)
    y1lab <- paste("Proportion exceeding given", yvar)
    y2lab <- paste("Days exceeding given", yvar)
    par(mar = c(5, 4, 4, 4) + 0.1, mgp = c(2.5, 0.75, 0))
    nxx <- sapply(xlist, length)
    daysperyear <- sumdays(months, leap)
    pxx <- nxx/daysperyear/(1440/interval)
    if (any(pxx > 1)) 
        stop("Do not run with more than one year of data")
    xmin <- min(sapply(xlist, min))
    xmax <- max(sapply(xlist, max))
    if (log == "x") 
        xmin <- max(zero, xmin)
    if (yvar == "SSC") 
        xlab <- "SSC (mg/L)"
    else if (yvar == "turbidity") 
        xlab <- "turbidity (NTU)"
    else xlab <- yvar
    plot(c(xmin, xmax), c(0, max(pxx)), type = "n", xlab = xlab, 
        ylab = y1lab, log = log, ...)
    title(title)
    y2range <- daysperyear * par()$usr[3:4]
    y2ticlab <- pretty(y2range)
    y2tic <- y2ticlab/daysperyear
    axis(4, at = y2tic, label = y2ticlab, las = 2)
    mtext(side = 4, outer = T, at = 0.5, text = y2lab, line = -1.75)
    if (grid == "days") {
        axis(1, tck = 1, fg = "gray")
        axis(4, tck = 1, fg = "gray", at = y2tic, label = F)
    }
    else if (grid == "pro") {
        axis(1, tck = 1, fg = "gray")
        axis(2, tck = 1, fg = "gray")
    }
    box()
    xnames <- names(xlist)
    for (i in 1:length(xlist)) {
        x <- xlist[[i]]
        nx <- nxx[i]
        px <- pxx[i]
        xname <- xnames[i]
        if (log == "x") {
            if (any(x <= 0)) {
                if (any(x < 0)) 
                  cat(sum(x < 0), "negative values of", xname, 
                    "replaced by", zero, "\n")
                if (any(x == 0)) 
                  cat(sum(x == 0), "zero values of", xname, "replaced by", 
                    zero, "\n")
                x[x <= 0] <- zero
            }
        }
        xsort <- sort(x)
        y <- (1 - (1:nx)/nx) * px
        lines(xsort, y, col = i)
    }
    legend(locator(1), xnames, lty = 1, col = 1:length(xnames))
}
