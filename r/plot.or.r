plot.or <-
function (stn, hy, sdate, stime, edate, etime, dumps, xaxis = "time", 
    cor = F, reread = T, linear = F, span = 2/3, degree = 1, 
    choose = F, ...) 
{
    hy <- zfill(hy, 2)
    orname <- paste(stn, hy, ".or", sep = "")
    if (exists(orname) && (reread == F)) 
        ordata <- eval(as.name(orname))
    else ordata <- read.or(stn, hy, choose.staff = choose)
    assign(orname, ordata, env = .GlobalEnv)
    if (!missing(dumps)) {
        subdata <- ordata[ordata$dump %in% dumps, ]
        dumpeval <- paste(unique(eval(match.call()$dumps)), collapse = ",")
        subtitle <- paste("HY", hy, " dumps ", dumpeval, sep = "")
    }
    else if (missing(sdate) | missing(edate)) {
        subdata <- ordata
        subtitle <- paste("HY", hy, sep = "")
    }
    else {
        if (missing(stime)) 
            stime <- 0
        if (missing(etime)) 
            etime <- 2400
        subdata <- subtime(ordata, sdate, stime, edate, etime)
        subtitle <- paste(zfill(sdate, 6), ":", zfill(stime, 
            4), " - ", zfill(edate, 6), ":", zfill(etime, 4), 
            sep = "")
    }
    if (!linear && dim(subdata)[1] < 8) {
        print("Forcing linear trend (need at least 8 points for loess)")
        linear <- T
    }
    attach(subdata)
    on.exit(detach(subdata))
    if (cor) {
        elecstg <- corstg
        xlab <- "Corrected electronic stage"
        ylab <- "Staff reading minus corrected elec stage"
    }
    else {
        elecstg <- rawstg
        xlab <- "Raw electronic stage"
        ylab <- "Staff reading minus raw elec stage"
    }
    diff <- staff - elecstg
    if (xaxis == "time") {
        if (linear) {
            plot(chr, diff, axes = F, xlab = "", ylab = ylab, 
                ...)
            fit <- lm(diff ~ chr, na.action = na.omit)
        }
        else {
            scatter.smooth(chr, diff, axes = F, xlab = "", ylab = ylab, 
                span = span, degree = degree, ...)
            fit <- loess(diff ~ chr, span = span, degree = degree, 
                family = "symmetric")
        }
        axis.time(chr)
        axis(2)
        box()
    }
    else if (xaxis == "stage") {
        if (linear) {
            plot(elecstg, diff, xlab = xlab, ylab = ylab, ...)
            fit <- lm(diff ~ elecstg, na.action = na.omit)
        }
        else {
            scatter.smooth(elecstg, diff, xlab = xlab, ylab = ylab, 
                span = span, degree = degree, ...)
            fit <- loess(diff ~ elecstg, span = span, degree = degree)
        }
    }
    if (linear) 
        abline(fit)
    abline(0, 0, lty = 8)
    title(paste("Station ", toupper(stn), ": ", subtitle, sep = ""), 
        cex.main = 1)
    legend(locator(1), c("trendline", "y = 0"), lty = c(1, 8))
    summary(fit)
}
