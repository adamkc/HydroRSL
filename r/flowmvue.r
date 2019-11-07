flowmvue <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    if (missing(dumps) && missing(bottles)) {
        sam <- subtime(samples, sdate2, stime2, edate2, etime2)
    }
    else if (missing(bottles)) {
        sam <- samples[allmatches(dumps, samples$dump), ]
    }
    else {
        sam <- samples[allmatches(paste(dumps, bottles), paste(samples$dump, 
            samples$bottle)), ]
    }
    if (exclude & !is.null(sam$exclude)) 
        sam <- sam[!sam$exclude, ]
    print(sam)
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$q <= 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take logarithm.")
    if (any(pop$q <= 0)) 
        stop("Zero or negative value in flow data.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    qpop <- pop$q/qfactor
    xsam <- sam$q/qfactor
    result <- logxy.mvue(xsam, sam$ssc, qpop, qpop, interval, 
        var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "mvue", meth = 2, 
            chr = pop$chr, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "mvue")
}
