turbqmle <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    sam <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    if (missing(dumps) && missing(bottles)) {
        sam <- subtime(sam, sdate2, stime2, edate2, etime2)
    }
    else if (missing(bottles)) {
        sam <- sam[allmatches(dumps, sam$dump), ]
    }
    else {
        sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump, 
            sam$bottle)), ]
    }
    if (exclude & !is.null(sam$exclude)) 
        sam <- sam[!sam$exclude, ]
    print(sam)
    if (any(is.na(sam$ssc) | is.na(sam$turb))) {
        if (all(is.na(sam$ssc) | is.na(sam$turb))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$turb <= 0)) 
        stop("Zero or negative value in sample turbidity.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("Sorry, you can't use this method where turbidity values are missing")
    if (any(pop$turb == 0)) {
        print("Zero turbidities found in flo data: changed to 1 NTU")
        pop$turb[pop$turb == 0] <- 1
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    result <- logxy.qmle(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, 
        interval, var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "qmle", meth = 1, 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "qmle")
}
