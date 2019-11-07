flowsqrt <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", comp = F, opt = c(0, 0), oldcomp = F) 
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
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(sam$ssc < 0)) 
        stop("Negative value in sample ssc.  Cannot take square root.")
    if (any(sam$q < 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take square root.")
    if (any(pop$q < 0)) 
        stop("Zero or negative value in flow data.  Cannot take square root.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.3147
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop$q <- pop$q/qfactor
    sam$q <- sam$q/qfactor
    fit <- lm(sqrt(ssc) ~ sqrt(q), data = sam)
    betahat <- coef(fit)
    resid <- fit$residuals
    n <- length(resid)
    if (comp) {
        bias = "comp"
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        pred <- predict(fit, newdata = pop)
        newpred <- pred + addback
        if (any(newpred < 0)) {
            cat(sum(newpred < 0), "negative predictions were set to zero before squaring\n")
            newpred[newpred < 0] <- 0
        }
        predssc <- newpred^2
    }
    else {
        bias = "duan"
        duan <- duan.sqrt(fit, newdata = pop)
        predssc <- duan$corrected
    }
    summ <- summary(fit)
    rsquare <- summ$r.sq
    sigma <- summ$sigma
    flux <- k * pop$q * predssc
    yhat <- sum(flux)
    if (long) 
        list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, 
            coef = betahat, type = "sqrt", bias = bias, meth = 2, 
            chr = pop$chr, predssc = predssc)
    else list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, 
        coef = betahat, type = "sqrt", bias = bias, meth = 2)
}
