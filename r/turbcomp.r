turbcomp <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = NULL, long = T, adj = T, 
    var = T, units = "cfs", comp = T, opt = c(0, 0), oldcomp = F) 
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
    if (!(is.null(exclude))) 
        sam <- sam[!(sam$turbcode %in% exclude), ]
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
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    xsample <- log(sam$turb)
    ysample <- log(sam$ssc)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% ysample
    poppredssc <- b[1] + b[2] * log(pop$turb)
    sampredssc <- b[1] + b[2] * xsample
    resid <- ysample - sampredssc
    r2 <- cor(ysample, sampredssc)^2
    s <- sqrt(sum(resid^2)/(n - 2))
    if (comp) 
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
    else addback <- 0
    newssc <- exp(poppredssc + addback)
    good <- newssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    estflux <- sum(k * popq[good] * newssc[good])
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA, 
            coef = b, type = "logxy", meth = 1, bias = "comp", 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA, 
        coef = b, type = "logxy", meth = 1, bias = "comp")
}
