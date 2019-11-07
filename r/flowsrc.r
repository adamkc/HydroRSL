flowsrc <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, type = "logxy", bias = "mvue", units = "cfs", span = 1, 
    degree = 1, comp = F, opt = c(0, 0), oldcomp = F) 
{
    args <- as.list(match.call())
    for (vname in c("dumps", "bottles", "interval", "span", "degree", 
        "opt")) {
        if (vname %in% names(args) && is.character(args[[vname]])) {
            args[[vname]] <- eval(parse(text = args[[vname]]))
            assign(vname, args[[vname]])
        }
    }
    if (type != "linear" && type != "logx") {
        pass <- c("stn", "hy", "sdate1", "stime1", "edate1", 
            "etime1", "sdate2", "stime2", "edate2", "etime2", 
            "dumps", "bottles", "interval", "exclude", "long", 
            "adj", "var", "log", "units", "span", "degree", "comp", 
            "opt", "oldcomp")
        keep <- intersect(pass, names(args))
        if (type == "logxy") {
            if (comp) 
                estfunc <- "flowcomp"
            else estfunc <- paste("flow", bias, sep = "")
        }
        else estfunc <- paste("flow", type, sep = "")
        result <- do.call(estfunc, args[keep])
        return(result)
    }
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
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    popq <- pop$q/qfactor
    if (any(is.na(popq))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (type == "logx") {
        if (any(sam$q <= 0)) 
            stop("\nZero or negative value in sample discharge.  Cannot take logarithm.")
        x <- log(sam$q/qfactor)
        if (any(popq <= 0)) 
            stop("\nZero or negative discharges found in flo data.")
        else q <- log(popq)
    }
    else {
        x <- sam$q/qfactor
        q <- popq
    }
    n <- dim(sam)[1]
    bias <- NULL
    if (comp) {
        bias <- "comp"
        x1 <- cbind(rep(1, n), x)
        xx <- t(x1) %*% x1
        invxx <- solve(xx)
        tmp <- invxx %*% t(x1)
        coef <- tmp %*% sam$ssc
        poppredssc <- coef[1] + coef[2] * popq
        sampredssc <- coef[1] + coef[2] * x
        resid <- sam$ssc - sampredssc
        r2 <- cor(sam$ssc, sampredssc)^2
        s <- sqrt(sum(resid^2)/(n - 2))
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        newssc <- poppredssc + addback
        good <- newssc > 0
        if (sum(!good) > 0) 
            cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
        estflux <- sum(k * newssc[good] * q[good])
        cv <- NA
    }
    else {
        result <- usual.mle(x, sam$ssc, q, interval, var = var)
        newssc <- result$predssc
        r2 <- result$rsquare
        s <- result$s
        coef <- result$betahat
        estflux <- result$est.load
        cv <- (100 * result$est.rmse)/estflux
    }
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = type, meth = 2, bias = bias, 
            chr = pop$chr, predssc = newssc)
    else list(yhat = estflux, n = dim(sam)[1], r2 = r2, s = s, 
        cv = cv, coef = coef, type = type, meth = 2, bias = bias)
}
