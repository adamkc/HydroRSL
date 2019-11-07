turbsrc <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, type = "linear", bias = "mvue", units = "cfs", span = 1, 
    degree = 1, comp = F, oldcomp = F, opt = c(0, 0)) 
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
            "adj", "var", "units", "span", "degree", "comp", 
            "opt", "oldcomp")
        keep <- intersect(pass, names(args))
        if (type == "logxy") {
            if (comp) 
                estfunc <- "turbcomp"
            else estfunc <- paste("turb", bias, sep = "")
        }
        else estfunc <- paste("turb", type, sep = "")
        result <- do.call(estfunc, args[keep])
        return(result)
    }
    bias <- NULL
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
            stop("\nNo samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb), ]
        print("missing value(s) removed from sample data")
    }
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    popturb <- pop$turb
    if (any(is.na(popturb))) 
        stop("\nSorry, the specified period has missing turbidity values")
    if (type == "logx") {
        if (any(sam$turb <= 0)) 
            stop("\nZero or negative value in sample turbidity.  Cannot take logarithm.")
        sam$turb <- log(sam$turb)
        popturb <- log(popturb)
        if (any(popturb <= 0)) {
            print("\nZero or negative turbidities found in flo data.  Changed to 1 NTU.")
            popturb[popturb <= 0] <- 1
        }
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    x1 <- cbind(rep(1, n), sam$turb)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% sam$ssc
    poppredssc <- b[1] + b[2] * popturb
    sampredssc <- b[1] + b[2] * sam$turb
    resid <- sam$ssc - sampredssc
    r <- cor(sam$ssc, sampredssc)
    s2 <- sum(resid^2)/(n - 2)
    if (comp) {
        bias <- "comp"
        var <- F
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        poppredssc <- poppredssc + addback
    }
    good <- poppredssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    yhat <- sum(k * popq[good] * poppredssc[good])
    if (var) {
        x2 <- cbind(k * popq, k * popq * popturb)
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Cannot calculate covariance matrix.  Time series too long")
            var <- F
        }
    }
    if (var) {
        sumV <- sum(V)
        estvar <- s2 * sumV
        cv <- (100 * sqrt(estvar))/yhat
    }
    else cv <- NA
    if (long) 
        list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = cv, 
            coef = b, type = type, meth = 1, bias = bias, chr = pop$chr, 
            turb = pop$turb, predssc = poppredssc)
    else list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = cv, 
        coef = b, type = type, meth = 1, bias = bias)
}
