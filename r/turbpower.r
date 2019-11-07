turbpower <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", comp = F, opt = c(0, 0), oldcomp = F) 
{
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
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb), ]
        print("missing value(s) removed from sample data")
    }
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("\nSorry, the specified period has missing turbidity values")
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop$q <- pop$q/qfactor
    n <- dim(sam)[1]
    startdata <- sam[sam$turb != 0 & sam$ssc != 0, ]
    startmodel <- lm(log(ssc) ~ log(turb), data = startdata)
    a0 <- exp(coef(startmodel)[1])
    b0 <- coef(startmodel)[2]
    fit1 <- nls(ssc ~ a * turb^b, data = sam, start = list(a = a0, 
        b = b0))
    pred1 <- predict(fit1, newdata = pop)
    yhat <- sum(k * pop$q * pred1)
    r <- cor(sam$ssc, fitted(fit1))
    resid <- sam$ssc - fitted(fit1)
    s <- summary(fit1)$sigma
    coef <- coef(fit1)
    names(coef) <- c("constant", "power")
    if (comp) {
        bias <- "comp"
        var <- F
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        pred1 <- pred1 + addback
    }
    good <- pred1 > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    yhat <- sum(k * pop$q[good] * pred1[good])
    if (var) {
        vcov <- vcov.nls(fit1)
        b0 <- coef["constant"]
        b1 <- coef["power"]
        col1 <- pop$turb^b1
        col2 <- b0 * log(pop$turb) * col1
        x2 <- k * pop$q * cbind(col1, col2)
        V <- try(x2 %*% vcov %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Cannot calculate covariance matrix.  Time series too long")
            cv <- NA
        }
        else cv <- (100 * sqrt(sum(V)))/yhat
    }
    else cv <- NA
    if (long) 
        list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef, 
            type = "power", meth = 1, bias = bias, chr = pop$chr, 
            turb = pop$turb, predssc = pred1)
    else list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef, 
        type = "power", meth = 1, bias = bias)
}
