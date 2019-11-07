turbloess <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", span = 1, degree = 1, comp = F, opt = c(0, 
        0), oldcomp = F) 
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
    fit1 <- loess(ssc ~ turb, data = sam, span = span, degree = degree, 
        family = "gaussian", model = T)
    pred1 <- predict(fit1, newdata = pop, se = T)
    ypred <- pred1$fit
    sepred <- pred1$se
    xypoints <- loess.smooth(sam$turb, sam$ssc, span = span, 
        degree = degree, family = "gaussian")
    xy <- data.frame(xypoints)
    if (any(is.na(ypred))) {
        np <- length(xy$x)
        lofit <- lm(y ~ x, data = xy[1:4, ])
        hifit <- lm(y ~ x, data = xy[(np - 3):np, ])
        lows <- (pop$turb < xy$x[1])
        highs <- (pop$turb > xy$x[np])
        se.grid <- predict(fit1, newdata = data.frame(turb = xy$x), 
            se = T)$se
        if (any(lows)) {
            ypred[lows] <- predict(lofit, newdata = data.frame(x = pop$turb[lows]))
            sepred[lows] <- se.grid[1]
        }
        if (any(highs)) {
            ypred[highs] <- predict(hifit, newdata = data.frame(x = pop$turb[highs]))
            sepred[highs] <- se.grid[np]
        }
        if (any(is.na(ypred))) 
            stop("Extrapolation failed to eliminate missing values")
        ymin <- predict(lofit, newdata = data.frame(x = 0))
        ymax <- predict(hifit, newdata = data.frame(x = 2 * max(pop$turb)))
        xypoints$x <- c(0, xypoints$x, 2 * max(pop$turb))
        xypoints$y <- c(ymin, xypoints$y, ymax)
    }
    if (comp) {
        bias <- "comp"
        resid <- sam$ssc - fitted(fit1)
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        ypred <- ypred + addback
    }
    good <- ypred > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    yhat <- sum(k * pop$q[good] * ypred[good])
    r2 <- cor(fit1$fitted, fit1$fitted + fit1$resid)^2
    s <- fit1$s
    coef <- c(NA, NA)
    cv <- NA
    if (long) 
        list(yhat = yhat, n = n, r2 = r2, s = s, cv = NA, coef = coef, 
            type = "loess", xy = xypoints, meth = 1, bias = bias, 
            chr = pop$chr, turb = pop$turb, predssc = ypred)
    else list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = NA, 
        coef = coef, type = "loess", meth = 1, bias = bias, xy = xypoints)
}
