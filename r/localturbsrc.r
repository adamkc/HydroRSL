localturbsrc <-
function (stn, hy, sdate, stime, edate, etime, interval = 10, 
    opt = 1, long = T, adj = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    S <- toupper(substring(stn, 1, 2))
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    pop <- subtime(pop, sdate, stime, edate, etime)
    samples <- subtime(samples, sdate, stime, edate, etime)
    mergesam <- samples[, c("chr", "ssc")]
    mergepop <- merge(pop, mergesam, all.x = T)
    mergepop <- mergepop[order(mergepop$chr), ]
    conf <- intersect(names(mergepop), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in localturbsrc:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    attach(mergepop)
    if (adj) 
        ssc <- exp(discoef[S, "a"]) * ssc^discoef[S, "b"]
    N <- dim(mergepop)[1]
    turbsav <- turb
    index <- 1:N
    sam <- index[-which.na(ssc)]
    diffs <- diff(c(0, sam, N))
    n <- length(sam)
    low1 <- rep(c(NA, sam), diffs)
    low2 <- rep(c(NA, NA, sam[-n]), diffs)
    high1 <- rep(c(sam, NA), diffs)
    high2 <- rep(c(sam[-1], NA, NA), diffs)
    x <- matrix(turb[c(low2, low1, high1, high2)], ncol = 4)
    y <- matrix(ssc[c(low2, low1, high1, high2)], ncol = 4)
    if (opt == 1) {
        x[is.na(x)] <- 0
        y[is.na(y)] <- 0
    }
    newssc <- numeric(N)
    for (i in index) {
        x1 <- x[i, ]
        y1 <- y[i, ]
        x1 <- x1[!is.na(x1)]
        y1 <- y1[!is.na(y1)]
        xy <- approx(x1, y1, xout = turb[i])
        newssc[i] <- xy$y
    }
    unfit <- is.na(newssc)
    if (sum(unfit) != 0) 
        newssc[unfit] <- predict.simple(turb[sam], ssc[sam], 
            turb[unfit])
    coef <- coef(lm(ssc[sam] ~ turb[sam]))
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    estflux <- sum((k * newssc * q)/qfactor)
    detach(2)
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            chr = mergepop$chr, turb = turbsav, predssc = newssc, 
            coef = coef, log = "", meth = 1)
    else list(yhat = estflux, n = n)
}
