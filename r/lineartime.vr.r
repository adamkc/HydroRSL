lineartime.vr <-
function (stn, hy, sdate, stime, edate, etime, interval = 10, 
    ssc1 = 0, ssc2 = 0, long = T, adj = F, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    S <- toupper(substring(stn, 1, 2))
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    pop <- subtime(pop, sdate, stime, edate, etime)
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    samples <- subtime(samples, sdate, stime, edate, etime)
    samples <- samples[!is.na(samples$ssc), ]
    sedtimes <- format(samples$chr)
    n <- dim(samples)[1]
    N <- dim(pop)[1]
    if (!(format(pop$chr[1]) %in% sedtimes)) {
        samples <- samples[c(1, 1:n), ]
        n <- n + 1
        samples[1, "chr"] <- pop$chr[1]
        samples[1, "ssc"] <- ssc1
    }
    if (!(format(pop$chr[N]) %in% sedtimes)) {
        samples <- samples[c(1:n, n), ]
        n <- n + 1
        samples[n, "chr"] <- pop$chr[N]
        samples[n, "ssc"] <- ssc2
    }
    ssc <- approx(samples$chr, samples$ssc, pop$chr)$y
    if (adj) 
        ssc <- exp(discoef[S, "a"]) * ssc^discoef[S, "b"]
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    disch <- pop$q/qfactor
    estflux <- sum(k * disch * ssc)
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            type = "linear", meth = 3, chr = pop$chr, predssc = ssc)
    else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
        type = "linear", meth = 3)
}
