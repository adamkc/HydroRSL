cubictime <-
function (stn, hy, sdate, stime, edate, etime, interval = 10, 
    ssc1 = 0, ssc2 = 0, long = T, adj = F, exclude = TRUE, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    pop <- subtime(pop, sdate, stime, edate, etime)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified segment has missing discharge values")
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    samples <- subtime(samples, sdate, stime, edate, etime)
    if (exclude & !is.null(samples$exclude)) 
        samples <- samples[!samples$exclude, ]
    mergesam <- samples[, c("chr", "ssc")]
    if (dim(samples)[1] == 1) 
        mergesam <- as.data.frame(mergesam)
    if (dim(mergesam)[1] == 0) {
        mergepop <- pop
        mergepop$ssc <- rep(NA, dim(mergepop)[1])
    }
    else {
        mergepop <- merge(pop, mergesam, all.x = T)
        mergepop <- mergepop[order(mergepop$chr), ]
    }
    conf <- intersect(names(mergepop), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in lineartime:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    attach(mergepop)
    ssc0 <- ssc
    if (adj) 
        ssc0 <- dis.adjust(stn, ssc0)
    N <- dim(mergepop)[1]
    index <- 1:N
    sam <- index[-which.na(ssc0)]
    n <- length(sam)
    x <- sam
    y <- ssc0[sam]
    if (!(1 %in% x)) {
        x <- c(1, x)
        y <- c(ssc1, y)
    }
    if (!(N %in% x)) {
        x <- c(x, N)
        y <- c(y, ssc2)
    }
    cuspline <- splinefun(x, y, method = "natural")
    predssc <- cuspline(index)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    disch <- pop$q/qfactor
    estflux <- sum(k * disch * predssc)
    detach(2)
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            type = "linear", meth = 3, chr = mergepop$chr, predssc = predssc)
    else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
        type = "linear", meth = 3)
}
