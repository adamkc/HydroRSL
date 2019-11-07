qturbplot <-
function (df, sdate, stime, edate, etime, col = T, log = "xy", 
    reg = T, units = "cfs", ...) 
{
    stn <- substring(deparse(substitute(df)), 1, 3)
    start <- dates(as.character(sdate), format = "ymd", out = "m/d/y")
    start <- start + mt2msm(stime)/1440
    end <- dates(as.character(edate), format = "ymd", out = "m/d/y")
    end <- end + mt2msm(etime)/1440
    df <- df[df$chr >= start & df$chr <= end, ]
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    df$q <- df$q/qfactor
    conf <- intersect(names(df), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in qturbplot:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    attach(df)
    plot(q, turb, type = "n", xlab = "discharge", ylab = "turb", 
        log = log, ...)
    title(paste("Station ", stn, ": ", format(start), "-", format(end), 
        sep = ""), cex = 0.8)
    if (col) {
        dumps <- unique(dump)
        for (i in seq(along = dumps)) {
            d <- dumps[i]
            text(q[dump == d], turb[dump == d], bottle[dump == 
                d], cex = 0.6, col = i)
        }
    }
    else text(q, turb, bottle, cex = 0.6)
    if (reg) {
        if (log == "xy") 
            logline(q, turb)
        else abline(lm(turb ~ q))
    }
    invisible(detach(2))
}
