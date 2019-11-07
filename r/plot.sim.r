plot.sim <-
function (df, simout, sdate, stime, edate, etime, ylim, title = "", 
    numbers = F) 
{
    if (missing(sdate)) {
        schron <- min(df$chr)
        sdate <- trunc(schron)
        stime <- msm2mt(1440 * (schron - sdate))
    }
    else {
        if (missing(stime)) 
            stime <- 0
        schron <- make.chron(sdate, stime)
        sdate <- trunc(schron)
    }
    if (missing(edate)) {
        echron <- max(df$chr)
        edate <- trunc(echron)
        etime <- msm2mt(1440 * (echron - edate))
    }
    else {
        if (missing(etime)) 
            etime <- 2400
        echron <- make.chron(edate, etime)
        edate <- trunc(echron)
    }
    row.names(df) <- 1:dim(df)[1]
    df <- df[df$chr >= schron & df$chr <= echron, ]
    nam <- as.numeric(row.names(df))
    attach(df)
    on.exit(detach(2))
    if (missing(ylim)) 
        ylim <- c(0, max(turb))
    plot(chr, turb, xlab = "", axes = F, ylim = ylim, type = "l")
    turb2 <- turb
    turb2[stg < attr(simout, "minstg")] <- NA
    lines(chr, turb2, col = 2)
    axis(2)
    axis.time(chr)
    box()
    t <- simout
    set0 <- which(nam %in% t[, 1])
    set1 <- which(nam %in% t[t[, 2] == 1, 1])
    set2 <- which(nam %in% t[t[, 2] == 2, 1])
    set4 <- which(nam %in% t[t[, 2] == 4, 1])
    set5 <- which(nam %in% t[t[, 2] == 5, 1])
    if (length(set1) > 0) 
        points(chr[set1], turb[set1], pch = 2, cex = 0.8)
    if (length(set2) > 0) 
        points(chr[set2], turb[set2], pch = 6, cex = 0.8)
    if (length(set4) > 0) 
        points(chr[set4], turb[set4], pch = 1, cex = 0.8)
    if (length(set5) > 0) 
        points(chr[set5], turb[set5], pch = 16, cex = 0.8)
    if (numbers && length(set0) > 0) {
        dx <- 0.015 * (par()$usr[2] - par()$usr[1])
        botnum <- which(t[, 1] %in% nam)
        text(chr[set0] + dx, turb[set0], botnum, cex = 0.7)
    }
    usr <- par()$usr
    x0 <- 0.87 * usr[2] + 0.13 * usr[1]
    y0 <- 0.985 * usr[4] + 0.015 * usr[3]
    legend(x0, y0, c("rising", "falling", "startup", "fixtime"), 
        pch = c(2, 6, 1, 16), cex = 0.75)
    title(title)
}
