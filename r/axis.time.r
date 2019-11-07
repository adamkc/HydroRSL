axis.time <-
function (sdate, edate, side = 1, m = 1, labels = T, hours = labels) 
{
    if (missing(edate)) {
        edate <- trunc(max(sdate))
        sdate <- trunc(min(sdate))
    }
    orig <- attr(sdate, "origin")
    xlim <- par()$usr[1:2]
    ndays <- edate - sdate
    ticlocs <- seq((sdate - 1), (edate + 1))
    if (ndays <= 21) 
        ticlen <- -0.04 * m
    else ticlen <- -0.02 * m
    if (ndays < 366) 
        tick <- T
    else tick <- F
    axis(side, at = ticlocs, labels = F, tck = ticlen, tick = tick)
    if (labels & ndays < 30) {
        ticlocs <- seq((sdate - 1), edate)
        lablocs <- ticlocs + 0.5
        if (ndays <= 8) {
            mm <- zfill(as.numeric(months(ticlocs)), 2)
            dd <- zfill(as.numeric(days(ticlocs)), 2)
            yy <- substring(fac2num(years(ticlocs)), 3, 4)
            ticlabels <- paste(mm, dd, yy, sep = "/")
            textsize <- 0.9
            line <- 1.1
        }
        else if (ndays <= 15) {
            mm <- as.numeric(months(ticlocs))
            dd <- zfill(as.numeric(days(ticlocs)), 2)
            ticlabels <- paste(mm, dd, sep = "/")
            textsize <- 0.8
            line <- 1.1
        }
        else if (ndays <= 22) {
            ticlabels <- as.character(days(ticlocs))
            textsize <- 0.75
            line <- 0.9
        }
        else {
            ticlabels <- as.character(days(ticlocs))
            textsize <- 0.6
            line <- 0
        }
        in.range <- (lablocs >= xlim[1]) & (lablocs <= xlim[2])
        if (sum(in.range) > 0) 
            mtext(ticlabels[in.range], side = 1, at = ticlocs[in.range] + 
                0.5, line = line, cex = textsize)
    }
    if (ndays > 15) {
        syear <- as.numeric(as.character(years(sdate)))
        eyear <- as.numeric(as.character(years(edate)))
        span <- eyear - syear
        if (span >= 1) {
            seq1 <- seq(as.numeric(months(sdate)), 12)
            seq3 <- seq(1, as.numeric(months(edate)))
            if (span > 1) {
                seq2 <- rep(1:12, span - 1)
                rep2 <- rep(12, span - 1)
            }
            else {
                seq2 <- rep2 <- numeric(0)
            }
            mseq <- c(seq1, seq2, seq3)
            reps <- c(length(seq1), rep2, length(seq3))
            yseq <- rep(syear:eyear, reps)
        }
        else {
            mseq <- seq(as.numeric(months(sdate)), as.numeric(months(edate)))
            yseq <- c(rep(syear, length(mseq)))
        }
        yseq <- substring(yseq, 3, 4)
        ticlocs <- dates(paste(zfill(mseq, 2), 1, yseq, sep = "/"), 
            origin = orig)
        lablocs <- dates(paste(zfill(mseq, 2), 16, yseq, sep = "/"), 
            origin = orig)
        if (length(lablocs) <= 12) {
            text <- months(lablocs)
            textsize <- 1.2
        }
        else {
            text <- mseq
            textsize <- 1
        }
        in.range <- (ticlocs >= xlim[1]) & (ticlocs <= xlim[2])
        if (sum(in.range) > 0) 
            axis(side, at = ticlocs, label = F, tck = -0.04 * 
                m)
        if (ndays > 22) 
            line <- 1.7
        else line <- 2.5
        in.range <- (lablocs >= xlim[1]) & (lablocs <= xlim[2])
        if (labels && sum(in.range) > 0) 
            mtext(as.character(text[in.range]), side = 1, at = lablocs[in.range], 
                line = line, cex = textsize)
    }
    if (ndays <= 22) {
        ticlocs <- seq(as.numeric(sdate - 1), as.numeric(edate + 
            1), by = 1/4)
        if (ndays <= 21) 
            ticlen <- -0.02 * m
        else ticlen <- -0.01 * m
        axis(side, at = ticlocs, labels = F, tck = ticlen, mgp = c(3, 
            0.75, 0), cex = 0.6)
        if (ndays <= 4 && hours) {
            ticlabels <- round(24 * ticlocs%%1)
            in.range <- (ticlocs >= xlim[1]) & (ticlocs <= xlim[2])
            if (sum(in.range) > 0) 
                mtext(ticlabels[in.range], side = 1, at = ticlocs[in.range], 
                  line = -1.2, cex = 0.75)
        }
    }
    if (ndays <= 8) {
        ticlocs <- seq(as.numeric(sdate - 1), as.numeric(edate + 
            1), by = 1/24)
        axis(side, at = ticlocs, labels = F, tck = -0.01 * m)
    }
}
