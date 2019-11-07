oldttsplot <-
function (stn, hy, msc1 = NULL, tts1 = NULL, msc2 = NULL, tts2 = NULL, 
    msc3 = NULL, adj = T, number = T, units = "cumecs", split = 0.35, 
    grid1, grid2, ...) 
{
    if (units == "cfs") {
        cat("DISCHARGE WILL BE ASSUMED TO HAVE UNITS OF: ", units, 
            "\n")
        cat("If discharge is in metric, all functions such as 'turbsrc' and 'flowsrc', \n'lineartime', etc, as well as 'ttsplot', MUST be told explicitly using \nthe argument 'units=\"cumecs\"'\n")
    }
    hy <- zfill(hy, 2)
    if (!missing(tts1) & !missing(tts2)) 
        extralinetype <- T
    else extralinetype <- F
    if (missing(tts2)) {
        tts2$chr <- NA
        tts2$turb <- NA
    }
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    sam <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    S <- toupper(stn)
    fdate1 <- chron(min(tts1$chr, tts2$chr, msc1$chr, msc2$chr, 
        msc3$chr, na.rm = T))
    fdate2 <- chron(max(tts1$chr, tts2$chr, msc1$chr, msc2$chr, 
        msc3$chr, na.rm = T))
    numdate1 <- as.numeric(fdate1)
    numdate2 <- as.numeric(fdate2)
    sdate <- trunc(fdate1)
    edate <- trunc(fdate2)
    oldpar <- par(fig = c(0, 1, split, 1), mar = c(0, 4.1, 4.1, 
        4.1), mgp = c(1.75, 0.55, 0), tck = -0.012)
    on.exit(par(oldpar))
    samples <- sam[sam$chr >= fdate1 & sam$chr <= fdate2, ]
    if (!missing(tts1)) {
        if (tts1$type == "logx" || tts1$type == "logxy") {
            tts.ssc1 <- tts1$coef[1] + tts1$coef[2] * log(tts1$turb)
            tts.ssc2 <- tts1$coef[1] + tts1$coef[2] * log(tts2$turb)
            if (tts1$type == "logxy") {
                tts.ssc1 <- exp(tts.ssc1)
                tts.ssc2 <- exp(tts.ssc2)
            }
        }
        else if (tts1$type == "sqrt") {
            tts.ssc1 <- tts1$coef[1] + tts1$coef[2] * sqrt(tts1$turb)
            tts.ssc2 <- tts1$coef[1] + tts1$coef[2] * sqrt(tts2$turb)
            tts.ssc1 <- ifelse(tts.ssc1 > 0, tts.ssc1^2, -1)
            tts.ssc2 <- ifelse(tts.ssc2 > 0, tts.ssc2^2, -1)
        }
        else if (tts1$type == "power") {
            tts.ssc1 <- tts1$coef[1] * tts1$turb^tts1$coef[2]
            tts.ssc2 <- tts1$coef[1] * tts2$turb^tts1$coef[2]
        }
        else if (tts1$type == "loess") {
            tts.ssc1 <- approx(tts1$xy$x, tts1$xy$y, xout = tts1$turb, 
                rule = 2)$y
            if (missing(tts2)) 
                tts.ssc2 <- numeric(0)
            else tts.ssc2 <- approx(tts1$xy$x, tts1$xy$y, xout = tts2$turb, 
                rule = 2)$y
        }
        else {
            tts.ssc1 <- tts1$coef[1] + tts1$coef[2] * tts1$turb
            tts.ssc2 <- tts1$coef[1] + tts1$coef[2] * tts2$turb
        }
    }
    else {
        tts.ssc1 <- tts.ssc2 <- NA
    }
    truessc <- samples$ssc
    if (adj) 
        truessc <- dis.adjust(stn, truessc)
    maxssc <- max(msc1$predssc, msc2$predssc, truessc, max(tts.ssc1), 
        max(tts.ssc2), na.rm = T)
    if (length(truessc) > 1 || !is.na(truessc[1])) {
        if (number) {
            plot(samples$chr, truessc, type = "n", xlim = c(numdate1, 
                numdate2), ylim = c(0, maxssc), axes = F, xlab = "", 
                ylab = "", ...)
            text(samples$chr, truessc, samples$bottle, cex = 0.6, 
                col = 6)
        }
        else {
            plot(samples$chr, truessc, pch = 16, xlim = c(numdate1, 
                numdate2), ylim = c(0, maxssc), axes = F, xlab = "", 
                ylab = "", ...)
        }
    }
    else {
        plot(0, 0, xlim = c(numdate1, numdate2), ylim = c(0, 
            maxssc), pch = 16, axes = F, xlab = "", ylab = "", 
            ...)
    }
    if (!missing(grid1)) {
        grid(nx = grid1, ny = grid1, col = "lightgray", lty = "dotted", 
            lwd = par("lwd"), equilogs = TRUE)
    }
    axis.time(sdate, edate, side = 3, labels = F)
    axis(side = 2, las = 2)
    mtext(side = 2, line = 2.5, "SSC (mg/l)")
    box()
    indices <- NULL
    if (!missing(tts1)) {
        if (tts1$type == "logxy" || tts1$type == "pairs" || tts1$type == 
            "sqrt" || (!is.null(tts1$bias) && tts1$bias == "comp")) {
            yrange <- par()$usr[4] - par()$usr[3]
            if (max(abs(tts.ssc1 - tts1$predssc)) < 0.005 * yrange) {
                lines(tts1$chr, tts1$predssc, lty = 1, ...)
                indices <- 1
            }
            else {
                lines(tts1$chr, tts.ssc1, lty = 3, ...)
                lines(tts1$chr, tts1$predssc, lty = 2, ...)
                indices <- 2:3
            }
        }
        else {
            neg <- tts.ssc1 < 0
            tts.ssc1[neg] <- NA
            lines(tts1$chr, tts.ssc1, lty = 1, ...)
            zeroes <- rep(0, length(neg))
            zeroes[!neg] <- NA
            lines(tts1$chr, zeroes, lty = 2, ...)
            indices <- 1
        }
        if (missing(tts2)) {
            minturb <- min(tts1$turb, na.rm = T)
            maxturb <- max(tts1$turb, na.rm = T)
        }
        else {
            minturb <- min(tts1$turb, tts2$turb, na.rm = T)
            maxturb <- max(tts1$turb, tts2$turb, na.rm = T)
        }
        ticlabels <- pretty(c(minturb, maxturb))
        if (tts1$type == "logx" || tts1$type == "logxy") {
            ticlabels <- ticlabels[ticlabels > 0]
            ticlocs <- tts1$coef[1] + tts1$coef[2] * log(ticlabels)
            if (tts1$type == "logxy") 
                ticlocs <- exp(ticlocs)
        }
        else if (tts1$type == "sqrt") {
            ticlabels <- ticlabels[ticlabels > 0]
            ticlocs <- tts1$coef[1] + tts1$coef[2] * sqrt(ticlabels)
            ticlocs <- ifelse(ticlocs >= 0, ticlocs^2, NA)
        }
        else if (tts1$type == "power") {
            ticlabels <- ticlabels[ticlabels >= 0]
            ticlocs <- tts1$coef[1] * ticlabels^tts1$coef[2]
        }
        else if (tts1$type == "loess") {
            ticlabels <- ticlabels[ticlabels >= 0]
            ticlocs <- approx(tts1$xy$x, tts1$xy$y, ticlabels, 
                rule = 1)$y
        }
        else ticlocs <- tts1$coef[1] + tts1$coef[2] * ticlabels
        axis(4, at = ticlocs, labels = ticlabels)
        mtext(side = 4, line = 1.75, "Turbidity (NTU)")
        if (!missing(tts2)) {
            if (extralinetype) {
                lines(tts2$chr, tts.ssc2, lty = 3, ...)
                tts2.ssc3 <- tts2$predssc
                tts2.ssc3[tts2.ssc3 < 0] <- 0
                lines(tts2$chr, tts2.ssc3, lty = 2, ...)
                indices <- c(1, 3, 2)
            }
            else lines(tts2$chr, tts.ssc2, lty = 1, ...)
        }
    }
    labels <- c("Turb & est SSC", "Est SSC only", "Turbidity only", 
        "SSC sample")
    ltypes <- c(1, 2, 3, -1)
    mrks <- c(-1, -1, -1, 4)
    col <- c(1, 1, 1, 6)
    if (!missing(msc1)) {
        lines(msc1$chr, msc1$predssc, lty = 2, ...)
        if (!extralinetype) 
            indices <- union(indices, 2)
    }
    if (!missing(msc2)) 
        lines(msc2$chr, msc2$predssc, lty = 2, ...)
    if (!missing(msc3)) 
        lines(msc3$chr, msc3$predssc, lty = 2, ...)
    indices <- c(indices, 4)
    legend(locator(1), labels[indices], lty = ltypes[indices], 
        pch = mrks[indices], cex = 0.6, col = col[indices], ...)
    mtext(paste(S, ":", format(fdate1), " - ", format(fdate2), 
        sep = ""), line = 1.5, cex = 1.2)
    yhat <- sum(tts1$yhat, tts2$yhat, msc1$yhat, msc2$yhat, msc3$yhat, 
        na.rm = T)
    if (adj && (discoef[S, "a"] != 0 || discoef[S, "b"] != 1)) 
        mtext(paste("DIS-adjusted load =", round(yhat), "kg"), 
            line = 0.5, cex = 0.8)
    else mtext(paste("Estimated load =", round(yhat), "kg"), 
        line = 0.5, cex = 0.8)
    par(new = T, fig = c(0, 1, 0, split), mar = c(5.1, 4.1, 0, 
        4.1))
    pop <- pop[pop$chr >= fdate1 & pop$chr <= fdate2, ]
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    q <- pop$q/qfactor
    plot(pop$chr, q, type = "n", axes = F, xlab = "", ylab = "")
    lines(pop$chr, q, ...)
    axis.time(sdate, edate, m = 2.2)
    axis(side = 2, las = 2, tck = -0.012)
    box()
    mtext(side = 2, line = 2.5, "Discharge (m3/sec)")
    if (!missing(grid2)) {
        grid(nx = grid2, ny = grid2, col = "lightgray", lty = "dotted", 
            lwd = par("lwd"), equilogs = TRUE)
    }
    stagetics <- get.stagetics(pop$q, stn, hy, nticks = 4)
    if (!is.null(stagetics)) {
        axis(4, at = qcalc(stn, stagetics, hy)/qfactor, labels = stagetics)
        mtext("Stage (ft)", side = 4, line = 1.75)
    }
    yhat
}
