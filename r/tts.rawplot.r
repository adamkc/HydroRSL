tts.rawplot <-
function (dump, tclvar, schron, echron, minstg) 
{
    attach(tclvar, warn = F)
    on.exit(detach(tclvar))
    msgflag <- 0
    if (is.null(dev.list())) 
        print("Error in tts.rawplot: line commented out because win.graph function issue")
       # win.graph(width = 10, height = 7, pointsize = 12)
    if (tclvalue(pright) == 1) 
        par(mar = c(5, 4, 4, 4) + 0.1)
    else par(mar = c(5, 4, 4, 2) + 0.1)
    if (tclvalue(pright) == 1) {
        if (tclvalue(choice) == "stage-turbidity") {
            ltitle <- "stage"
            rtitle <- "turbidity"
        }
        else if (tclvalue(choice) == "discharge-turbidity") {
            ltitle <- "discharge"
            rtitle <- "turbidity"
        }
        else if (tclvalue(choice) == "discharge-rainfall") {
            ltitle <- "discharge"
            rtitle <- "cumulative rainfall"
        }
        else if (tclvalue(choice) == "discharge-water temperature") {
            ltitle <- "discharge"
            rtitle <- "water temperature"
        }
        else {
            ltitle <- "water temperature"
            rtitle <- "air temperature"
        }
        msgflag <- 1
    }
    else {
        ltitle <- tclvalue(choice)
        msgflag <- 0
    }
    midpoint <- mean(c(schron, echron))
    maintitle <- paste("TTS DATA PLOT\n", "Station ", toupper(attr(dump, 
        "stn")), ": HY ", hydro.year(midpoint), sep = "")
    leftcol <- ifelse(tclvalue(choice) %in% c("turbidity", "air temperature"), 
        "red", "blue")
    rightcol <- "red"
    if (tclvalue(choice) == "rainfall") {
        missing.rain <- is.na(dump$left)
        if (sum(missing.rain) > 0) {
            dump$left[missing.rain] <- 0
            cs <- cumsum(dump$left)
            cs[missing.rain] <- NA
        }
        else cs <- cumsum(dump$left)
        plot.default(x = dump$chron, y = cs, axes = F, type = "l", 
            col = leftcol, ylim = c(0, sum(dump$left)), main = maintitle, 
            ylab = "cumulative rainfall", xlab = "")
        text.default(x = dump$chron[length(dump$chron)], sum(dump$left), 
            labels = round(sum(dump$left), 2), offset = 0.5, 
            cex = 0.7, col = leftcol)
    }
    else {
        plot.default(x = dump$chron, y = dump$left, axes = F, 
            type = "n", ylim = c(as.numeric(tclvalue(min1)), 
                as.numeric(tclvalue(max1))), main = maintitle, 
            ylab = ltitle, xlab = "")
        diffs <- diff(dump$chron)
        startgaps <- seq(along = diffs)[diffs > 1.5 * min(diffs) & 
            diffs > 0.01]
        starts <- 1 + c(0, startgaps)
        ends <- c(startgaps, length(dump$chron))
        for (i in seq(along = starts)) {
            series <- starts[i]:ends[i]
            x <- dump$chron[series]
            y <- dump$left[series]
            lines(x = x, y = y, col = leftcol, err = -1)
        }
    }
    stagechosen <- length(grep("stage", tclvalue(choice)))
    if (stagechosen && !is.na(minstg)) {
        segments(min(as.numeric(x)), minstg, max(as.numeric(x)), 
            minstg, col = "blue", lty = 2)
        mtext(c("min", "stg"), side = 2, at = minstg + c(0.65, 
            -0.25) * strheight("minstg"), line = -0.25, las = 1, 
            adj = 0, cex = 0.7, col = "blue")
    }
    axis(2, las = 2, cex = 0.75)
    minortics(2)
    if (tclvalue(pright) == 1) {
        par(new = T)
        if (tclvalue(choice) == "discharge-rainfall") {
            missing.rain <- is.na(dump$right)
            if (sum(missing.rain) > 0) {
                dump$right[missing.rain] <- 0
                cs <- cumsum(dump$right)
                cs[missing.rain] <- NA
            }
            else cs <- cumsum(dump$right)
            plot.default(x = dump$chron, y = cs, axes = F, type = "l", 
                col = rightcol, ylim = c(0, sum(dump$right)), 
                main = maintitle, ylab = "", xlab = "")
            text.default(x = dump$chron[length(dump$chron)], 
                sum(dump$right), labels = round(sum(dump$right), 
                  2), offset = 0.5, cex = 0.7, col = rightcol)
        }
        else {
            plot.default(x = dump$chron, y = dump$right, axes = F, 
                type = "n", ylim = c(as.numeric(tclvalue(min2)), 
                  as.numeric(tclvalue(max2))), ylab = "", xlab = "")
            for (i in seq(along = starts)) {
                series <- starts[i]:ends[i]
                x <- dump$chron[series]
                y <- dump$right[series]
                lines(x = x, y = y, col = rightcol, err = -1)
            }
        }
        axis(4, las = 2, cex = 0.75, tck = -0.015)
        minortics(4)
        mtext(rtitle, side = 4, line = 2.75)
    }
    turbchosen <- length(grep("turbidity", tclvalue(choice)))
    if (turbchosen) {
        if (!all(dump$bottle == 0, na.rm = T)) {
            xpnts <- dump$chron[dump$bottle >= 1]
            ypnts <- dump$turb[dump$bottle >= 1]
            botnum <- dump$bottle[dump$bottle >= 1]
            points(xpnts, ypnts, cex = 0.7, err = -1)
            xrange <- par()$usr[2] - par()$usr[1]
            text(xpnts + 0.006 * xrange, ypnts, botnum, cex = 0.7, 
                adj = 0, col = "black")
        }
    }
    axis.time(dump$chron)
    mtext(paste("START = First logged point on or after", format(schron)), 
        side = 1, adj = 0, line = 3, cex = 0.5)
    mtext(paste("END = Last logged point on or before", format(echron)), 
        side = 1, adj = 0, line = 3.7, cex = 0.5)
    mystamp()
    box()
    if (msgflag == 1 || (turbchosen & any(dump$bottle > 0, na.rm = T))) 
        winDialog(type = c("ok"), "Click on the plot to place legend and redisplay dialog")
    if (turbchosen) {
        if (all(dump$bottle == 0, na.rm = T)) {
            if (tclvalue(pright) == 1) {
                location <- locator(1)
                xleg <- location$x
                yleg <- location$y
                legend(x = xleg, y = yleg, legend = c(ltitle, 
                  rtitle), col = c(leftcol, rightcol), lty = c(1, 
                  1), pch = c(-1, -1), cex = 0.7)
            }
        }
        else {
            location <- locator(1)
            xleg <- location$x
            yleg <- location$y
            if (tclvalue(pright) != 1) 
                legend(x = xleg, y = yleg, legend = c(ltitle, 
                  "bottle"), col = c(leftcol, "black"), lty = c(1, 
                  -1), pch = c(-1, 1), cex = 0.7)
            else legend(x = xleg, y = yleg, legend = c(ltitle, 
                rtitle, "bottle"), col = c(leftcol, rightcol, 
                "black"), lty = c(1, 1, -1), pch = c(-1, -1, 
                1), cex = 0.7)
        }
    }
    else {
        if (tclvalue(pright) == 1) {
            location <- locator(1)
            xleg <- location$x
            yleg <- location$y
            legend(x = xleg, y = yleg, legend = c(ltitle, rtitle), 
                col = c(leftcol, rightcol), lty = c(1, 1), cex = 0.7)
        }
    }
}
