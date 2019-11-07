addPlotMenu <-
structure(function () 
{
    winMenuAdd("TTS Plots")
    winMenuAddItem("TTS Plots", "Set Logger Home Dir (for raw data plots)", 
        "setLoggerHome()")
    winMenuAddItem("TTS Plots", "Set TTS Home Dir (for appended data plots)", 
        "setTTSHome()")
    winMenuAddItem("TTS Plots", "Plot Raw Data", "rawplot.gui()")
    winMenuAddItem("TTS Plots", "Plot Appended Data", "floplot.gui()")
}, source = c("function () ", "{", "winMenuAdd(\"TTS Plots\")", 
"winMenuAddItem(\"TTS Plots\", \"Set Logger Home Dir (for raw data plots)\", \"setLoggerHome()\")", 
"winMenuAddItem(\"TTS Plots\", \"Set TTS Home Dir (for appended data plots)\", \"setTTSHome()\")", 
"winMenuAddItem(\"TTS Plots\", \"Plot Raw Data\",\"rawplot.gui()\")", 
"winMenuAddItem(\"TTS Plots\", \"Plot Appended Data\", \"floplot.gui()\")", 
"}"))
addttsMenu <-
structure(function () 
{
    winMenuAdd("TTS")
    winMenuAddItem("TTS", "Set Logger Home Directory", "setLoggerHome()")
    winMenuAddItem("TTS", "Set TTS Home Directory", "setTTSHome()")
    winMenuAddItem("TTS", "Plot Raw Data", "rawplot.gui()")
    winMenuAddItem("TTS", "Plot Appended Data", "floplot.gui()")
    winMenuAddItem("TTS", "Read data and merge", "readAndMerge.gui()")
    winMenuAddItem("TTS", "Create interstorm data", "drop.storms.gui()")
    winMenuAddItem("TTS", "Identify sediment samples for exclusion", 
        "reselData.gui()")
    winMenuAddItem("TTS", "Scatterplot", "scatterPlot.gui()")
    winMenuAddItem("TTS", "Create a regression model for SSC using turbidity or flow", 
        "modelssc.gui()")
    winMenuAddItem("TTS", "Create a composite model for SSC using turbidity or flow", 
        "compmodel.gui()")
    winMenuAddItem("TTS", "Create a model for SSC using time interpolation", 
        "lineartime.gui()")
    winMenuAddItem("TTS", "Plot one or more contiguous models", 
        "ttsplot.gui()")
    winMenuAddItem("TTS", "Stats and total for several models", 
        "total.gui()")
    winMenuAddItem("TTS", "Save results as SSC time series", 
        "tts.ssc.gui()")
}, source = c("function() {", "winMenuAdd(\"TTS\")", "winMenuAddItem(\"TTS\", \"Set Logger Home Directory\", \"setLoggerHome()\")", 
"winMenuAddItem(\"TTS\", \"Set TTS Home Directory\", \"setTTSHome()\")", 
"winMenuAddItem(\"TTS\", \"Plot Raw Data\",\"rawplot.gui()\")", 
"winMenuAddItem(\"TTS\", \"Plot Appended Data\", \"floplot.gui()\")", 
"winMenuAddItem(\"TTS\", \"Read data and merge\", \"readAndMerge.gui()\")", 
"winMenuAddItem(\"TTS\", \"Create interstorm data\", \"drop.storms.gui()\")", 
"winMenuAddItem(\"TTS\", \"Identify sediment samples for exclusion\", \"reselData.gui()\")", 
"winMenuAddItem(\"TTS\", \"Scatterplot\",\"scatterPlot.gui()\")", 
"winMenuAddItem(\"TTS\", \"Create a regression model for SSC using turbidity or flow\", \"modelssc.gui()\")", 
"winMenuAddItem(\"TTS\", \"Create a composite model for SSC using turbidity or flow\", \"compmodel.gui()\")", 
"winMenuAddItem(\"TTS\", \"Create a model for SSC using time interpolation\", \"lineartime.gui()\")", 
"winMenuAddItem(\"TTS\", \"Plot one or more contiguous models\", \"ttsplot.gui()\")", 
"winMenuAddItem(\"TTS\", \"Stats and total for several models\", \"total.gui()\")", 
"winMenuAddItem(\"TTS\", \"Save results as SSC time series\", \"tts.ssc.gui()\")", 
"}"))
aic.lm <-
structure(function (lmfit) 
{
    if (class(lmfit) != "lm") 
        stop("Need 'lm' object")
    resids <- resid(lmfit)
    n <- length(resids)
    p <- lmfit$rank
    K <- p + 1
    sig2 <- sum(resids^2)/n
    aic <- n * log(sig2) + 2 * K
    aic.c <- aic + (2 * K * (K + 1))/(n - K - 1)
    c(aic = aic, aic.c = aic.c)
}, source = c("function(lmfit)", "{", "\t# Returns AIC and corrected AIC, apart from an additive constant", 
"\t# This is based on Burnham and Anderson, pages 48 and 51", 
"\t# See also the methods for AIC in the NLME library", "\tif(class(lmfit) != \"lm\") stop(\"Need 'lm' object\")", 
"\tresids <- resid(lmfit)", "\tn <- length(resids)", "\tp <- lmfit$rank", 
"\tK <- p + 1", "\tsig2 <- sum(resids^2)/n", "\taic <- n * log(sig2) + 2 * K", 
"\taic.c <- aic + (2 * K * (K + 1))/(n - K - 1)", "\tc(aic = aic, aic.c = aic.c)", 
"}"))
aic.loess <-
structure(function (model) 
{
    call <- model$call
    formula <- call$formula
    span <- model$pars$span
    degree <- model$pars$degree
    if (length(formula) != 3) 
        stop("This only works for simple y ~ x models")
    y <- eval(formula[[2]])
    x <- eval(formula[[3]])
    aicc <- crit.loess(span, x, y, degree)
    return(model$n * (aicc - 1))
}, source = c("function(model)", "{", "\t# Calls crit.loess to get AICC criterion for loess model", 
"\t# This version takes a fitted model as input", "\t# Assumes the formula is simply y ~ x", 
"\t# Result is rescaled for compatibility with aic.lm", "\tcall <- model$call", 
"\tformula <- call$formula", "\tspan <- model$pars$span", "\tdegree <- model$pars$degree", 
"\tif(length(formula) != 3)", "\t\tstop(\"This only works for simple y ~ x models\")", 
"\ty <- eval(formula[[2]])", "\tx <- eval(formula[[3]])", "\taicc <- crit.loess(span, x, y, degree)", 
"\treturn(model$n * (aicc - 1))", "}"))
albcalc <-
structure(function (stg) 
{
    ifelse(stg >= 0.4, 6.337 * (stg - 0.4)^2.367, 0)
}, source = c("function(stg) {", "  # Discharge rating equations for station ALB", 
"  # Add hy arg if different rating equations are to be applied in different years", 
"  # Convert stage in feet to flow in cfs", "       ifelse(stg >= 0.4, 6.337*(stg-0.4)^2.367, 0)", 
"}"))
allmatches <-
structure(function (x, y) 
{
    (1:length(y))[!is.na(match(y, x))]
}, source = c("function(x, y)", "{", "\t#  Same as match, but returns the indexes of ALL items", 
"\t#  in y vector that exactly match any element of x.", "\t#  If no matches are found, returns numeric(0).", 
"\t(1.:length(y))[!is.na(match(y, x))]", "}"))
arfcalc <-
structure(function (stg) 
{
    print("Warning from qcalc: ARF rating equation is invalid after HY99")
    a <- -0.2927
    b <- 1.4174
    c <- 81.5627
    c * stg^(b + a * log(stg))
}, source = c("function(stg)", "{", "\t# ARF discharge rating through HY99.", 
"\t# After HY99 use NFC minus XYZ.", "\tprint(\"Warning from qcalc: ARF rating equation is invalid after HY99\")", 
"\ta <- -0.2927", "\tb <- 1.4174", "\tc <- 81.5627", "\tc * stg^(b + a * log(stg))", 
"}"))
avg.err <-
structure(function (e) 
(exp(e) - exp(-e))/2, source = "function(e) (exp(e) - exp(-e))/2")
axis.time <-
structure(function (sdate, edate, side = 1, m = 1, labels = T, 
    hours = labels) 
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
}, source = c("function (sdate, edate, side = 1, m = 1, labels = T, hours = labels) ", 
"{", "# This version tested and is compatible with both R2.2.x and R2.6.x", 
"# Spacing (mtext(line=...) altered for \"png\" output (07Feb2005)", 
"    if (missing(edate)) {", "        edate <- trunc(max(sdate))", 
"        sdate <- trunc(min(sdate))", "    }", "    orig <- attr(sdate,\"origin\")", 
"    xlim <- par()$usr[1:2]", "    ndays <- edate - sdate", "    ticlocs <- seq((sdate - 1), (edate + 1))", 
"    if (ndays <= 21)", "        ticlen <- -0.04 * m", "    else ticlen <- -0.02 * m", 
"    if (ndays < 366)", "        tick <- T", "    else tick <- F", 
"    axis(side, at = ticlocs, labels = F, tck = ticlen, tick=tick)", 
"    if (labels & ndays < 30) {", "        ticlocs <- seq((sdate - 1), edate)", 
"        lablocs <- ticlocs + 0.5", "        if (ndays <= 8) {", 
"            mm <- zfill(as.numeric(months(ticlocs)),2)", "            dd <- zfill(as.numeric(days(ticlocs)),2)", 
"            yy <- substring(fac2num(years(ticlocs)),3,4)", "            ticlabels <- paste(mm, dd, yy, sep = \"/\")", 
"            textsize <- 0.90", "            line <- 1.1", "        }", 
"        else if (ndays <= 15) {", "            mm <- as.numeric(months(ticlocs))", 
"            dd <- zfill(as.numeric(days(ticlocs)),2)", "            ticlabels <- paste(mm, dd, sep = \"/\")", 
"            textsize <- 0.80", "            line <- 1.1", "        }", 
"        else if (ndays <= 22) {", "            ticlabels <- as.character(days(ticlocs))", 
"            textsize <- 0.75", "            line <- 0.9", "        }", 
"        else {", "            ticlabels <- as.character(days(ticlocs))", 
"            textsize <- 0.6", "            line <- 0", "        }", 
"        in.range <- (lablocs >= xlim[1]) & (lablocs <= xlim[2])", 
"        if (sum(in.range) > 0) ", "            mtext(ticlabels[in.range], side = 1, at = ticlocs[in.range] + ", 
"                0.5, line = line, cex = textsize)", "    }", 
"    if (ndays > 15) {", "        syear <- as.numeric(as.character(years(sdate)))", 
"        eyear <- as.numeric(as.character(years(edate)))", "        span <- eyear - syear", 
"        if (span >= 1) {", "            seq1 <- seq(as.numeric(months(sdate)), 12)", 
"            seq3 <- seq(1, as.numeric(months(edate)))", "            if (span > 1) {", 
"                seq2 <- rep(1:12, span-1)", "                rep2 <- rep(12,span-1)", 
"            }", "            else {", "                seq2 <- rep2 <- numeric(0)", 
"            }", "            mseq <- c(seq1, seq2, seq3)", "            reps <- c(length(seq1),rep2,length(seq3))", 
"            yseq <- rep(syear:eyear, reps)", "        }", "        else {", 
"            mseq <- seq(as.numeric(months(sdate)), as.numeric(months(edate)))", 
"            yseq <- c(rep(syear, length(mseq)))", "        }", 
"        yseq <- substring(yseq, 3, 4)", "        ticlocs <- dates(paste(zfill(mseq, 2), 1, yseq, sep = \"/\"), ", 
"            origin = orig)", "        lablocs <- dates(paste(zfill(mseq, 2), 16, yseq, sep = \"/\"), ", 
"            origin = orig)", "        if (length(lablocs) <= 12) {", 
"            text <- months(lablocs)", "            textsize <- 1.2", 
"        }", "        else {", "            text <- mseq", "            textsize <- 1.0", 
"        }", "        in.range <- (ticlocs >=xlim[1]) & (ticlocs <= xlim[2])", 
"        if (sum(in.range) > 0)", "            axis(side, at = ticlocs, label = F, tck = -0.04 * m)", 
"        if (ndays > 22) ", "            line <- 1.7", "        else line <- 2.5", 
"        in.range <- (lablocs >= xlim[1]) & (lablocs <= xlim[2])", 
"        if (labels && sum(in.range) > 0) ", "            mtext(as.character(text[in.range]), side = 1, at = lablocs[in.range], ", 
"                line = line, cex=textsize)", "    }", "    if (ndays <= 22) {", 
"        ticlocs <- seq(as.numeric(sdate - 1), as.numeric(edate + 1), by = 1/4)", 
"        if (ndays <= 21) ", "            ticlen <- -0.02 * m", 
"        else ticlen <- -0.01 * m", "        axis(side, at = ticlocs, labels = F, tck = ticlen, ", 
"            mgp = c(3, 0.75, 0), cex = 0.6)", "        if (ndays <= 4 && hours) {", 
"            ticlabels <- round(24 * ticlocs%%1)", "            in.range <- (ticlocs >= xlim[1]) & (ticlocs <=xlim[2])", 
"            if (sum(in.range) > 0) ", "                mtext(ticlabels[in.range], side = 1, at = ticlocs[in.range], ", 
"                line = -1.2, cex=0.75)", "        }", "    }", 
"    if (ndays <= 8) {", "        ticlocs <- seq(as.numeric(sdate - 1), as.numeric(edate + ", 
"            1), by = 1/24)", "        axis(side, at = ticlocs, labels = F, tck = -0.01 * m)", 
"    }", "}"))
bands <-
structure(function (fit, x, log = F, lty = 2, col = 1, type = "conf", 
    conf = 0.95) 
{
    n <- length(x)
    xmin <- range(x)[1]
    xmax <- range(x)[2]
    xpts <- seq(xmin, xmax, length = 20)
    if (log) {
        xbar <- mean(log(x))
        xssq <- sum((log(x) - xbar)^2)
        logxpts <- log(xpts)
    }
    else {
        xbar <- mean(x)
        xssq <- sum((x - xbar)^2)
        logxpts <- xpts
    }
    s <- summary(fit)$sigma
    df <- n - 2
    p <- 1 - 0.5 * (1 - conf)
    t <- qt(p, df)
    term <- 1/n + ((logxpts - xbar)^2)/xssq
    if (type == "pred") 
        term <- 1 + term
    halfwidth <- t * s * sqrt(term)
    a <- coef(fit)[1]
    b <- coef(fit)[2]
    yhat <- a + b * logxpts
    lower <- yhat - halfwidth
    upper <- yhat + halfwidth
    if (log) {
        lower <- exp(lower)
        upper <- exp(upper)
    }
    lines(xpts, lower, lty = lty, col = col, err = -1)
    lines(xpts, upper, lty = lty, col = col, err = -1)
}, source = c("function(fit, x, log = F, lty = 2., col = 1, type = \"conf\", conf = 0.95)", 
"{", "        # Plots confidence or prediction bands around a regression line", 
"        # Use type=\"pred\" to get prediction bands", "        n <- length(x)", 
"        xmin <- range(x)[1.]", "        xmax <- range(x)[2.]", 
"        xpts <- seq(xmin, xmax, length = 20.)", "        if(log) {", 
"                xbar <- mean(log(x))", "                xssq <- sum((log(x) - xbar)^2.)", 
"                logxpts <- log(xpts)", "        }", "        else {", 
"                xbar <- mean(x)", "                xssq <- sum((x - xbar)^2.)", 
"                logxpts <- xpts", "        }", "        s <- summary(fit)$sigma", 
"        df <- n - 2.", "        p <- 1. - 0.5 * (1. - conf)", 
"        t <- qt(p, df)", "        term <- 1./n + ((logxpts - xbar)^2.)/xssq", 
"        if(type == \"pred\")", "                term <- 1. + term", 
"        halfwidth <- t * s * sqrt(term)", "        a <- coef(fit)[1.]", 
"        b <- coef(fit)[2.]", "        yhat <- a + b * logxpts", 
"        lower <- yhat - halfwidth", "        upper <- yhat + halfwidth", 
"        if(log) {", "                lower <- exp(lower)", "                upper <- exp(upper)", 
"        }", "        lines(xpts, lower, lty = lty, col = col, err = -1.)", 
"        lines(xpts, upper, lty = lty, col = col, err = -1.)", 
"}"))
BoxCox <-
structure(function () 
{
    initializeDialog(title = "Box-Cox Transformations")
    variablesBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
        title = "Select variables (one or more)")
    onOK <- function() {
        variables <- getSelection(variablesBox)
        if (length(variables) < 1) {
            errorCondition(recall = BoxCox, message = "You must select one or more variables.")
            return()
        }
        closeDialog()
        command <- paste("box.cox.powers(na.omit(cbind(", paste(paste(variables, 
            "=", ActiveDataSet(), "$", variables, sep = ""), 
            collapse = ", "), ")))", sep = "")
        doItAndPrint(command)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "box.cox.powers")
    tkgrid(getFrame(variablesBox), sticky = "nw")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(rows = 2, columns = 1)
}, source = c("function(){", "   initializeDialog(title=\"Box-Cox Transformations\")", 
"   variablesBox <- variableListBox(top, Numeric(), selectmode=\"multiple\",", 
"   title=\"Select variables (one or more)\")", "   onOK <- function(){", 
"      variables <- getSelection(variablesBox)", "      if (length(variables) < 1) {", 
"         errorCondition(recall=BoxCox,", "         message=\"You must select one or more variables.\")", 
"         return()", "      }", "      closeDialog()", "      command <- paste(\"box.cox.powers(na.omit(cbind(\",", 
"         paste(paste(variables, \"=\", ActiveDataSet(), \"$\", variables, sep=\"\"),", 
"         collapse=\", \"), \")))\", sep=\"\")", "      doItAndPrint(command)", 
"      tkfocus(CommanderWindow())", "   }", "   OKCancelHelp(helpSubject=\"box.cox.powers\")", 
"   tkgrid(getFrame(variablesBox), sticky=\"nw\")", "   tkgrid(buttonsFrame, sticky=\"w\")", 
"   dialogSuffix(rows=2, columns=1)", "}"))
build.power.table <-
structure(function (power, n1, n2, prop.n2, alpha, theta1, theta2, 
    disp1, disp2, expand.args, one.sample, compute.what) 
{
    if (!is.null(prop.n2) && any(prop.n2 <= 0)) {
        stop("prop.n2 must be greater than 0")
    }
    if (missing(disp1)) {
        disp1 <- NULL
        disp2 <- NULL
    }
    if (!(one.sample || compute.what == "sample.size")) {
        expand.n <- length(n1) != length(n2)
        if (expand.args && expand.n) {
            if (is.null(n1)) {
                n.table <- expand.grid(n1 = as.numeric(NA), n2 = n2, 
                  prop.n2 = prop.n2)
                n.table$n1 <- n.table$n2/n.table$prop.n2
            }
            else if (is.null(n2)) {
                n.table <- expand.grid(n1 = n1, n2 = as.numeric(NA), 
                  prop.n2 = prop.n2)
                n.table$n2 <- n.table$n1 * n.table$prop.n2
            }
            else {
                n.table <- expand.grid(n1 = n1, n2 = n2, prop.n2 = as.numeric(NA))
                n.table$prop.n2 <- n.table$n2/n.table$n1
            }
            prop.n2 <- seq(nrow(n.table))
            n1 <- n2 <- as.numeric(NA)
        }
        else {
            if (is.null(n1)) {
                n1 <- n2/prop.n2
            }
            else if (is.null(n2)) {
                n2 <- prop.n2 * n1
            }
            else {
                prop.n2 <- n2/n1
            }
            n.table <- as.data.frame(list(n1 = n1, n2 = n2, prop.n2 = prop.n2))
            if (expand.args) {
                prop.n2 <- seq(nrow(n.table))
                n1 <- n2 <- as.numeric(NA)
            }
        }
    }
    arg.list <- list(theta1 = theta1, disp1 = disp1, theta2 = theta2, 
        disp2 = disp2, delta = as.numeric(NA), alpha = alpha, 
        power = power, n1 = n1, n2 = n2, prop.n2 = prop.n2)
    arg.list <- arg.list[sapply(arg.list, length) > 0]
    if (expand.args) {
        power.table <- expand.grid(arg.list)
        if (!(one.sample || compute.what == "sample.size")) {
            n.table <- n.table[power.table$prop.n2, ]
            power.table$prop.n2 <- n.table$prop.n2
            power.table$n1 <- n.table$n1
            power.table$n2 <- n.table$n2
        }
    }
    else {
        power.table <- as.data.frame(arg.list)
    }
    if (compute.what != "delta") {
        power.table$delta <- power.table$theta2 - power.table$theta1
    }
    class(power.table) <- c("power.table", "data.frame")
    return(power.table)
}, source = c("function(power, n1, n2, prop.n2, alpha, theta1, theta2, disp1, disp2, expand.args, one.sample, compute.what)", 
"{", "\tif(!is.null(prop.n2) && any(prop.n2 <= 0)) {", "\t\tstop(\"prop.n2 must be greater than 0\")", 
"\t}", "\tif(missing(disp1)) {", "\t\tdisp1 <- NULL", "\t\tdisp2 <- NULL", 
"\t}", "###", "### If not computing N, need to reconcile n1, n2 and prop.n2.", 
"### If expand.args, and n1, n2 different lengths, expand the N's.", 
"###", "\tif(!(one.sample || compute.what == \"sample.size\")) {", 
"\t\texpand.n <- length(n1) != length(n2)", "\t\tif(expand.args && expand.n) {", 
"\t\t\tif(is.null(n1)) {", "\t\t\t\tn.table <- expand.grid(n1 = as.numeric(NA), n2 = n2, prop.n2 = prop.n2)", 
"\t\t\t\tn.table$n1 <- n.table$n2/n.table$prop.n2", "\t\t\t}", 
"\t\t\telse if(is.null(n2)) {", "\t\t\t\tn.table <- expand.grid(n1 = n1, n2 = as.numeric(NA), prop.n2 = prop.n2)", 
"\t\t\t\tn.table$n2 <- n.table$n1 * n.table$prop.n2", "\t\t\t}", 
"\t\t\telse {", "\t\t\t\tn.table <- expand.grid(n1 = n1, n2 = n2, prop.n2 = as.numeric(NA))", 
"\t\t\t\tn.table$prop.n2 <- n.table$n2/n.table$n1", "\t\t\t}", 
"\t\t\tprop.n2 <- seq(nrow(n.table))", "\t\t\tn1 <- n2 <- as.numeric(NA)", 
"\t\t}", "\t\telse {", "\t\t\tif(is.null(n1)) {", "\t\t\t\tn1 <- n2/prop.n2", 
"\t\t\t}", "\t\t\telse if(is.null(n2)) {", "\t\t\t\tn2 <- prop.n2 * n1", 
"\t\t\t}", "\t\t\telse {", "\t\t\t\tprop.n2 <- n2/n1", "\t\t\t}", 
"\t\t\tn.table <- as.data.frame(list(n1 = n1, n2 = n2, prop.n2 = prop.n2))", 
"\t\t\tif(expand.args) {", "\t\t\t\tprop.n2 <- seq(nrow(n.table))", 
"\t\t\t\tn1 <- n2 <- as.numeric(NA)", "\t\t\t}", "\t\t}", "\t}", 
"\targ.list <- list(theta1 = theta1, disp1 = disp1, theta2 = theta2, disp2 = disp2, delta = as.numeric(NA), alpha", 
"\t\t = alpha, power = power, n1 = n1, n2 = n2, prop.n2 = prop.n2)", 
"\targ.list <- arg.list[sapply(arg.list, length) > 0]", "\tif(expand.args) {", 
"###", "### Expand table, then put back n1 and n2 if two-sample", 
"###", "\t\tpower.table <- expand.grid(arg.list)", "\t\tif(!(one.sample || compute.what == \"sample.size\")) {", 
"\t\t\tn.table <- n.table[power.table$prop.n2,  ]", "\t\t\tpower.table$prop.n2 <- n.table$prop.n2", 
"\t\t\tpower.table$n1 <- n.table$n1", "\t\t\tpower.table$n2 <- n.table$n2", 
"\t\t}", "\t}", "\telse {", "\t\tpower.table <- as.data.frame(arg.list)", 
"\t}", "\tif(compute.what != \"delta\") {", "\t\tpower.table$delta <- power.table$theta2 - power.table$theta1", 
"\t}", "\tclass(power.table) <- c(\"power.table\", \"data.frame\")", 
"\treturn(power.table)", "}"))
campbell.date <-
structure(function (year, day, time = 0) 
{
    daynum <- numeric(length(year))
    for (yr in unique(year)) {
        tmp <- dates(day[year == yr], origin = c(12, 31, yr - 
            1))
        daynum[year == yr] <- as.numeric(dates(tmp, origin = c(1, 
            1, 1970)))
    }
    chron <- chron(daynum) + mt2msm(time)/1440
    chron
}, source = c("function(year, day, time = 0)", "{", "\t# Returns chron array for given year, day, and time vectors", 
"\t# year = 4-digit year", "\t# day = day number from start of year (Jan.1 = 1)", 
"\t# time = military time", "\t# Sets common origin to 1/1/1970 for axis.time to work properly", 
"\tdaynum <- numeric(length(year))", "\tfor(yr in unique(year)) {", 
"\t\ttmp <- dates(day[year == yr], origin = c(12, 31, yr - 1))", 
"\t\tdaynum[year == yr] <- as.numeric(dates(tmp, origin = c(1, 1,", 
"\t\t\t1970)))", "\t}", "\tchron <- chron(daynum) + mt2msm(time)/1440.", 
"\tchron", "}"))
check.interstorm <-
structure(function (stn, hy, interstorm, surrogate, checkflo = T, 
    checksed = T) 
{
    if (interstorm) {
        if (surrogate == "turbidity") 
            sta <- "trb"
        else sta <- "flo"
        if (checksed) {
            objname <- paste(sta, hy, ".sed", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                cat("ERROR:", objname, "not found\n")
            sedobj <- eval(as.name(objname))
            if (attr(sedobj, "stn") != stn) 
                cat("ERROR: The interstorm sed data is from station", 
                  attr(sedobj, "stn"), ", not", stn, "\n")
        }
        if (checkflo) {
            objname <- paste(sta, hy, ".flo", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                cat("ERROR:", objname, "not found\n")
            floobj <- eval(as.name(objname))
            if (attr(floobj, "stn") != stn) 
                cat("ERROR: The interstorm flo data is from station", 
                  attr(sedobj, "stn"), ", not", stn, "\n")
        }
    }
    else sta <- stn
    return(sta)
}, source = c("function(stn, hy, interstorm, surrogate, checkflo=T, checksed=T) {", 
"  # Checks that interstorm data \"stn\" attribute matches user's station", 
"  # Returns \"trb\" or \"flo\" for interstorm kludge analysis", 
"  if (interstorm) {", "    if (surrogate==\"turbidity\")", "      sta <- \"trb\"", 
"    else", "      sta <- \"flo\"", "    # Check attribute to be sure we'll get the right station", 
"    if (checksed) {", "      objname <- paste(sta,hy,\".sed\",sep=\"\")", 
"      if (!exists(objname,env=.GlobalEnv)) ", "        cat(\"ERROR:\",objname,\"not found\\n\")", 
"      sedobj <- eval(as.name(objname))", "      if (attr(sedobj,\"stn\") != stn)", 
"        cat(\"ERROR: The interstorm sed data is from station\",attr(sedobj,\"stn\"),\", not\",stn,\"\\n\")", 
"    }", "    if (checkflo) {", "      objname <- paste(sta,hy,\".flo\",sep=\"\")", 
"      if (!exists(objname,env=.GlobalEnv)) ", "        cat(\"ERROR:\",objname,\"not found\\n\")", 
"      floobj <- eval(as.name(objname))", "      if (attr(floobj,\"stn\") != stn)", 
"        cat(\"ERROR: The interstorm flo data is from station\",attr(sedobj,\"stn\"),\", not\",stn,\"\\n\")", 
"    }    ", "  }", "  else", "    sta <- stn", "  return(sta)", 
"}"))
compmodel.gui <-
structure(function () 
{
    model <- function(panel) {
        with(panel, {
            stn <- tolower(pars["stn"])
            hy4 <- pars["hy4"]
            firstdt1 <- pars["firstdt1"]
            lastdt1 <- pars["lastdt1"]
            firstdt2 <- pars["firstdt2"]
            lastdt2 <- pars["lastdt2"]
            dumpexpr <- as.vector(pars["dumpexpr"])
            botexpr <- as.vector(pars["botexpr"])
            result <- pars["result"]
            interstorm <- checkvars["interstorm"]
            adj <- checkvars["adj"]
            exclude <- checkvars["exclude"]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- as.vector(substring(hy4, 3, 4))
            setTTSenv(c("STN", "HY", "SDATE", "EDATE", "DUMPS", 
                "BOTTLES", "ADJUST", "INTERSTORM"), c(stn, hy4, 
                firstdt1, lastdt1, dumpexpr, botexpr, adj, interstorm))
            span <- as.numeric(loesspars["span"])
            degree <- as.numeric(loesspars["degree"])
            sdate1 <- paste(substring(firstdt1, 7, 8), substring(firstdt1, 
                1, 2), substring(firstdt1, 4, 5), sep = "")
            stime1 <- paste(substring(firstdt1, 10, 11), substring(firstdt1, 
                13, 14), sep = "")
            edate1 <- paste(substring(lastdt1, 7, 8), substring(lastdt1, 
                1, 2), substring(lastdt1, 4, 5), sep = "")
            etime1 <- paste(substring(lastdt1, 10, 11), substring(lastdt1, 
                13, 14), sep = "")
            arglist <- list(type = type, exclude = exclude, adj = adj, 
                var = F)
            sta <- check.interstorm(stn, hy2, interstorm, surrogate, 
                checkflo = F)
            if (type == "loess") 
                arglist <- c(arglist, degree = degree, span = span)
            if (subsetby == "Alternate period") {
                sdate2 <- paste(substring(firstdt2, 7, 8), substring(firstdt2, 
                  1, 2), substring(firstdt2, 4, 5), sep = "")
                stime2 <- paste(substring(firstdt2, 10, 11), 
                  substring(firstdt2, 13, 14), sep = "")
                edate2 <- paste(substring(lastdt2, 7, 8), substring(lastdt2, 
                  1, 2), substring(lastdt2, 4, 5), sep = "")
                etime2 <- paste(substring(lastdt2, 10, 11), substring(lastdt2, 
                  13, 14), sep = "")
                arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                  etime1, sdate2, stime2, edate2, etime2, arglist)
            }
            else if (subsetby == "Specific dumps/bottles") {
                if (botexpr == "") {
                  arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                    etime1, dumps = dumpexpr, arglist)
                }
                else {
                  arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                    etime1, dumps = dumpexpr, bottles = botexpr, 
                    arglist)
                }
            }
            else {
                arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                  etime1, arglist)
            }
            opt1 <- match(startopt, c("Start with zero", "Start with first residual in estimation period", 
                "Use nearest residual preceding estimation period"))
            opt2 <- match(endopt, c("End with zero", "End with last residual in estimation period", 
                "Use nearest residual following estimation period"))
            arglist <- c(arglist, comp = T, opt = list(c(opt1 - 
                1, opt2 - 1)))
            if (surrogate == "turbidity") 
                funcname <- "turbsrc"
            else funcname <- "flowsrc"
            modelfunc <- get(funcname, envir = .GlobalEnv)
            res <- do.call("modelfunc", arglist)
            saveCommand(stn, hy2, funcname, arglist, result, 
                checkvars["savecmd"])
            if (result != "") {
                assign(result, res, envir = .GlobalEnv)
                cat("Result saved in workspace as", result, "\n")
            }
            else cat("To save results, enter name of output object and press OK\n")
        })
        panel
    }
    nothing <- function(panel) panel
    env <- getTTSenv()
    init.stn <- env["STN", ]
    init.hy <- env["HY", ]
    init.sdate <- env["SDATE", ]
    init.edate <- env["EDATE", ]
    init.sdump <- env["SDUMP", ]
    init.edump <- env["EDUMP", ]
    dumpexpr <- env["DUMPS", ]
    botexpr <- env["BOTTLES", ]
    init.adjust <- env["ADJUST", ]
    init.interstorm <- env["INTERSTORM", ]
    if (is.na(init.adjust)) 
        init.adjust <- F
    if (is.na(init.interstorm)) 
        init.interstorm <- F
    pars <- c(init.stn, init.hy, init.sdate, init.edate, init.sdate, 
        init.edate, dumpexpr, botexpr, "")
    loesspars <- c(1, 1)
    panel <- rp.control("Composite model for SSC")
    rp.radiogroup(panel, surrogate, c("turbidity", "flow"), title = "Sediment surrogate", 
        action = nothing)
    my.textentry(panel, pars, labels = c("Station", "Water year", 
        "Starting date/time (m/d/y h:m)", "Ending date/time (m/d/y h:m)", 
        "Alternate start for samples (m/d/y h:m)", "Alternate end for samples (m/d/y h:m)", 
        "Dumps (R expression)", "Corresponding bottles (R expression)", 
        "Output object name"), names = c("stn", "hy4", "firstdt1", 
        "lastdt1", "firstdt2", "lastdt2", "dumpexpr", "botexpr", 
        "result"), title = "Enter values", initval = pars)
    rp.radiogroup(panel, subsetby, c("Estimation period", "Alternate period", 
        "Specific dumps/bottles"), title = "Method of selecting samples", 
        action = nothing, initval = "Estimation period")
    rp.radiogroup(panel, startopt, c("Start with zero", "Start with first residual in estimation period", 
        "Use nearest residual preceding estimation period"), 
        title = "Beginning interpolation option", action = nothing, 
        initval = "Start with zero")
    rp.radiogroup(panel, endopt, c("End with zero", "End with last residual in estimation period", 
        "Use nearest residual following estimation period"), 
        title = "Ending interpolation option", action = nothing, 
        initval = "End with zero")
    rp.radiogroup(panel, type, c("linear", "logxy", "power", 
        "loess", "sqrt"), title = "Model to fit", initval = "linear", 
        action = nothing)
    my.textentry(panel, loesspars, labels = c("span", "degree"), 
        names = c("span", "degree"), title = "Loess parameters", 
        initval = loesspars)
    rp.checkbox(panel, checkvars, initval = c(T, init.interstorm, 
        init.adjust, T), title = "Other options", labels = c("Exclude previously flagged points (those with exclude=T)", 
        "Interstorm data only", "Adjust SSC to depth-integrated equivalent", 
        "Save command to file"), names = c("exclude", "interstorm", 
        "adj", "savecmd"), action = nothing)
    rp.button(panel, action = model, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run turbsrc() and flowsrc() ", 
"  model <- function(panel) {", "    with(panel, {", "      stn <- tolower(pars[\"stn\"])", 
"      hy4 <- pars[\"hy4\"]", "      firstdt1 <- pars[\"firstdt1\"]", 
"      lastdt1 <- pars[\"lastdt1\"]", "      firstdt2 <- pars[\"firstdt2\"]", 
"      lastdt2 <- pars[\"lastdt2\"]", "      dumpexpr <- as.vector(pars[\"dumpexpr\"])  # necessary when arglist is assembled later", 
"      botexpr <- as.vector(pars[\"botexpr\"])    # necessary when arglist is assembled later", 
"      result <- pars[\"result\"]", "      interstorm <- checkvars[\"interstorm\"]", 
"      adj <- checkvars[\"adj\"]", "      exclude <- checkvars[\"exclude\"]", 
"", "      if (nchar(hy4) < 3) {", "        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- as.vector(substring(hy4,3,4))  # as.vector required to get rid of the name \"hy4\"", 
"      # Save current values in TTS Environment", "      setTTSenv(c(\"STN\",\"HY\",\"SDATE\",\"EDATE\",\"DUMPS\",\"BOTTLES\",\"ADJUST\",\"INTERSTORM\"),", 
"                c(stn,hy4,firstdt1,lastdt1,dumpexpr,botexpr,adj,interstorm))", 
"      ", "      span <- as.numeric(loesspars[\"span\"])", "      degree <- as.numeric(loesspars[\"degree\"]) ", 
"      sdate1 <- paste(substring(firstdt1,7,8),substring(firstdt1,1,2),substring(firstdt1,4,5),sep=\"\")", 
"      stime1 <- paste(substring(firstdt1,10,11),substring(firstdt1,13,14),sep=\"\")", 
"      edate1 <- paste(substring(lastdt1,7,8),substring(lastdt1,1,2),substring(lastdt1,4,5),sep=\"\")", 
"      etime1 <- paste(substring(lastdt1,10,11),substring(lastdt1,13,14),sep=\"\")", 
"      arglist <- list(type=type,exclude=exclude,adj=adj,var=F)", 
"      sta <- check.interstorm(stn, hy2, interstorm, surrogate,checkflo=F)", 
"      if (type==\"loess\") arglist <- c(arglist,degree=degree,span=span)", 
"      if (subsetby == \"Alternate period\") {", "        sdate2 <- paste(substring(firstdt2,7,8),substring(firstdt2,1,2),substring(firstdt2,4,5),sep=\"\")", 
"        stime2 <- paste(substring(firstdt2,10,11),substring(firstdt2,13,14),sep=\"\")", 
"        edate2 <- paste(substring(lastdt2,7,8),substring(lastdt2,1,2),substring(lastdt2,4,5),sep=\"\")", 
"        etime2 <- paste(substring(lastdt2,10,11),substring(lastdt2,13,14),sep=\"\")", 
"        arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,sdate2,stime2,edate2,etime2,arglist)", 
"      }", "      else if(subsetby==\"Specific dumps/bottles\") {", 
"        if (botexpr == \"\") {", "          arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,dumps=dumpexpr,arglist)", 
"        }", "        else {", "          arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,dumps=dumpexpr,bottles=botexpr,arglist)", 
"        }", "      }", "      else {", "        arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,arglist)", 
"      }", "      # Finally add composite method arguments", 
"      opt1 <- match(startopt,c(\"Start with zero\",\"Start with first residual in estimation period\",", 
"               \"Use nearest residual preceding estimation period\"))", 
"      opt2 <- match(endopt,c(\"End with zero\",\"End with last residual in estimation period\",", 
"               \"Use nearest residual following estimation period\"))", 
"      # Map options (1,2,3) to (0,1,2) ", "      arglist <- c(arglist,comp=T,opt=list(c(opt1-1,opt2-1)))", 
"      # print(arglist)", "      if (surrogate == \"turbidity\")", 
"        funcname <- \"turbsrc\"", "      else ", "        funcname <- \"flowsrc\"", 
"      modelfunc <- get(funcname,envir=.GlobalEnv)", "      res <- do.call(\"modelfunc\",arglist)", 
"      saveCommand(stn,hy2,funcname,arglist,result,checkvars[\"savecmd\"])", 
"      if (result!=\"\") {", "          assign(result,res,envir=.GlobalEnv)", 
"          cat(\"Result saved in workspace as\",result,\"\\n\")", 
"      }", "      else ", "        cat(\"To save results, enter name of output object and press OK\\n\")", 
"    })", "    panel", "  }", "  nothing <- function(panel) panel", 
"  env <- getTTSenv()", "  init.stn <- env[\"STN\",]", "  init.hy <- env[\"HY\",]", 
"  init.sdate <- env[\"SDATE\",]", "  init.edate <- env[\"EDATE\",]", 
"  init.sdump <- env[\"SDUMP\",]", "  init.edump <- env[\"EDUMP\",] ", 
"  dumpexpr <- env[\"DUMPS\",]", "  botexpr <- env[\"BOTTLES\",]", 
"  init.adjust <- env[\"ADJUST\",]", "  init.interstorm <- env[\"INTERSTORM\",]", 
"  if (is.na(init.adjust))", "    init.adjust <- F", "  if (is.na(init.interstorm))", 
"    init.interstorm <- F", "", "  pars <- c(init.stn,init.hy,init.sdate,init.edate,init.sdate,init.edate,dumpexpr,botexpr,\"\")", 
"  loesspars <- c(1,1)", "  panel <- rp.control(\"Composite model for SSC\")", 
"  rp.radiogroup(panel, surrogate, c(\"turbidity\",\"flow\"),title=\"Sediment surrogate\",action=nothing)", 
"  my.textentry(panel,pars,labels=c(\"Station\",\"Water year\",\"Starting date/time (m/d/y h:m)\",\"Ending date/time (m/d/y h:m)\",", 
"                                   \"Alternate start for samples (m/d/y h:m)\",\"Alternate end for samples (m/d/y h:m)\",", 
"                                   \"Dumps (R expression)\",\"Corresponding bottles (R expression)\",\"Output object name\"),", 
"               names=c(\"stn\",\"hy4\",\"firstdt1\",\"lastdt1\",\"firstdt2\",\"lastdt2\",\"dumpexpr\",\"botexpr\",\"result\"),", 
"               title=\"Enter values\",initval=pars)", "  rp.radiogroup(panel, subsetby, c(\"Estimation period\",\"Alternate period\",\"Specific dumps/bottles\"),", 
"                title=\"Method of selecting samples\",action=nothing,initval=\"Estimation period\")", 
"  rp.radiogroup(panel, startopt, c(\"Start with zero\",\"Start with first residual in estimation period\",", 
"               \"Use nearest residual preceding estimation period\"),", 
"                title=\"Beginning interpolation option\",action=nothing,initval=\"Start with zero\")", 
"  rp.radiogroup(panel, endopt, c(\"End with zero\",\"End with last residual in estimation period\",", 
"               \"Use nearest residual following estimation period\"),", 
"                title=\"Ending interpolation option\",action=nothing,initval=\"End with zero\")", 
"  rp.radiogroup(panel, type, c(\"linear\", \"logxy\", \"power\", \"loess\", \"sqrt\"), title=\"Model to fit\", initval=\"linear\",action=nothing)", 
"  my.textentry(panel,loesspars,labels=c(\"span\",\"degree\"),names=c(\"span\",\"degree\"),title=\"Loess parameters\",initval=loesspars)", 
"  rp.checkbox(panel, checkvars, initval = c(T,init.interstorm,init.adjust,T), title=\"Other options\", ", 
"              labels=c(\"Exclude previously flagged points (those with exclude=T)\",\"Interstorm data only\",", 
"                       \"Adjust SSC to depth-integrated equivalent\",\"Save command to file\"),", 
"              names=c(\"exclude\",\"interstorm\",\"adj\",\"savecmd\"),action=nothing)", 
"  rp.button(panel, action = model, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
composite <-
structure(function (intime, resid, outtime, opt, oldcomp) 
{
    if (length(intime) != length(resid)) 
        stop("Number of samples does not match number of sample times")
    if (oldcomp && (opt[1] == 2 || opt[2] == 2)) 
        warning("With oldcomp=T, you should specify the original opt values (0 or 1 only).")
    if (oldcomp == F && opt[1] == 2 && min(intime) >= outtime[1]) {
        opt[1] <- 1
        warning("No external samples were specified prior to estimation period.")
    }
    if (oldcomp == F && opt[2] == 2 && max(intime) <= last.val(outtime)) {
        opt[2] <- 1
        warning("No external samples were specified after estimation period.")
    }
    if (oldcomp || opt[1] == 2) 
        begin <- intime[1]
    else begin <- min(intime[intime >= outtime[1]])
    if (oldcomp || opt[2] == 2) 
        end <- last.val(intime)
    else end <- max(intime[intime <= last.val(outtime)])
    xin <- intime[intime >= begin & intime <= end]
    yin <- resid[intime >= begin & intime <= end]
    if (outtime[1] < begin) {
        xin <- c(outtime[1], xin)
        ystart <- ifelse(opt[1] == 0, 0, yin[1])
        yin <- c(ystart, yin)
    }
    if (last.val(outtime) > end) {
        xin <- c(xin, last.val(outtime))
        yend <- ifelse(opt[2] == 0, 0, last.val(yin))
        yin <- c(yin, yend)
    }
    approx(xin, yin, outtime)$y
}, source = c("function (intime,resid,outtime,opt,oldcomp) {", 
"    # Residuals interpolation for the composite method      ", 
"    # oldcomp option is for compatibility with an earlier version", 
"    # opt is a vector of 2 values defining behaviors at start and end", 
"    # of the estimation period = range of outtime  ", "    #   0 = use 0 for the endpoint", 
"    #   1 = use the first or last sample in the estimation period", 
"    #   2 = use nearest sample outside the estimation period", 
"    # If oldcomp = T, then use option 2 if samples are found outside", 
"    #   the estimation period, otherwise use the specified option 0 or 1", 
"    # If oldcomp = F and opt=2, then issue a warning if no samples are found ", 
"    #   outside the estimation period.  ", "    if (length(intime) != length(resid))", 
"       stop(\"Number of samples does not match number of sample times\")", 
"    # First, determine the begin and end times", "    if (oldcomp && (opt[1]==2 || opt[2]==2))", 
"       warning(\"With oldcomp=T, you should specify the original opt values (0 or 1 only).\")", 
"    if (oldcomp == F && opt[1]==2 && min(intime) >= outtime[1]) {", 
"       opt[1] <-  1", "       warning(\"No external samples were specified prior to estimation period.\")", 
"    }", "    if (oldcomp == F && opt[2]==2 && max(intime) <= last.val(outtime)) {", 
"       opt[2] <-  1", "       warning(\"No external samples were specified after estimation period.\")", 
"    }", "    if (oldcomp || opt[1] == 2)", "       # Start of interpolation will be an external sample", 
"       begin <- intime[1]", "    else", "       begin <- min(intime[intime >= outtime[1]])", 
"    if (oldcomp || opt[2] == 2)", "       # End of interpolation will be an external sample", 
"       end <- last.val(intime)", "    else", "       end <- max(intime[intime <= last.val(outtime)])", 
"", "    # Assign the sample times and residuals for interpolation  ", 
"    xin <- intime[intime >= begin & intime <= end]", "    yin <- resid[intime >= begin & intime <= end]", 
" ", "    # Interpolate residuals at specified output times", 
"    if (outtime[1] < begin) {", "       xin <- c(outtime[1],xin)", 
"       ystart <- ifelse(opt[1]==0,0,yin[1])", "       yin <- c(ystart,yin)", 
"    } ", "    if (last.val(outtime) > end) {", "       xin <- c(xin, last.val(outtime))", 
"       yend <- ifelse(opt[2]==0,0,last.val(yin))", "       yin <- c(yin, yend)", 
"    }", "    approx(xin,yin,outtime)$y", "}"))
cor.matrix <-
structure(function (interval, popq, xsample, xpop, def = 0.75) 
{
    k <- 0.06 * interval
    n <- length(xsample)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    x2 <- cbind(k * popq, k * popq * xpop)
    V <- try(x2 %*% invxx %*% t(x2))
    if (inherits(V, "try-error")) {
        print("Cannot compute covariance matrix")
        print(paste("Using default correlation of", def))
        N <- length(xpop)
        defmatrix <- matrix(def, nc = N, nr = N)
        diag(defmatrix) <- 1
        return(defmatrix)
    }
    else {
        vars <- diag(V)
        sigma <- sqrt(vars)
        cormatrix <- t(V/sigma)/sigma
        return(cormatrix)
    }
}, source = c("function(interval, popq, xsample, xpop, def = 0.75)", 
"{", "\t# Calculates corrlelation matrix of the predictions from", 
"\t# the simple linear regression model y ~ xsample.", "\t# Note that the y sample data is not needed because it's just", 
"\t#   needed for the regression MSE, which is a scalar multiplier", 
"\t#   on the whole covariance matrix.  It drops out of the", 
"\t#   calculation of correlations.", "\t# The correlation matrix can be used with the pointwise standard", 
"\t# errors from another regression model to guess the covariance", 
"\t# matrix of the predictions from that model.", "\t# xsample is the sample X data", 
"\t# xpop is the X data being predicted", "\t# popq is the discharge ", 
"\tk <- 0.06 * interval", "\tn <- length(xsample)", "\tx1 <- cbind(rep(1, n), xsample)", 
"\txx <- t(x1) %*% x1", "\tinvxx <- solve(xx)", "\tx2 <- cbind(k * popq, k * popq * xpop)", 
"\tV <- try(x2 %*% invxx %*% t(x2))", "\tif(inherits(V,\"try-error\")) {", 
"\t\tprint(\"Cannot compute covariance matrix\")", "\t\tprint(paste(\"Using default correlation of\", def))", 
"\t\tN <- length(xpop)", "\t\tdefmatrix <- matrix(def, nc = N, nr = N)", 
"\t\tdiag(defmatrix) <- 1", "\t\treturn(defmatrix)", "\t}", "\telse {", 
"\t\tvars <- diag(V)", "\t\tsigma <- sqrt(vars)", "\t\t# Divide rows and columns by sigma to get cor matrix", 
"\t\tcormatrix <- t(V/sigma)/sigma", "\t\treturn(cormatrix)", 
"\t}", "}"))
crbasic.date <-
structure(function (date) 
{
    date.list <- sapply(as.character(date), strsplit, " ")
    date <- sapply(date.list, function(x) x[[1]])
    time <- sapply(date.list, function(x) x[[2]])
    chron(date, time, format = c("y-m-d", "h:m:s"), out.format = c("m/d/y", 
        "h:m:s"))
}, source = c("function(date) {", "date.list <- sapply(as.character(date),strsplit,\" \")", 
"date <- sapply(date.list,function(x) x[[1]])", "time <- sapply(date.list,function(x) x[[2]])", 
"chron(date,time,format=c(\"y-m-d\",\"h:m:s\"),out.format=c(\"m/d/y\",\"h:m:s\"))", 
"}"))
create.flo <-
structure(function (data, filepath, extras, write = T) 
{
    year <- as.numeric(as.character(years(data$chr)))
    mo <- zfill(as.numeric(months(data$chr)), 2)
    dy <- zfill(as.numeric(days(data$chr)), 2)
    mt <- zfill(msm2mt(round(1440 * (as.numeric(data$chr)%%1))), 
        4)
    dump <- 1
    bot <- 0
    code <- "BX"
    stg <- 0
    corstg <- 0
    stgcode <- 1
    q <- data$q
    turb <- data$turb
    corturb <- data$corturb
    df <- data.frame(year, mo, dy, mt, dmp = 1, bot = 0, code = "BX", 
        rawstg = data$q, corstg = data$q, stgcode = 1, q = data$q, 
        rawturb = data$turb, turb = data$turb, turbcode = -1)
    if (!missing(extras)) {
        for (var in extras) df[, var] <- data[, var]
    }
    if (write) 
        write.table(df, filepath, quote = FALSE, row.names = F, 
            col.names = F, sep = ",")
    else return(df)
}, source = c("function(data,filepath,extras,write=T) {", "  year <- as.numeric(as.character(years(data$chr)))", 
"  mo <- zfill(as.numeric(months(data$chr)),2)", "  dy <- zfill(as.numeric(days(data$chr)),2)", 
"  mt <- zfill(msm2mt(round(1440*(as.numeric(data$chr)%%1))),4)", 
"  dump <- 1", "  bot <- 0", "  code <- \"BX\"", "  stg <- 0", 
"  corstg <- 0", "  stgcode <- 1", "  q <- data$q", "  turb <- data$turb", 
"  corturb <- data$corturb", "  df <- data.frame(year,mo,dy,mt,dmp=1,bot=0,code=\"BX\",rawstg=data$q,corstg=data$q,stgcode=1,q=data$q,rawturb=data$turb,turb=data$turb,turbcode=-1)", 
"  if (!missing(extras)) {", "    for (var in extras) df[,var] <- data[,var]", 
"  }", "  if (write) write.table(df,filepath,quote=FALSE,row.names=F,col.names=F,sep=\",\")", 
"  else return(df)", "}"))
crit.loess <-
structure(function (h, x, y, degree = 2) 
{
    n <- length(y)
    l <- loess(y ~ x, span = h, degree = degree)
    df <- l$trace.hat
    if (df >= n - 2) 
        Inf
    else log(sum((predict(l, x) - y)^2)/n) + (1 + df/n)/(1 - 
        df/n - 2/n)
}, source = c("function(h, x, y, degree = 2.)", "{", "\t#", "\t# AICC criterion for loess estimator", 
"\t# obtained from http://pages.stern.nyu.edu/~jsimonof/aicc.dmp", 
"\t# See Hurvich, Simonoff, and Tsai (1998, JRSSB) for discussion.", 
"\t#", "\tn <- length(y)", "\tl <- loess(y ~ x, span = h, degree = degree)", 
"\tdf <- l$trace.hat", "\tif(df >= n - 2.)", "\t\tInf", "\telse log(sum((predict(l, x) - y)^2.)/n) + (1. + df/n)/(1. - df/n - 2./", 
"\t\t\tn)", "}"))
cubictime <-
structure(function (stn, hy, sdate, stime, edate, etime, interval = 10, 
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
}, source = c("function(stn, hy, sdate, stime, edate, etime, interval = 10, ssc1 = 0, ssc2 = 0,", 
"        long = T, adj = F, exclude = TRUE, units = \"cfs\")", 
"{", "      #  Cubic natural spline interpolation", "      #  Revised 2011feb10   Added exclude option.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  05/18/07 now calls dis.adjust to limit extrapolated adjustments", 
"      #  Extracts records from pop between given starting date/time", 
"      #  and ending date/time and merges with samples from the", 
"      #  correct station for the same time period.  Then estimates", 
"      #  the load using a natural cubic interpolating spline.", 
"      #  Assumes time interval of 10 minutes if not specified", 
"      #  ssc1 and ssc2 specify endpoint SSC when no sample exists", 
"      #      at sdate,stime or edate,etime.  No effect otherwise.", 
"      #  long=F gives abbreviated output", "      #  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"      #      adj=T does not affect the values ssc1 and ssc2", 
"      #  Set units=\"cumecs\" if discharge is in m3/sec instead of cfs", 
"      hy <- zfill(hy, 2)", "      k <- 0.06 * interval", "      pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"      pop <- subtime(pop, sdate, stime, edate, etime)", "      if(any(is.na(pop$q)))", 
"         stop(\"\\nSorry, the specified segment has missing discharge values\")", 
"      samples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"      samples <- subtime(samples, sdate, stime, edate, etime)", 
"      if (exclude & !is.null(samples$exclude))", "         samples <- samples[!samples$exclude,  ]", 
"      mergesam <- samples[, c(\"chr\", \"ssc\")]", "      if (dim(samples)[1] == 1)", 
"         mergesam <- as.data.frame(mergesam)", "      if (dim(mergesam)[1] == 0) {", 
"          mergepop <- pop", "          mergepop$ssc <- rep(NA, dim(mergepop)[1])", 
"      }", "      else {", "         mergepop <- merge(pop, mergesam, all.x = T)", 
"         mergepop <- mergepop[order(mergepop$chr),  ]", "      }", 
"      conf <- intersect(names(mergepop),objects(1))", "      if (length(conf) > 0) {", 
"          cat(\"The following objects conflict with object names in lineartime:\\n\")", 
"          cat(conf,\"\\n\")", "          cat(\"Please remove conflicting objects before proceeding.\\n\")", 
"          return(invisible())", "      }", "      attach(mergepop)", 
"      ssc0 <- ssc", "      if(adj) ssc0 <- dis.adjust(stn, ssc0)", 
"      N <- dim(mergepop)[1]", "      index <- 1:N", "      sam <- index[ - which.na(ssc0)]", 
"      n <- length(sam)", "      x <- sam", "      y <- ssc0[sam]", 
"      if (!(1 %in% x)) {", "              x <- c(1,x)", "              y <- c(ssc1,y)", 
"      }", "      if (!(N %in% x)) {", "              x <- c(x,N)", 
"              y <- c(y,ssc2)", "      }", "      cuspline <- splinefun(x, y, method=\"natural\")", 
"      predssc <- cuspline(index)", "      if(units == \"cfs\")", 
"          qfactor <- 35.31467", "      else if(units == \"cumecs\")", 
"          qfactor <- 1", "      else stop(\"flow units must be cfs or cumecs\")", 
"          disch <- pop$q/qfactor", "      # cubic meters per sec", 
"      estflux <- sum(k * disch * predssc)", "      detach(2)", 
"      if(long)", "           list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, type=\"linear\", meth = 3, chr = ", 
"                mergepop$chr, predssc = predssc)", "      else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, type=\"linear\", meth = 3)", 
"}"))
dailystats <-
structure(function (stn, hy) 
{
    data <- read.flo(stn, hy)
    day <- dates(data$chr)
    meandaily <- tapply(data$q, day, mean, na.rm = T)
    mindaily <- tapply(data$q, day, min, na.rm = T)
    maxdaily <- tapply(data$q, day, max, na.rm = T)
    date <- dates(as.numeric(names(meandaily)))
    data.frame(date = date, mean = as.vector(meandaily), min = as.vector(mindaily), 
        max = as.vector(maxdaily))
}, source = c("function(stn,hy) {", "  data <- read.flo(stn,hy)", 
"  day <- dates(data$chr)", "  meandaily <- tapply(data$q, day, mean, na.rm = T)", 
"  mindaily <- tapply(data$q, day, min, na.rm= T)", "  maxdaily <- tapply(data$q, day, max, na.rm=T)", 
"  date <- dates(as.numeric(names(meandaily)))", "  data.frame(date=date, mean=as.vector(meandaily), min=as.vector(mindaily), max=as.vector(maxdaily))", 
"}"))
density.draw <-
structure(function (panel) 
{
    plot(density(panel$x, bw = panel$h))
    panel
}, source = c("function(panel) {", "     plot(density(panel$x, bw = panel$h))", 
"     panel", "     }"))
dis.adjust <-
structure(function (stn, ssc) 
{
    S <- toupper(stn)
    if (!exists("discoef")) 
        stop("Need 'discoef' data frame to make DI sample adjustments")
    if (S %in% row.names(discoef)) {
        a <- discoef[S, "a"]
        b <- discoef[S, "b"]
        dismax <- discoef[S, "max"]
        tmp1 <- ssc[!is.na(ssc)]
        tmp2 <- exp(a) * tmp1^b
        if (!is.na(dismax) & any(tmp1 > dismax)) {
            maxfactor <- exp(a) * dismax^(b - 1)
            tmp2[tmp1 > dismax] <- maxfactor * tmp1[tmp1 > dismax]
            cat("Note: concentrations above ", dismax, "mg/L found. ")
            cat("Applying limiting DI adjustment factor of ", 
                maxfactor, "\n")
        }
        ssc[!is.na(ssc)] <- tmp2
        if (a == 0 && b == 1) 
            attr(ssc, "adjusted") <- TRUE
        else attr(ssc, "adjusted") <- FALSE
    }
    else stop(paste("Warning: station ", S, " not found in 'discoef' data frame."))
    ssc
}, source = c("function(stn,ssc) {", "   #  May 18, 2007  Adjusts pumped sample SSC using DI sample coefficients", 
"   #     Looks for coefficients in data frame 'discoef'", "   #  A limiting adjustment factor is applied when pumped SSC > discoef[S, \"max\"]", 
"   #  This forces the DIS regression to a slope of 1 above the specified maximum", 
"   S <- toupper(stn)   ", "   if (!exists(\"discoef\")) ", "         stop(\"Need 'discoef' data frame to make DI sample adjustments\")", 
"   if(S %in% row.names(discoef)) {", "         a <- discoef[S, \"a\"]", 
"         b <- discoef[S, \"b\"]", "         dismax <- discoef[S, \"max\"]", 
"         tmp1 <- ssc[!is.na(ssc)]", "         tmp2 <- exp(a) * tmp1^b", 
"         if (!is.na(dismax) & any(tmp1 > dismax)) {", "                maxfactor <- exp(a) * dismax^(b-1)", 
"                tmp2[tmp1 > dismax] <- maxfactor * tmp1[tmp1 > dismax]", 
"                cat(\"Note: concentrations above \",dismax,\"mg/L found. \")", 
"                cat(\"Applying limiting DI adjustment factor of \",maxfactor,\"\\n\")", 
"         }", "         ssc[!is.na(ssc)] <- tmp2", "         if (a == 0 && b == 1) ", 
"            attr(ssc,\"adjusted\") <- TRUE", "         else", 
"            attr(ssc,\"adjusted\") <- FALSE", "    }", "   else stop(paste(\"Warning: station \",S,\" not found in 'discoef' data frame.\"))", 
"   ssc", "}"))
display <-
structure(function (x) 
page(x, method = "print"), source = "function(x) page(x,method=\"print\")")
drop.parens <-
structure(function (x) 
{
    x1 <- sub("[(]", "", x)
    sub("[)]", "", x1)
}, source = c("function(x) {", "   x1 <- sub(\"[(]\",\"\",x)", 
"   sub(\"[)]\",\"\",x1)", "}"))
drop.storm2 <-
function (stn, hy, match = F, loc = paste(Sys.getenv("WATER"),"\\Sediment\\SuspendedSediment\\MethodsTesting_Analyses\\SuspendedSedLoads\\LoadEstimation\\hy",hy, sep = "")) 
{
    hy <- zfill(hy, 2)
    data <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    storm <- read.allstorms(stn, hy, match, loc)
    keep <- rep(T, dim(data)[1])
    for (i in 1:length(storm$number)) keep[data$chr >= storm$schron[i] & 
        data$chr <= storm$echron[i]] <- F
    data[keep, c("q", "stgcode", "turb", "turbcode", "codes")]
}
drop.storms <-
structure(function (stn, hy, match = F, source = "filesystem", 
    loc = paste(getTTSenv("TTSHOME"), "/work/hy", hy, sep = "")) 
{
    hy <- zfill(hy, 2)
    data <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    if (source == "filesystem") 
        storm <- read.allstorms(stn, hy, match, loc)
    else storm <- getStormdates(stn, hy)
    keep <- rep(T, dim(data)[1])
    for (i in 1:length(storm$number)) keep[data$chr >= storm$schron[i] & 
        data$chr <= storm$echron[i]] <- F
    print("The following periods were dropped to create interstorm data:")
    print(storm)
    data <- data[keep, c("chr", "q", "turb", "turbcode")]
    attr(data, "stn") <- stn
    data
}, source = c("function(stn, hy, match=F, source=\"filesystem\", loc = paste(getTTSenv(\"TTSHOME\"),\"/work/hy\",hy,sep=\"\"))", 
"{", "  # Revised Sep 2, 2012 added \"stn\" attribute to output data", 
"  # Reads storm dates (see read.allstorms function)", "  # into a data frame containing storm number, schron, and echron", 
"  # Returns q, turb, and turbcode for values of chr NOT in storm periods", 
"  # See read.allstorms for description of 'match' argument", 
"  # For source = \"filesystem\", file location is specified by loc argument. ", 
"  # For source = \"workspace\", storms are identified from objects created by tts.ssc()", 
"  hy <- zfill(hy, 2)", "  data <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"  if (source == \"filesystem\")", "    storm <- read.allstorms(stn,hy,match,loc)", 
"  else    # source = \"workspace\"  presumably", "    storm <- getStormdates(stn, hy)", 
"  keep <- rep(T, dim(data)[1])", "  for (i in 1:length(storm$number))", 
"    keep[data$chr >= storm$schron[i] & data$chr <= storm$echron[i]] <- F", 
"  print(\"The following periods were dropped to create interstorm data:\")", 
"  print(storm)", "  data <- data[keep, c(\"chr\", \"q\", \"turb\", \"turbcode\")]", 
"  attr(data,\"stn\") <- stn   # Save station name as attribute in case data frame is renamed", 
"  data", "}"))
drop.storms.gui <-
structure(function () 
{
    drop <- function(panel) {
        with(panel, {
            stn <- tolower(pars[1])
            hy4 <- pars[2]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- substring(hy4, 3, 4)
            setTTSenv(c("STN", "HY"), c(stn, hy4))
            ttshome <- getTTSenv("TTSHOME")
            floyy.flo <- paste("flo", hy2, ".flo", sep = "")
            trbyy.flo <- paste("trb", hy2, ".flo", sep = "")
            floyy.sed <- paste("flo", hy2, ".sed", sep = "")
            trbyy.sed <- paste("trb", hy2, ".sed", sep = "")
            rhs1 <- paste("drop.storms(\"", stn, "\",", hy2, 
                ",source=\"", datesource, "\")", sep = "")
            rhs2 <- paste("interstorm.flo[is.na(interstorm.flo$turb),]", 
                sep = "")
            rhs3 <- paste("interstorm.flo[!is.na(interstorm.flo$turb),]", 
                sep = "")
            rhs4 <- paste(stn, hy2, ".sed", sep = "")
            cmd1 <- paste("interstorm.flo", rhs1, sep = " <- ")
            cmd2 <- paste(floyy.flo, rhs2, sep = " <- ")
            cmd3 <- paste(trbyy.flo, rhs3, sep = " <- ")
            cmd4 <- paste(floyy.sed, rhs4, sep = " <- ")
            cmd5 <- paste(trbyy.sed, rhs4, sep = " <- ")
            interstorm.flo <- eval(parse(text = rhs1))
            nmissing <- sum(is.na(interstorm.flo$turb))
            if (nmissing > 0) 
                allcmds <- paste(cmd1, cmd2, cmd3, cmd4, cmd5, 
                  sep = "\n")
            else {
                allcmds <- paste(cmd1, cmd3, cmd5, sep = "\n")
                cat("No missing interstorm turbidity data. Flow-based estimates will be unnecessary.\n")
            }
            saveCommand(stn, hy2, save = savecmd, cmd = allcmds)
            res3 <- eval(parse(text = rhs3))
            res4 <- eval(parse(text = rhs4))
            assign(trbyy.flo, res3, envir = .GlobalEnv)
            assign(trbyy.sed, res4, envir = .GlobalEnv)
            if (nmissing > 0) {
                res2 <- eval(parse(text = rhs2))
                assign(floyy.flo, res2, envir = .GlobalEnv)
                assign(floyy.sed, res4, envir = .GlobalEnv)
            }
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control("Create inter-storm data sets")
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    rp.radiogroup(panel, datesource, c("filesystem", "workspace"), 
        title = "Source of storm dates", initval = "workspace", 
        action = nothing)
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save commands to file", 
        action = nothing)
    res1 <- rp.button(panel, action = drop, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "    # Creates a GUI using rpanel package to run drop.storms() and create interstorm data frames", 
"    drop <- function(panel) {", "      with(panel, {", "        stn <- tolower(pars[1])", 
"        hy4 <- pars[2]", "        if (nchar(hy4) < 3) {", "          if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"          if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"        }", "        hy2 <- substring(hy4,3,4)", "        # Save selected station and year in TTS environment", 
"        setTTSenv(c(\"STN\",\"HY\"),c(stn,hy4))", "        # Get TTSHOME environment variable", 
"        ttshome <- getTTSenv(\"TTSHOME\")", "        ", "        # Create commands    ", 
"        floyy.flo <- paste('flo',hy2,'.flo',sep=\"\")", "        trbyy.flo <- paste('trb',hy2,'.flo',sep=\"\")", 
"        floyy.sed <- paste('flo',hy2,'.sed',sep=\"\")", "        trbyy.sed <- paste('trb',hy2,'.sed',sep=\"\")", 
"        rhs1 <- paste('drop.storms(\"',stn,'\",',hy2,',source=\"',datesource,'\")',sep=\"\")", 
"        rhs2 <- paste('interstorm.flo[is.na(interstorm.flo$turb),]',sep=\"\")", 
"        rhs3 <- paste('interstorm.flo[!is.na(interstorm.flo$turb),]',sep=\"\")", 
"        rhs4 <- paste(stn,hy2,'.sed',sep=\"\")", "        cmd1 <- paste('interstorm.flo',rhs1,sep=' <- ')", 
"        cmd2 <- paste(floyy.flo,rhs2, sep=' <- ')", "        cmd3 <- paste(trbyy.flo,rhs3, sep=' <- ')", 
"        cmd4 <- paste(floyy.sed,rhs4, sep=' <- ')", "        cmd5 <- paste(trbyy.sed,rhs4, sep=' <- ')", 
"     ", "        # Save and Run the commands", "        # Create a trbyy.flo and a trbyy.sed data frame for interstorm estimation", 
"        # If any interstorm turb data missing, also create floyy.flo and floyy.sed", 
"        interstorm.flo <- eval(parse(text=rhs1))   # This is command 1", 
"        nmissing <- sum(is.na(interstorm.flo$turb))", "        if (nmissing > 0)", 
"          allcmds <- paste(cmd1,cmd2,cmd3,cmd4,cmd5,sep=\"\\n\")", 
"        else {", "          allcmds <- paste(cmd1,cmd3,cmd5, sep=\"\\n\")", 
"          cat(\"No missing interstorm turbidity data. Flow-based estimates will be unnecessary.\\n\")", 
"        }", "        saveCommand(stn,hy2,save=savecmd,cmd=allcmds)", 
"        # Run the commands", "", "        res3 <- eval(parse(text=rhs3))", 
"        res4 <- eval(parse(text=rhs4))", "        assign(trbyy.flo, res3, envir=.GlobalEnv)", 
"        assign(trbyy.sed, res4, envir=.GlobalEnv)", "        if (nmissing > 0) {", 
"          res2 <- eval(parse(text=rhs2))", "          assign(floyy.flo, res2, envir=.GlobalEnv)", 
"          assign(floyy.sed, res4, envir=.GlobalEnv)", "        }", 
"      }", "      )", "      panel", "    }", "    nothing <- function(panel) panel", 
"    panel <- rp.control(\"Create inter-storm data sets\")", 
"    env <- getTTSenv()", "    init.stn <- getTTSenv(\"STN\")", 
"    init.hy <- getTTSenv(\"HY\")", "    my.textentry(panel, pars, action = nothing, labels=c(\"Station\",\"Water year\"), ", 
"                 title=\"Select station and year\",initval = c(init.stn, init.hy))", 
"    rp.radiogroup(panel, datesource, c(\"filesystem\",\"workspace\"), ", 
"                  title=\"Source of storm dates\", initval=\"workspace\", action=nothing)", 
"    rp.checkbox(panel, savecmd, initval = TRUE, ", "                label = \"Save commands to file\",action=nothing)", 
"    res1 <- rp.button(panel, action = drop, title=\"OK\", pos=\"left\",quit=FALSE)", 
"    rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"    rp.do(panel,nothing)", "  }"))
duan.cbrt <-
structure(function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    result <- yhat^3 + 3 * yhat * mean(res^2)
    list(naive = yhat^3, corrected = result)
}, source = c("function(fit, newdata)", "{", "\t# Make predictions for y when the variable fitted was y^(1/3)", 
"\t# Use Duan's non-parametric smearing estimator", "\tyhat <- predict(fit, newdata = newdata)", 
"\tres <- resid(fit)", "\tresult <- yhat^3 + 3 * yhat * mean(res^2)", 
"\tlist(naive = yhat^3, corrected = result)", "}"))
duan.log <-
structure(function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    naive <- exp(yhat)
    result <- naive * mean(exp(res))
    list(naive = naive, corrected = result)
}, source = c("function(fit, newdata)", "{", "\t# Make predictions for y when the variable fitted was log(y)", 
"\t# Use Duan's non-parametric smearing estimator", "\tyhat <- predict(fit, newdata = newdata)", 
"\tres <- resid(fit)", "\tnaive <- exp(yhat)", "\tresult <- naive * mean(exp(res))", 
"\tlist(naive = naive, corrected = result)", "}"))
duan.sqrt <-
structure(function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    result <- yhat^2 + 2 * yhat * mean(res) + mean(res^2)
    result[yhat < 0] <- 0
    list(naive = yhat^2, corrected = result)
}, source = c("function(fit, newdata)", "{", "\t# Make predictions for y when the variable fitted was sqrt(y)", 
"\t# Use Duan's non-parametric smearing estimator", "\t# This version modified so negative values are backtransformed to zero", 
"\tyhat <- predict(fit, newdata = newdata)", "\tres <- resid(fit)", 
"\tresult <- yhat^2 + 2 * yhat * mean(res) + mean(res^2)", "\tresult[yhat < 0] <- 0", 
"\tlist(naive = yhat^2, corrected = result)", "}"))
electStnYear <-
structure(function () 
{
    initializeDialog(title = gettextRcmdr("Select station and year"))
    onOK <- function() {
        stn <- tclvalue(stnValue)
        hy <- tclvalue(hyValue)
        closeDialog()
        doItAndPrint(paste("hy =", hy, "\nstn =", stn))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    stnFrame <- tkframe(top)
    stnValue <- tclVar(ifelse(exists("stn", env = .GlobalEnv), 
        hy, ""))
    stnField <- tkentry(stnFrame, width = "6", textvariable = stnValue)
    hyFrame <- tkframe(top)
    hyValue <- tclVar(ifelse(exists("hy", env = .GlobalEnv), 
        hy, ""))
    tkgrid(tklabel(stnFrame, text = gettextRcmdr("Station Name"), 
        fg = "blue"))
    tkgrid(stnField, sticky = "w")
    tkgrid(tklabel(hyFrame, text = gettextRcmdr("Hydro Year"), 
        fg = "blue"))
    tkgrid(hyField, sticky = "w")
    tkgrid(stnFrame, hyFrame, sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix(rows = 2, columns = 2)
}, source = c("function() {", "   initializeDialog(title=gettextRcmdr(\"Select station and year\"))", 
"   onOK <- function() {", "       stn <- tclvalue(stnValue)", 
"       hy <- tclvalue(hyValue)", "       closeDialog()", "       doItAndPrint(paste(\"hy =\",hy,\"\\nstn =\",stn))", 
"       tkfocus(CommanderWindow())", "   }", "   OKCancelHelp()", 
"   stnFrame <- tkframe(top)", "   stnValue <- tclVar(ifelse(exists(\"stn\", env=.GlobalEnv),hy,\"\"))", 
"   stnField <- tkentry(stnFrame, width=\"6\", textvariable=stnValue)", 
"   hyFrame <- tkframe(top)", "   hyValue <- tclVar(ifelse(exists(\"hy\", env=.GlobalEnv),hy,\"\"))", 
"   tkgrid(tklabel(stnFrame,text=gettextRcmdr(\"Station Name\"), fg=\"blue\"))", 
"   tkgrid(stnField, sticky=\"w\")", "   tkgrid(tklabel(hyFrame,text=gettextRcmdr(\"Hydro Year\"), fg=\"blue\"))", 
"   tkgrid(hyField, sticky=\"w\")", "   tkgrid(stnFrame,hyFrame, sticky=\"nw\")", 
"   tkgrid(buttonsFrame, columnspan=2, sticky=\"w\")", "   dialogSuffix(rows=2, columns=2)", 
"}"))
endpoints <-
structure(function (object, all = F) 
{
    ssc <- object$predssc
    c(start = ssc[1], end = last.val(ssc))
}, source = c("function(object, all = F)", "{", "\t# Show first and last predicted turbidity from a tts or msc object", 
"\tssc <- object$predssc", "\tc(start = ssc[1], end = last.val(ssc))", 
"}"))
exceed <-
structure(function (x, plevels = c(0.001, 0.01, 0.02, 0.05, 0.1), 
    varname = "ssc", ssclevels = c(25, 50, 100, 200, 500, 1000), 
    interval = 10, months, leap) 
{
    daysperyear <- sumdays(months, leap)
    nx <- length(x)
    px <- nx/daysperyear/(1440/interval)
    if (any(px > 1)) 
        stop("Period of x exceeds specified number of days per year")
    q <- quantile(x, 1 - plevels/px)
    names(q) <- plevels
    counts <- sapply(ssclevels, function(ssc, data) sum(x > ssc), 
        data = x)
    hrs <- counts * interval/60
    names(hrs) <- ssclevels
    xlogical <- sapply(ssclevels, function(level, x) x >= level, 
        x = x)
    runlen <- apply(xlogical, 2, rle)
    max.hrs <- sapply(runlen, function(rln) {
        true.length <- rln$lengths[rln$values]
        if (length(true.length) == 0) 
            return(0)
        else return(max(rln$lengths[rln$values]) * interval/60)
    })
    names(max.hrs) <- ssclevels
    result <- list(ssc.exceeded.at.given.p = q, hours.exceeded.at.given.ssc = hrs, 
        max.continuous.hours = max.hrs)
    if (varname != "ssc") {
        name1 <- paste(varname, "exceeded.at.given.p", sep = ".")
        name2 <- paste("hours.exceeded.at.given", varname, sep = ".")
        name3 <- names(result)[3]
        names(result) <- c(name1, name2, name3)
    }
    result
}, source = c("function(x, plevels=c(0.001,0.01,0.02,0.05,0.10), varname=\"ssc\",", 
"              ssclevels=c(25,50,100,200,500,1000), interval=10, months, leap) {", 
"# Calculate exceedence value corresponding to given percentages of time", 
"# Calculate hours exceeded for given values of SSC (or any variable)", 
"# Calculate maximum continuous hours of exposure at given values or higher", 
"  daysperyear <- sumdays(months,leap)", "  nx <- length(x)", 
"  px <- nx/daysperyear/(1440/interval)", "  if (any(px > 1))", 
"      stop(\"Period of x exceeds specified number of days per year\")", 
"  q <- quantile(x,1-plevels/px)", "  names(q) <- plevels", "  counts <- sapply(ssclevels, function(ssc, data) sum(x>ssc), data = x)", 
"  hrs <- counts*interval/60", "  names(hrs) <- ssclevels     ", 
"  # Create matrix of logicals for exceeding each SSC level", 
"  xlogical <- sapply(ssclevels, function(level, x) x >= level, x=x)", 
"  # Determine run lengths for each ", "  runlen <- apply(xlogical, 2, rle)", 
"  # Extract maximum run length for TRUE (x >= level) in each component", 
"  max.hrs <- sapply(runlen, function(rln) {", "     true.length <- rln$lengths[rln$values]", 
"     if (length(true.length) == 0)", "         return(0)", "     else", 
"         return(max(rln$lengths[rln$values])*interval/60)", 
"  })", "  names(max.hrs) <- ssclevels", "  result <- list(ssc.exceeded.at.given.p = q, hours.exceeded.at.given.ssc= hrs, ", 
"       max.continuous.hours = max.hrs)", "  if (varname != \"ssc\") {", 
"       name1 <- paste(varname,\"exceeded.at.given.p\",sep=\".\")", 
"       name2 <- paste(\"hours.exceeded.at.given\",varname,sep=\".\")", 
"       name3 <- names(result)[3]", "       names(result) <- c(name1,name2,name3)", 
"  }", "  result", "}"))
ezchron <-
structure(function (date, time) 
chron(date, paste(time, "00", sep = ":")), source = "function(date,time) chron(date,paste(time,\"00\",sep=\":\"))")
ezchron2 <-
structure(function (date.time) 
{
    require(chron)
    timelist <- strsplit(date.time, " ")
    date <- sapply(timelist, "[", 1)
    time <- sapply(timelist, "[", 2)
    time <- paste(time, "00", sep = ":")
    chron(date, time)
}, source = c("function(date.time) {", "  require(chron)", "  timelist <- strsplit(date.time,\" \")", 
"  date <- sapply(timelist,\"[\",1)", "  time <- sapply(timelist,\"[\",2)", 
"  time <- paste(time,\"00\",sep=\":\")", "  chron(date,time)", 
"}"))
fac2num <-
structure(function (x) 
as.numeric(as.character(x)), source = "function(x) as.numeric(as.character(x))")
find.chardata <-
structure(function (x) 
{
    lev <- levels(x)
    lev <- lev[lev != ""]
    oldopts <- options(warn = -1)
    lev.indices <- which.na(as.numeric(lev))
    options(warn = oldopts$warn)
    chardata <- lev[lev.indices]
    x.indices <- which(x %in% chardata)
    list(values = chardata, indexes = x.indices)
}, source = c("function (x)", "{", "# Finds non-numeric values in levels of a factor", 
"# and subscripts of data belonging to that level", "\tlev <- levels(x)", 
"\tlev <- lev[lev != \"\"]", "\toldopts <- options(warn=-1)", 
"\tlev.indices <- which.na(as.numeric(lev))", "\toptions(warn=oldopts$warn)", 
"\tchardata <- lev[lev.indices]", "\tx.indices <- which(x %in% chardata)", 
"\tlist(values=chardata, indexes=x.indices)", "}"))
find.factors <-
structure(function (data) 
{
    modes <- sapply(data, data.class)
    factor.names <- names(modes)[modes == "factor"]
    if (length(factor.names) > 0) {
        cat("Error: character data found in", paste(factor.names, 
            collapse = ", "), "\n\n")
        for (fac in factor.names) {
            list <- find.chardata(data[, fac])
            names(list) <- paste(fac, names(list), sep = ".")
            print(list)
        }
    }
    return()
}, source = c("function(data) ", "{", "#  Finds factors in a data frame and reports an error for any found", 
"    modes <- sapply(data,data.class)", "    factor.names <- names(modes)[modes==\"factor\"]", 
"    if (length(factor.names) > 0) {", "       cat(\"Error: character data found in\",paste(factor.names,collapse=\", \"),\"\\n\\n\")", 
"       for (fac in factor.names) {", "\t   list <- find.chardata(data[,fac])", 
"           names(list) <- paste(fac,names(list),sep=\".\")", 
"\t   print(list)", "       }", "    }", "    return()", "}"))
finney <-
structure(function (m, z) 
{
    N <- length(z)
    if (length(m) == 1) 
        m <- rep(m, N)
    result <- rep(1, N)
    goodones <- (abs(z) < 50) & (m > 0)
    if (sum(goodones) > 0) 
        result[goodones] <- finney2(m[goodones], z[goodones])
    result
}, source = c("function(m, z)", "{", "\t#  Computes Finney's gm(z) function for Cohn's MVUE estimator", 
"\t#  Input and output are vectors.", "\tN <- length(z)", "\tif(length(m) == 1)", 
"\t\tm <- rep(m, N)", "\tresult <- rep(1,N)", "\tgoodones <- (abs(z) < 50) & (m > 0)", 
"\tif(sum(goodones) > 0)", "\t\tresult[goodones] <- finney2(m[goodones], z[goodones])", 
"\tresult", "}"))
finney2 <-
structure(function (m, z) 
{
    apply(cbind(m, z), 1, function(row) {
        df <- row[1]
        bt <- (row[2] * (df^2))/(2 * (df + 1))
        p <- 1:100
        series <- bt/((df/2 + p - 1) * p)
        terms <- cumprod(series)
        if (abs(terms[length(terms)] > 1e-07)) 
            print("Finney's gm did not converge")
        return(1 + sum(terms))
    })
}, source = c("function(m, z)", "{", "\t# Called by finney() to compute Finney's gm", 
"\tapply(cbind(m, z), 1, function(row)", "\t    {", "\t\tdf <- row[1]", 
"\t\tbt <- (row[2] * (df^2))/(2 * (df + 1))", "\t\tp <- 1:100", 
"\t\tseries <- bt/((df/2 + p - 1) * p)", "\t\tterms <- cumprod(series)", 
"\t\tif(abs(terms[length(terms)] > 1e-07))", "\t\t\tprint(\"Finney's gm did not converge\")", 
"\t\treturn(1 + sum(terms))", "\t    }", "\t)", "}"))
floplot.gui <-
structure(function (path = getTTSenv("TTSHOME")) 
{
    require(tcltk) || stop("tcltk support is absent")
    options(warn = -1)
    require(chron) || stop("chron support is absent")
    options(warn = 0)
    cat("\n\nWelcome to TTS FLO PLOT...\n")
    while (1) {
        stn.info <- select.station(path)
        if (is.null(stn.info)) 
            return(invisible())
        stn <- stn.info$stn
        if (stn == "") 
            break
        filepath <- paste(path, stn, sep = "/")
        files <- list.files(filepath)
        flofile <- select.flofile(stn, files)
        if (is.null(flofile)) 
            next
        flopath <- paste(filepath, flofile, sep = "/")
        flodata <- read.flofile(flopath, stn.info$extravars)
        attr(flodata, "stn") <- stn
        vars <- names(flodata)
        plot <- function() {
            if (tclvalue(choice) != "none") {
                tflag <- var.check(tclvar)
                if (tflag == T) {
                  schron <- chron(tclvalue(sd), mt2msm(as.numeric(tclvalue(st)))/1440)
                  echron <- chron(tclvalue(ed), mt2msm(as.numeric(tclvalue(et)))/1440)
                  if (tclvalue(choice) == "stage") 
                    flodata$left <- flodata$stg
                  if (tclvalue(choice) == "discharge") 
                    flodata$left <- flodata$q
                  if (tclvalue(choice) == "turbidity") 
                    flodata$left <- flodata$turb
                  if (tclvalue(choice) == "stage-turbidity") {
                    flodata$left <- flodata$stg
                    flodata$right <- flodata$turb
                  }
                  if (tclvalue(choice) == "discharge-turbidity") {
                    flodata$left <- flodata$q
                    flodata$right <- flodata$turb
                  }
                  if (tclvalue(choice) == "rainfall") 
                    flodata$left <- flodata$rain
                  if (tclvalue(choice) == "discharge-rainfall") {
                    flodata$left <- flodata$q
                    flodata$right <- flodata$rain
                  }
                  if (tclvalue(choice) == "water temperature") 
                    flodata$left <- flodata$wtemp
                  if (tclvalue(choice) == "air temperature") 
                    flodata$left <- flodata$atemp
                  if (tclvalue(choice) == "water-air temperature") {
                    flodata$left <- flodata$wtemp
                    flodata$right <- flodata$atemp
                  }
                  if (tclvalue(choice) == "discharge-water temperature") {
                    flodata$left <- flodata$q
                    flodata$right <- flodata$wtemp
                  }
                  if (tclvalue(choice) == "throughfall") 
                    flodata$left <- flodata$lc1
                  if (tclvalue(choice) == "wind speed") 
                    flodata$left <- flodata$wind
                  flodata <- as.data.frame(flodata)
                  flodata <- flodata[flodata$chron >= schron & 
                    flodata$chron <= echron, ]
                  tts.rawplot(flodata, tclvar, schron, echron, 
                    stn.info$minstg)
                  tkraise(gui)
                }
            }
            else {
                winDialog(type = c("ok"), "ERROR: You must first choose the type of data you wish to\nplot from the Data Type pull down menu.")
            }
        }
        def.time <- function() {
            if (tclvalue(sd) == "m/d/y") {
                tclvalue(sd) <- format(dates(flodata$chron[1]))
                tclvalue(st) <- flodata$time[1]
            }
            else {
                check.sd <- try(dates(tclvalue(sd)))
                if (inherits(check.sd, "try-error")) {
                  tclvalue(sd) <- format(dates(flodata$chron[1]))
                  tclvalue(st) <- flodata$time[1]
                  winDialog(type = c("ok"), "ERROR: Defaulted to flodatas start date\ndue to an incorrect date field (mm/dd/yy).")
                }
            }
            if (tclvalue(ed) == "m/d/y") {
                tclvalue(ed) <- format(dates(flodata$chron[length(flodata$chron)]))
                tclvalue(et) <- flodata$time[length(flodata$time)]
            }
            else {
                check.ed <- try(dates(tclvalue(ed)))
                if (inherits(check.ed, "try-error")) {
                  tclvalue(ed) <- format(dates(flodata$chron[length(flodata$chron)]))
                  tclvalue(et) <- flodata$time[length(flodata$time)]
                  winDialog(type = c("ok"), "ERROR: Defaulted to flodatas end date\ndue to an incorrect date field (mm/dd/yy).")
                }
            }
        }
        cmd1 <- function() {
            time <- def.time()
            tclvalue(choice) <- "stage"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$stg, na.rm = T)
            tclvalue(max1) <- max(flodata$stg, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd1q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd2 <- function() {
            time <- def.time()
            tclvalue(choice) <- "turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$turb, na.rm = T)
            tclvalue(max1) <- max(flodata$turb, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd3 <- function() {
            time <- def.time()
            tclvalue(choice) <- "stage-turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$stg, na.rm = T)
            tclvalue(max1) <- max(flodata$stg, na.rm = T)
            tclvalue(min2) <- min(flodata$turb, na.rm = T)
            tclvalue(max2) <- max(flodata$turb, na.rm = T)
        }
        cmd3q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge-turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- min(flodata$turb, na.rm = T)
            tclvalue(max2) <- max(flodata$turb, na.rm = T)
        }
        cmd4 <- function() {
            time <- def.time()
            tclvalue(choice) <- "rainfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- 0
            tclvalue(max1) <- sum(flodata$rain, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd4q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge-rainfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- 0
            tclvalue(max2) <- sum(flodata$rain, na.rm = T)
        }
        cmd5 <- function() {
            time <- def.time()
            tclvalue(choice) <- "water temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$wtemp, na.rm = T)
            tclvalue(max1) <- max(flodata$wtemp, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd5q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge-water temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- min(flodata$wtemp, na.rm = T)
            tclvalue(max2) <- max(flodata$wtemp, na.rm = T)
        }
        cmd6 <- function() {
            time <- def.time()
            tclvalue(choice) <- "air temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$atemp, na.rm = T)
            tclvalue(max1) <- max(flodata$atemp, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd7 <- function() {
            time <- def.time()
            tclvalue(choice) <- "water-air temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$wtemp, na.rm = T)
            tclvalue(max1) <- max(flodata$wtemp, na.rm = T)
            tclvalue(min2) <- min(flodata$atemp, na.rm = T)
            tclvalue(max2) <- max(flodata$atemp, na.rm = T)
        }
        cmd8 <- function() {
            time <- def.time()
            tclvalue(choice) <- "throughfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$lc1, na.rm = T)
            tclvalue(max1) <- max(flodata$lc7, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd9 <- function() {
            time <- def.time()
            tclvalue(choice) <- "wind speed"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$wind, na.rm = T)
            tclvalue(max1) <- max(flodata$wind, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        gui <- tktoplevel()
        tkwm.title(gui, "TTS DATA PLOT")
        labpath <- tklabel(gui, text = "Path name:")
        pathname <- tclVar(flopath)
        entpath <- tkentry(gui, textvariable = as.character(pathname), 
            justify = "center", background = "grey", state = "disabled")
        tkpack(labpath, anchor = "w")
        tkpack(entpath, fill = "both", padx = 2)
        tkpack(mb <- tkmenubutton(gui, text = "Data Type", underline = "0", 
            relief = "raised"), fill = "both", pady = 4, padx = 2)
        mb.menu <- tkmenu(mb)
        tkconfigure(mb, menu = paste(tcl("winfo", "parent", mb.menu), 
            ".", tcl("winfo", "name", mb.menu), sep = ""))
        if (any(vars == "stg")) 
            tkadd(mb.menu, "command", label = "stage", command = cmd1)
        if (any(vars == "q")) 
            tkadd(mb.menu, "command", label = "discharge", command = cmd1q)
        if (any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "turbidity", command = cmd2)
        if (any(vars == "stg") & any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "stage-turbidity", 
                command = cmd3)
        if (any(vars == "q") & any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "discharge-turbidity", 
                command = cmd3q)
        if (any(vars == "rain")) 
            tkadd(mb.menu, "command", label = "rainfall", command = cmd4)
        if (any(vars == "q") & any(vars == "rain")) 
            tkadd(mb.menu, "command", label = "discharge-rainfall", 
                command = cmd4q)
        if (any(vars == "wtemp")) 
            tkadd(mb.menu, "command", label = "water temperature", 
                command = cmd5)
        if (any(vars == "q") & any(vars == "wtemp")) 
            tkadd(mb.menu, "command", label = "discharge-water temperature", 
                command = cmd5q)
        if (any(vars == "atemp")) 
            tkadd(mb.menu, "command", label = "air temperature", 
                command = cmd6)
        if (any(vars == "wtemp") & any(vars == "atemp")) 
            tkadd(mb.menu, "command", label = "water-air temperature", 
                command = cmd7)
        if (any(vars == "lc1") | any(vars == "lc2") | any(vars == 
            "lc3") | any(vars == "lc4") | any(vars == "lc5") | 
            any(vars == "lc6") | any(vars == "lc7")) 
            tkadd(mb.menu, "command", label = "throughfall", 
                command = cmd8)
        if (any(vars == "wind")) 
            tkadd(mb.menu, "command", label = "wind speed", command = cmd9)
        choice <- tclVar("none")
        entchoice <- tkentry(gui, textvariable = as.character(choice), 
            justify = "center", background = "grey", state = "disabled")
        tkpack(entchoice, fill = "both", padx = 2)
        big.frm <- tkframe(gui)
        datl.frm <- tkframe(big.frm)
        datr.frm <- tkframe(big.frm)
        labsd <- tklabel(datl.frm, text = "Start Date")
        sd <- tclVar("m/d/y")
        entsd <- tkentry(datl.frm, textvariable = as.character(sd), 
            width = 10, justify = "center", background = "white")
        tkpack(labsd, entsd)
        labst <- tklabel(datr.frm, text = "Start Time")
        st <- tclVar(0)
        entst <- tkentry(datr.frm, textvariable = as.character(st), 
            width = 10, justify = "center", background = "white")
        tkpack(labst, entst)
        labed <- tklabel(datl.frm, text = "End Date")
        ed <- tclVar("m/d/y")
        ented <- tkentry(datl.frm, textvariable = as.character(ed), 
            width = 10, justify = "center", background = "white")
        tkpack(labed, ented)
        labet <- tklabel(datr.frm, text = "End Time")
        et <- tclVar(2400)
        entet <- tkentry(datr.frm, textvariable = as.character(et), 
            width = 10, justify = "center", background = "white")
        tkpack(labet, entet)
        tkpack(datl.frm, datr.frm, side = "left")
        tkpack(big.frm, pady = 2, padx = 2)
        pleft <- tclVar(0)
        tkpack(tkcheckbutton(gui, text = "Left Axis", variable = as.character(pleft), 
            state = "disabled"), anchor = "w", padx = 1)
        min1.frm <- tkframe(gui)
        labmin1 <- tklabel(min1.frm, text = "axis min", anchor = "w")
        min1 <- tclVar("NA")
        entmin1 <- tkentry(min1.frm, textvariable = as.character(min1), 
            width = 6, justify = "center", background = "white")
        tkpack(entmin1, labmin1, side = "left")
        tkpack(min1.frm, anchor = "w", padx = 2)
        max1.frm <- tkframe(gui)
        labmax1 <- tklabel(max1.frm, text = "axis max", anchor = "w")
        max1 <- tclVar("NA")
        entmax1 <- tkentry(max1.frm, textvariable = as.character(max1), 
            width = 6, justify = "center", background = "white")
        tkpack(entmax1, labmax1, side = "left")
        tkpack(max1.frm, anchor = "w", padx = 2)
        pright <- tclVar(0)
        tkpack(tkcheckbutton(gui, text = "Right Axis", variable = as.character(pright), 
            state = "disabled"), anchor = "w", padx = 1)
        min2.frm <- tkframe(gui)
        labmin2 <- tklabel(min2.frm, text = "axis min")
        min2 <- tclVar("NA")
        entmin2 <- tkentry(min2.frm, textvariable = as.character(min2), 
            width = 6, justify = "center", background = "white")
        tkpack(entmin2, labmin2, side = "left")
        tkpack(min2.frm, anchor = "w", padx = 2)
        max2.frm <- tkframe(gui)
        labmax2 <- tklabel(max2.frm, text = "axis max")
        max2 <- tclVar("NA")
        entmax2 <- tkentry(max2.frm, textvariable = as.character(max2), 
            width = 6, justify = "center", background = "white")
        tkpack(entmax2, labmax2, side = "left")
        tkpack(max2.frm, anchor = "w", padx = 2)
        tclvar <- list(choice = choice, sd = sd, ed = ed, st = st, 
            et = et, min1 = min1, max1 = max1, min2 = min2, max2 = max2, 
            pright = pright, pleft = pleft)
        done <- tclVar(0)
        but.frm <- tkframe(gui)
        plot.but <- tkbutton(but.frm, text = "PLOT", command = plot)
        quit.but <- tkbutton(but.frm, text = "QUIT", command = function() tclvalue(done) <- 1)
        tkpack(plot.but, quit.but, fill = "both")
        tkpack(but.frm, fill = "both", pady = 4, padx = 2)
        tkraise(gui)
        tkbind(gui, "<Destroy>", function() tclvalue(done) <- 2)
        tkwait.variable(as.character(done))
        if (tclvalue(done) == "2") {
            print("GUI aborted")
            return(invisible())
        }
        if (!is.null(dev.list())) {
            dev.off()
        }
        tkdestroy(gui)
    }
}, source = c("function (path=getTTSenv(\"TTSHOME\")) ", "{", 
"#", "# FLOPLOT.GUI()", "# Plotting routine developed for data in TTS FLO file.  ", 
"# Program requires both the tcltk and chron library's.", "#", 
"# Adapted from Jason C. Fisher's RAWPLOT.GUI (formerly TTS.GUI)", 
"# Initial file selection replaced with station and year selection", 
"# Added several new choices for plotting discharge", "# Changed all occurrences of tkcmd (defunct with R 2.4.0) to tcl", 
"# Version 1.0  (10/15/2007) J.Lewis", "require(tcltk) || stop(\"tcltk support is absent\")", 
"options(warn=-1)", "require(chron) || stop(\"chron support is absent\")", 
"options(warn=0)", "cat(\"\\n\\nWelcome to TTS FLO PLOT...\\n\")", 
"while (1) {", "    stn.info<-select.station(path)", "    if (is.null(stn.info)) return(invisible())", 
"    stn <- stn.info$stn", "    if (stn == \"\") break", "    # Get names and years of FLO files at this location", 
"    filepath <- paste(path,stn,sep=\"/\")", "    files <- list.files(filepath)", 
"    flofile <- select.flofile(stn,files)", "    if (is.null(flofile)) next", 
"    flopath <- paste(filepath,flofile,sep=\"/\")", "    flodata<-read.flofile(flopath,stn.info$extravars)", 
"    attr(flodata,\"stn\") <- stn", "    vars<-names(flodata)", 
"    plot <- function() {", "      if(tclvalue(choice)!=\"none\") {", 
"        tflag<-var.check(tclvar)", "        if(tflag==T) {", 
"          schron<-chron(tclvalue(sd),mt2msm(as.numeric(tclvalue(st)))/1440)", 
"          echron<-chron(tclvalue(ed),mt2msm(as.numeric(tclvalue(et)))/1440)", 
"          if(tclvalue(choice)==\"stage\") flodata$left<-flodata$stg", 
"          if(tclvalue(choice)==\"discharge\") flodata$left<-flodata$q", 
"          if(tclvalue(choice)==\"turbidity\") flodata$left<-flodata$turb", 
"          if(tclvalue(choice)==\"stage-turbidity\") {", "            flodata$left<-flodata$stg", 
"            flodata$right<-flodata$turb", "          }", "          if(tclvalue(choice)==\"discharge-turbidity\") {", 
"            flodata$left<-flodata$q", "            flodata$right<-flodata$turb", 
"          }", "          if(tclvalue(choice)==\"rainfall\") flodata$left<-flodata$rain", 
"          if(tclvalue(choice)==\"discharge-rainfall\") {", "            flodata$left<-flodata$q", 
"            flodata$right<-flodata$rain", "          }", "          if(tclvalue(choice)==\"water temperature\") flodata$left<-flodata$wtemp", 
"          if(tclvalue(choice)==\"air temperature\") flodata$left<-flodata$atemp", 
"          if(tclvalue(choice)==\"water-air temperature\") {", 
"            flodata$left<-flodata$wtemp", "            flodata$right<-flodata$atemp", 
"          }", "          if(tclvalue(choice)==\"discharge-water temperature\") {", 
"            flodata$left<-flodata$q", "            flodata$right<-flodata$wtemp", 
"          }", "          if(tclvalue(choice)==\"throughfall\") flodata$left<-flodata$lc1", 
"          if(tclvalue(choice)==\"wind speed\") flodata$left<-flodata$wind", 
"          flodata<-as.data.frame(flodata)", "          flodata<-flodata[flodata$chron>=schron&flodata$chron<=echron, ]", 
"          tts.rawplot(flodata,tclvar,schron,echron,stn.info$minstg)", 
"          tkraise(gui)      ", "        }", "      }", "      else {", 
"        winDialog(type=c(\"ok\"),\"ERROR: You must first choose the type of data you wish to\\nplot from the Data Type pull down menu.\")", 
"      }", "    }", "    def.time<-function() {", "      if(tclvalue(sd)==\"m/d/y\") {", 
"        tclvalue(sd)<-format(dates(flodata$chron[1]))", "        tclvalue(st)<-flodata$time[1]  ", 
"      }", "      else {", "        check.sd<-try(dates(tclvalue(sd)))", 
"        if(inherits(check.sd,\"try-error\")) {", "          tclvalue(sd)<-format(dates(flodata$chron[1]))", 
"          tclvalue(st)<-flodata$time[1]  ", "          winDialog(type=c(\"ok\"),\"ERROR: Defaulted to flodatas start date\\ndue to an incorrect date field (mm/dd/yy).\")      ", 
"        }", "      }", "      if(tclvalue(ed)==\"m/d/y\") {", 
"        tclvalue(ed)<-format(dates(flodata$chron[length(flodata$chron)]))", 
"        tclvalue(et)<-flodata$time[length(flodata$time)]  ", 
"      }", "      else {", "        check.ed<-try(dates(tclvalue(ed)))", 
"        if(inherits(check.ed,\"try-error\")) {", "          tclvalue(ed)<-format(dates(flodata$chron[length(flodata$chron)]))", 
"          tclvalue(et)<-flodata$time[length(flodata$time)] ", 
"          winDialog(type=c(\"ok\"),\"ERROR: Defaulted to flodatas end date\\ndue to an incorrect date field (mm/dd/yy).\")", 
"        }       ", "      }", "    }", "#    print<-function() {", 
"#      win.print(width=6,height=5,pointsize=12)", "#      win.print()", 
"#      plot()", "#      dev.off()", "#    }", "    cmd1<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"stage\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(flodata$stg,na.rm=T)", "      tclvalue(max1)<-max(flodata$stg,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd1q<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"discharge\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<-min(flodata$q,na.rm=T)", 
"      tclvalue(max1)<-max(flodata$q,na.rm=T)", "      tclvalue(min2)<-\"NA\"", 
"      tclvalue(max2)<-\"NA\"", "    }", "    cmd2<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"turbidity\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(flodata$turb,na.rm=T)", "      tclvalue(max1)<-max(flodata$turb,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd3<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"stage-turbidity\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 1", "      tclvalue(min1)<-min(flodata$stg,na.rm=T)", 
"      tclvalue(max1)<-max(flodata$stg,na.rm=T)", "      tclvalue(min2)<-min(flodata$turb,na.rm=T)", 
"      tclvalue(max2)<-max(flodata$turb,na.rm=T)", "    }", "    cmd3q<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"discharge-turbidity\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 1", 
"      tclvalue(min1)<-min(flodata$q,na.rm=T)", "      tclvalue(max1)<-max(flodata$q,na.rm=T)", 
"      tclvalue(min2)<-min(flodata$turb,na.rm=T)", "      tclvalue(max2)<-max(flodata$turb,na.rm=T)", 
"    }", "    cmd4<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"rainfall\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<- 0", "      tclvalue(max1)<- sum(flodata$rain,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd4q<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"discharge-rainfall\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 1", "      tclvalue(min1)<-min(flodata$q,na.rm=T)", 
"      tclvalue(max1)<-max(flodata$q,na.rm=T)", "      tclvalue(min2)<- 0", 
"      tclvalue(max2)<- sum(flodata$rain,na.rm=T)", "    }", 
"    cmd5<-function() {", "      time<-def.time()", "      tclvalue(choice)<-\"water temperature\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(flodata$wtemp,na.rm=T)", "      tclvalue(max1)<-max(flodata$wtemp,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd5q<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"discharge-water temperature\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 1", "      tclvalue(min1)<-min(flodata$q,na.rm=T)", 
"      tclvalue(max1)<-max(flodata$q,na.rm=T)", "      tclvalue(min2)<-min(flodata$wtemp,na.rm=T)", 
"      tclvalue(max2)<-max(flodata$wtemp,na.rm=T)", "    }", 
"    cmd6<-function() {", "      time<-def.time()", "      tclvalue(choice)<-\"air temperature\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(flodata$atemp,na.rm=T)", "      tclvalue(max1)<-max(flodata$atemp,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd7<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"water-air temperature\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 1", "      tclvalue(min1)<-min(flodata$wtemp,na.rm=T)", 
"      tclvalue(max1)<-max(flodata$wtemp,na.rm=T)", "      tclvalue(min2)<-min(flodata$atemp,na.rm=T)", 
"      tclvalue(max2)<-max(flodata$atemp,na.rm=T)", "    }", 
"    cmd8<-function() {", "      time<-def.time()", "      tclvalue(choice)<-\"throughfall\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(flodata$lc1,na.rm=T)", "      tclvalue(max1)<-max(flodata$lc7,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd9<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"wind speed\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<-min(flodata$wind,na.rm=T)", 
"      tclvalue(max1)<-max(flodata$wind,na.rm=T)", "      tclvalue(min2)<-\"NA\"", 
"      tclvalue(max2)<-\"NA\"", "    }", "    gui <- tktoplevel()", 
"    tkwm.title(gui, \"TTS DATA PLOT\")", "    ", "    labpath <- tklabel(gui, text = \"Path name:\")", 
"    pathname <- tclVar(flopath)", "    entpath <- tkentry(gui, textvariable = as.character(pathname), justify = \"center\", ", 
"        background = \"grey\",state=\"disabled\")", "    tkpack(labpath, anchor = \"w\")", 
"    tkpack(entpath, fill = \"both\", padx = 2)", " ", "    tkpack(mb<-tkmenubutton(gui,text=\"Data Type\",underline=\"0\",relief=\"raised\"),", 
"      fill=\"both\",pady=4,padx=2)", "    mb.menu<-tkmenu(mb)", 
"    tkconfigure(mb,menu=paste(tcl(\"winfo\",\"parent\",mb.menu),\".\",tcl(\"winfo\",\"name\",mb.menu),sep=\"\"))", 
"    if(any(vars==\"stg\"))", "      tkadd(mb.menu,\"command\",label=\"stage\",command=cmd1)", 
"    if(any(vars==\"q\"))", "      tkadd(mb.menu,\"command\",label=\"discharge\",command=cmd1q)", 
"    if(any(vars==\"turb\"))", "      tkadd(mb.menu,\"command\",label=\"turbidity\",command=cmd2)", 
"    if(any(vars==\"stg\")&any(vars==\"turb\"))", "      tkadd(mb.menu,\"command\",label=\"stage-turbidity\",command=cmd3)", 
"    if(any(vars==\"q\")&any(vars==\"turb\"))", "      tkadd(mb.menu,\"command\",label=\"discharge-turbidity\",command=cmd3q)", 
"    if(any(vars==\"rain\"))", "      tkadd(mb.menu,\"command\",label=\"rainfall\",command=cmd4)", 
"    if(any(vars==\"q\")&any(vars==\"rain\"))", "      tkadd(mb.menu,\"command\",label=\"discharge-rainfall\",command=cmd4q)", 
"    if(any(vars==\"wtemp\"))", "      tkadd(mb.menu,\"command\",label=\"water temperature\",command=cmd5)", 
"    if(any(vars==\"q\")&any(vars==\"wtemp\"))", "      tkadd(mb.menu,\"command\",label=\"discharge-water temperature\",command=cmd5q)", 
"    if(any(vars==\"atemp\"))", "      tkadd(mb.menu,\"command\",label=\"air temperature\",command=cmd6)", 
"    if(any(vars==\"wtemp\")&any(vars==\"atemp\"))", "      tkadd(mb.menu,\"command\",label=\"water-air temperature\",command=cmd7)", 
"    if(any(vars==\"lc1\")|any(vars==\"lc2\")|any(vars==\"lc3\")|any(vars==\"lc4\")|any(vars==\"lc5\")|any(vars==\"lc6\")|any(vars==\"lc7\"))", 
"      tkadd(mb.menu,\"command\",label=\"throughfall\",command=cmd8)", 
"    if(any(vars==\"wind\"))", "      tkadd(mb.menu,\"command\",label=\"wind speed\",command=cmd9)", 
"    choice <- tclVar(\"none\")", "    entchoice<-tkentry(gui,textvariable=as.character(choice),justify=\"center\",background=\"grey\",state=\"disabled\")", 
"    tkpack(entchoice,fill=\"both\",padx=2)", "", "    big.frm <- tkframe(gui)", 
"    datl.frm <- tkframe(big.frm)", "    datr.frm <- tkframe(big.frm)", 
"    labsd <- tklabel(datl.frm, text = \"Start Date\")", "    sd <- tclVar(\"m/d/y\")", 
"    entsd <- tkentry(datl.frm, textvariable = as.character(sd), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labsd, entsd)", 
"", "    labst <- tklabel(datr.frm, text = \"Start Time\")", 
"    st <- tclVar(0)", "    entst <- tkentry(datr.frm, textvariable = as.character(st), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labst, entst)", 
"", "    labed <- tklabel(datl.frm, text = \"End Date\")", "    ed <- tclVar(\"m/d/y\")", 
"    ented <- tkentry(datl.frm, textvariable = as.character(ed), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labed, ented)", 
"", "    labet <- tklabel(datr.frm, text = \"End Time\")", "    et <- tclVar(2400) ", 
"    entet <- tkentry(datr.frm, textvariable = as.character(et), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labet, entet)", 
"    ", "    tkpack(datl.frm, datr.frm, side = \"left\")", "    tkpack(big.frm, pady = 2,padx=2)", 
"    ", "    pleft <- tclVar(0)", "    tkpack(tkcheckbutton(gui, text = \"Left Axis\", variable = as.character(pleft),state=\"disabled\"), ", 
"        anchor = \"w\", padx = 1)", "    ", "    min1.frm <- tkframe(gui)", 
"    labmin1 <- tklabel(min1.frm, text = \"axis min\", anchor = \"w\")", 
"    min1 <- tclVar(\"NA\")", "    entmin1 <- tkentry(min1.frm, textvariable = as.character(min1), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmin1, labmin1, side = \"left\")", 
"    tkpack(min1.frm, anchor = \"w\", padx = 2)", " ", "    ", 
"    max1.frm <- tkframe(gui)", "    labmax1 <- tklabel(max1.frm, text = \"axis max\", anchor = \"w\")", 
"    max1 <- tclVar(\"NA\")", "    entmax1 <- tkentry(max1.frm, textvariable = as.character(max1), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmax1, labmax1, side = \"left\")", 
"    tkpack(max1.frm, anchor = \"w\", padx = 2)", "     ", "    pright <- tclVar(0)", 
"    tkpack(tkcheckbutton(gui, text = \"Right Axis\", variable = as.character(pright),state=\"disabled\"), ", 
"        anchor = \"w\", padx = 1)", "    ", "    min2.frm <- tkframe(gui)", 
"    labmin2 <- tklabel(min2.frm, text = \"axis min\")", "    min2 <- tclVar(\"NA\")", 
"    entmin2 <- tkentry(min2.frm, textvariable = as.character(min2), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmin2, labmin2, side = \"left\")", 
"    tkpack(min2.frm, anchor = \"w\", padx = 2)", "    ", "    max2.frm <- tkframe(gui)", 
"    labmax2 <- tklabel(max2.frm, text = \"axis max\")", "    max2 <- tclVar(\"NA\")", 
"    entmax2 <- tkentry(max2.frm, textvariable = as.character(max2), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmax2, labmax2, side = \"left\")", 
"    tkpack(max2.frm, anchor = \"w\", padx = 2)", "    ", "    tclvar <- list(choice=choice,sd=sd,ed=ed,st=st,et=et,min1=min1,max1=max1,min2=min2,max2=max2,pright=pright,pleft=pleft)", 
"", "    done <- tclVar(0)", "    but.frm <- tkframe(gui)", "    plot.but <- tkbutton(but.frm, text = \"PLOT\", command = plot)", 
"#    print.but<-tkbutton(but.frm,text=\"PRINT\",command=print)", 
"    quit.but <- tkbutton(but.frm, text = \"QUIT\", command = function() tclvalue(done) <- 1)", 
"    tkpack(plot.but,quit.but, fill = \"both\")", "    tkpack(but.frm, fill = \"both\", pady = 4, padx = 2)", 
"", "    tkraise(gui)", "    tkbind(gui, \"<Destroy>\", function() tclvalue(done) <- 2)", 
"    tkwait.variable(as.character(done))", "    if (tclvalue(done) == \"2\") { ", 
"        print(\"GUI aborted\")", "        return(invisible())", 
"    }", "    if(!is.null(dev.list())){", "      dev.off()", 
"    }", "    tkdestroy(gui)", "}", "}"))
flowcomp <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = NULL, long = T, 
    adj = T, var = T, units = "cfs", comp = T, opt = c(0, 0), 
    oldcomp = F) 
{
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
    if (!(is.null(exclude))) 
        sam <- sam[!(sam$stgcode %in% exclude), ]
    print(sam)
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$q <= 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(pop$q <= 0)) 
        stop("\nZero or negative discharges found in flo data.")
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    xsample <- log(sam$q)
    ysample <- log(sam$ssc)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% ysample
    poppredssc <- b[1] + b[2] * log(pop$q)
    sampredssc <- b[1] + b[2] * xsample
    resid <- ysample - sampredssc
    r2 <- cor(ysample, sampredssc)^2
    s <- sqrt(sum(resid^2)/(n - 2))
    if (comp) 
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
    else addback <- 0
    newssc <- exp(poppredssc + addback)
    good <- newssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    estflux <- sum(k * popq[good] * newssc[good])
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA, 
            coef = b, type = "logxy", bias = "comp", meth = 2, 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA, 
        coef = b, type = "logxy", bias = "comp", meth = 2)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"        stime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"        exclude = NULL, long = T, adj = T, var = T, units = \"cfs\", ", 
"        comp=T, opt=c(0,0), oldcomp=F)", "{", "        #  Revised 2015mar29   Changed how opt works with composite method, unless oldcomp=T.", 
"        #     See comments in turbrc() for detailed explanation ", 
"        #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"        #  Coded 2016jan25 using turbcomp", "        #  Estimates sediment load using composite method (Aulenbach 2006)", 
"        #  based on log-log model of SSC vs. DISCHARGE ", "        #  sam contains sample discharge and ssc", 
"        #  pop contains population discharge and stages at intervals", 
"        #     specified by \"interval\" argument", "        #  sam contains minutes since midnight, pop needs only military time", 
"        #  Extracts records from sample and population files", 
"        #  between specified starting and ending dates and times.", 
"        #  If dumps and bottles are specified, the regression uses only those.", 
"        #  Excludes samples with stgcode matching values in exclude vector.", 
"        #  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"        #  When comp=T, bias will be returned as \"comp\" (as a kludge for ttsplot)", 
"        #  Output component \"predssc\" may contain negative values, but", 
"        #  only positive values of predssc are included in load estimate", 
"        #  Assumes units are cfs unless.  Alternative is units=\"cumecs\". (m3/sec)", 
"        hy <- zfill(hy, 2)", "        k <- 0.06 * interval", 
"        pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"        sam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"        if(missing(dumps) && missing(bottles)) {", "                # Use bottles between specified start and end dates", 
"                sam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"        }", "        else if(missing(bottles)) {", "                # Use all bottles in specified dumps", 
"                sam <- sam[allmatches(dumps, sam$dump),  ]", 
"        }", "        else {", "                # Use only specified bottles from specified dumps", 
"                sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"                        sam$bottle)),  ]", "        }", "        if (!(is.null(exclude)))", 
"                sam <- sam[!(sam$stgcode %in% exclude),  ]", 
"        print(sam)", "        if(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"                if(all(is.na(sam$ssc) | is.na(sam$q)))", "                        stop(\"No samples match specified criteria\")", 
"                sam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", 
"                print(\"missing value(s) removed from sample data\")", 
"        }", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$q<=0)) stop('Zero or negative value in sample discharge.  Cannot take logarithm.')", 
"        if (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "        pop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"        if(any(is.na(pop$q)))", "                stop(\"\\nSorry, the specified period has missing discharge values\")", 
"        if(any(pop$q <= 0))", "                stop(\"\\nZero or negative discharges found in flo data.\")", 
"        #convert flow to m^3/sec", "        if(units == \"cfs\") ", 
"                qfactor <- 35.31467 ", "        else if(units == \"cumecs\")", 
"                qfactor <- 1", "        else stop(\"flow units must be cfs or cumecs\")", 
"        popq <- pop$q/qfactor", "        n <- dim(sam)[1]", 
"        xsample <- log(sam$q)", "        ysample <- log(sam$ssc)", 
"        x1 <- cbind(rep(1, n), xsample)", "        xx <- t(x1) %*% x1", 
"        invxx <- solve(xx)", "        tmp <- invxx %*% t(x1)", 
"        b <- tmp %*% ysample", "        poppredssc <- b[1] + b[2] * log(pop$q)", 
"        sampredssc <- b[1] + b[2] * xsample", "        resid <- ysample - sampredssc", 
"        r2 <- cor(ysample, sampredssc)^2", "        s <- sqrt(sum(resid^2)/(n - 2))", 
"        # The next \"if\" is just for compatibility with other turb... functions", 
"        # This function will normally never be called with comp=F", 
"        if (comp)", "           addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"        else addback <- 0  ", "        newssc <- exp(poppredssc + addback)", 
"        good <- newssc > 0", "        if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"        estflux <- sum(k * popq[good] * newssc[good])", "        if(long)", 
"             list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA,", 
"                coef = b, type = \"logxy\", bias = \"comp\", meth = 2,", 
"                chr = pop$chr, turb = pop$turb, predssc = newssc)", 
"        else list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA,", 
"                 coef = b, type = \"logxy\", bias = \"comp\", meth = 2)", 
"}"))
flowduan <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    sam <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
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
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$q <= 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take logarithm.")
    if (any(pop$q <= 0)) 
        stop("Zero or negative value in flow data.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    qpop <- pop$q/qfactor
    xsam <- sam$q/qfactor
    result <- logxy.duan(xsam, sam$ssc, qpop, qpop, interval, 
        var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "duan", meth = 2, 
            chr = pop$chr, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "duan")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude=TRUE, long = T, adj = T, var = T, units = \"cfs\")", 
"{", "      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  5/18/07 now calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates the load from .flo file between specified dates/times", 
"\t#  using an SRC from .sed file between specified dates/times", 
"\t#  Fit is linear in the logs of discharge and ssc", "\t#  Bias-correction and MSE calcs use Duan's smearing correction", 
"\t#  Adjusts concentrations using DIS regressions unless adj=F.", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", "\tif(any(is.na(pop$q)))", 
"\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"\t}", "\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$", 
"\t\t\tdump, sam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$q)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$q<=0)) stop('Zero or negative value in sample discharge.  Cannot take logarithm.')", 
"      if (any(pop$q<=0)) stop('Zero or negative value in flow data.  Cannot take logarithm.')", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\t# cubic meters per second", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tqpop <- pop$q/qfactor", "\txsam <- sam$q/qfactor", "\tresult <- logxy.duan(xsam, sam$ssc, qpop, qpop, interval, var = var)", 
"\tn <- dim(sam)[1]", "\tnewssc <- result$predssc", "\tr2 <- result$rsquare", 
"\ts <- result$s", "\tcoef <- result$betahat", "\testflux <- result$est.load", 
"\tcv <- (100 * result$est.rmse)/estflux", "\tif(long)", "\t\tlist(yhat = estflux, n = n, r2 = r2, s = s, cv = ", 
"\t\t\tcv, coef = coef, type = \"logxy\", bias = \"duan\", meth = 2,", 
"\t\t\tchr = pop$chr, predssc = newssc)", "\telse list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"\t\t\tcoef = coef, type = \"logxy\", bias = \"duan\")", "}"))
flowloess <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs", span = 1, degree = 1, comp = F, 
    opt = c(0, 0), oldcomp = F) 
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
    print(sam)
    if (exclude & !is.null(sam$exclude)) 
        sam <- sam[!sam$exclude, ]
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop$q <- pop$q/qfactor
    sam$q <- sam$q/qfactor
    n <- dim(sam)[1]
    fit1 <- loess(ssc ~ q, data = sam, span = span, degree = degree, 
        family = "gaussian", model = T)
    pred1 <- predict(fit1, newdata = pop, se = T)
    ypred <- pred1$fit
    sepred <- pred1$se
    xypoints <- loess.smooth(sam$q, sam$ssc, span = span, degree = degree, 
        family = "gaussian")
    xy <- data.frame(xypoints)
    if (any(is.na(ypred))) {
        np <- length(xy$x)
        lofit <- lm(y ~ x, data = xy[1:4, ])
        hifit <- lm(y ~ x, data = xy[(np - 3):np, ])
        lows <- (pop$q < xy$x[1])
        highs <- (pop$q > xy$x[np])
        ypred[lows] <- predict(lofit, newdata = data.frame(x = pop$q[lows]))
        se.grid <- predict(fit1, newdata = data.frame(q = xy$x), 
            se = T)$se
        ypred[highs] <- predict(hifit, newdata = data.frame(x = pop$q[highs]))
        sepred[lows] <- se.grid[1]
        sepred[highs] <- se.grid[np]
        if (any(is.na(ypred))) 
            stop("Extrapolation failed to eliminate missing values")
        ymin <- predict(lofit, newdata = data.frame(x = 0))
        ymax <- predict(hifit, newdata = data.frame(x = 2 * max(pop$q)))
        xypoints$x <- c(0, xypoints$x, 2 * max(pop$q))
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
    var <- F
    if (var) {
        sig <- k * pop$q * sepred
        cor <- cor.matrix(interval, pop$q, sam$q, pop$q)
        cov <- sig * t(cor * sig)
        estvar <- sum(cov)
        cv <- (100 * sqrt(estvar))/yhat
    }
    else cv <- NA
    if (long) 
        list(yhat = yhat, n = n, r2 = r2, s = s, cv = NA, coef = coef, 
            type = "loess", xy = xypoints, meth = 2, bias = bias, 
            chr = pop$chr, predssc = ypred)
    else list(yhat = yhat, n = n, r2 = r^2, s = s, cv = NA, coef = coef, 
        type = "loess", xy = xypoints, meth = 2, bias = bias)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\", ", 
"      span = 1, degree = 1, comp = F, opt = c(0,0), oldcomp = F)", 
"{", "      #  Revised 2015mar29   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbrc() for detailed explanation ", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Modified 2016jan25. Implemented composite method.  ALSO changed so that ", 
"      #     negative predictions are ALWAYS set to zero in flux calculation", 
"      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #     New: sets negative predictions to zero for estimating the load ", 
"      #        (predssc output component still contains the negatives)", 
"      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Modified June 13, 2006; previously failed unless extrapolation required", 
"\t#  Predicts concentrations using a loess fit of ssc to discharge", 
"\t#    Extends the range linearly if necessary for prediction", 
"\t#  Calculates a guess for cv based on the correlation matrix", 
"\t#    from the linear model's predictions.  The standard errors", 
"\t#    from loess seem too high, so I'm returning NA for now.", 
"\t#  sam contains sample discharges (flow) and ssc", "\t#  pop contains population flow at intervals", 
"\t#     specified by \"interval\" argument", "\t#  Extracts records from sample and population files", 
"\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Excludes samples whose stgcode matches a value in exclude vector", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"      bias <- NULL", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", 
"\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tprint(sam)", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tif(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$q)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpop$q <- pop$q/qfactor", "\tsam$q <- sam$q/qfactor", "\tn <- dim(sam)[1]", 
"\tfit1 <- loess(ssc ~ q, data = sam, span = span, degree = degree, family", 
"\t\t = \"gaussian\", model = T)", "\tpred1 <- predict(fit1, newdata = pop, se = T)", 
"\typred <- pred1$fit", "\tsepred <- pred1$se", "\txypoints <- loess.smooth(sam$q, sam$ssc, span = span, degree = ", 
"\t\tdegree, family = \"gaussian\")", "\txy <- data.frame(xypoints)", 
"\tif(any(is.na(ypred))) {", "\t\tnp <- length(xy$x)", "\t\tlofit <- lm(y ~ x, data = xy[1:4,  ])", 
"\t\thifit <- lm(y ~ x, data = xy[(np - 3):np,  ])", "\t\tlows <- (pop$q < xy$x[1])", 
"\t\thighs <- (pop$q > xy$x[np])", "\t\typred[lows] <- predict(lofit, newdata = data.frame(x = pop$", 
"\t\t\tq[lows]))", "\t\tse.grid <- predict(fit1, newdata = data.frame(q = xy$x), se", 
"\t\t\t = T)$se", "\t\typred[highs] <- predict(hifit, newdata = data.frame(x = pop$", 
"\t\t\tq[highs]))", "\t\tsepred[lows] <- se.grid[1]", "\t\tsepred[highs] <- se.grid[np]", 
"\t\tif(any(is.na(ypred)))", "\t\t\tstop(\"Extrapolation failed to eliminate missing values\"", 
"\t\t\t\t)", "\t\t# Extrapolate curve to q=0 and twice the max flow", 
"\t\tymin <- predict(lofit, newdata = data.frame(x = 0))", "\t\tymax <- predict(hifit, newdata = data.frame(x = 2 * max(pop$", 
"\t\t\tq)))", "\t\txypoints$x <- c(0, xypoints$x, 2 * max(pop$q))", 
"\t\txypoints$y <- c(ymin, xypoints$y, ymax)", "\t}", "      if (comp) {", 
"         bias <- \"comp\"", "         resid <- sam$ssc - fitted(fit1)  # residuals", 
"         addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"         ypred <- ypred + addback", "      }", "      good <- ypred > 0", 
"      if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"      yhat <- sum(k * pop$q[good] * ypred[good])", "\tr2 <- cor(fit1$fitted, fit1$fitted + fit1$resid)^2", 
"\ts <- fit1$s", "\tcoef <- c(NA, NA)", "\tvar <- F", "\tif(var) {", 
"\t\tsig <- k * pop$q * sepred", "\t\tcor <- cor.matrix(interval, pop$q, sam$q, pop$q)", 
"\t\tcov <- sig * t(cor * sig)", "\t\testvar <- sum(cov)", "\t\tcv <- (100 * sqrt(estvar))/yhat", 
"\t}", "\telse cv <- NA", "      if(long)", "            list(yhat = yhat, n = n, r2 = r2, s = s, cv = NA, coef = coef,", 
"                 type = \"loess\", xy = xypoints, meth = 2, bias = bias, ", 
"                 chr = pop$chr, predssc = ypred)", "      else list(yhat = yhat, n = n, r2 = r^2, s = s, cv = NA, coef = coef,", 
"                 type = \"loess\", xy = xypoints, meth = 2, bias = bias)", 
"}"))
flowmvue <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    if (missing(dumps) && missing(bottles)) {
        sam <- subtime(samples, sdate2, stime2, edate2, etime2)
    }
    else if (missing(bottles)) {
        sam <- samples[allmatches(dumps, samples$dump), ]
    }
    else {
        sam <- samples[allmatches(paste(dumps, bottles), paste(samples$dump, 
            samples$bottle)), ]
    }
    if (exclude & !is.null(sam$exclude)) 
        sam <- sam[!sam$exclude, ]
    print(sam)
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$q <= 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take logarithm.")
    if (any(pop$q <= 0)) 
        stop("Zero or negative value in flow data.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    qpop <- pop$q/qfactor
    xsam <- sam$q/qfactor
    result <- logxy.mvue(xsam, sam$ssc, qpop, qpop, interval, 
        var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "mvue", meth = 2, 
            chr = pop$chr, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "mvue")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\")", 
"{", "      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; now calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates the load from .flo file between specified dates/times", 
"\t#  using an SRC from .sed file between specified dates/times", 
"\t#  Fit is linear in the logarithms of discharge and ssc.", 
"\t#  Adjusts concentrations using DIS regressions unless adj=F.", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", "\tif(any(is.na(pop$q)))", 
"\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tsamples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\tsam <- subtime(samples, sdate2, stime2, edate2, etime2)", 
"\t}", "\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- samples[allmatches(dumps, samples$dump),  ]", "\t}", 
"\telse {", "\t\t# Use only specified bottles from specified dumps", 
"\t\tsam <- samples[allmatches(paste(dumps, bottles), paste(samples$", 
"\t\t\tdump, samples$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$q)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$q<=0)) stop('Zero or negative value in sample discharge.  Cannot take logarithm.')", 
"      if (any(pop$q<=0)) stop('Zero or negative value in flow data.  Cannot take logarithm.')", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "      # cubic meters per second", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"        qpop <- pop$q/qfactor", "        xsam <- sam$q/qfactor", 
"        result <- logxy.mvue(xsam, sam$ssc, qpop, qpop, interval, var = var)", 
"        n <- dim(sam)[1]", "        newssc <- result$predssc", 
"        r2 <- result$rsquare", "        s <- result$s", "        coef <- result$betahat", 
"        estflux <- result$est.load", "        cv <- (100 * result$est.rmse)/estflux", 
"        if(long)", "                list(yhat = estflux, n = n, r2 = r2, s = s, cv = ", 
"                        cv, coef = coef, type = \"logxy\", bias = \"mvue\", meth = 2,", 
"                        chr = pop$chr, predssc = newssc)", "        else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"                        coef = coef, type = \"logxy\", bias = \"mvue\")", 
"}"))
flowpairs <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    interval = 10, opt = 1, long = T, adj = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    samples <- subtime(samples, sdate1, stime1, edate1, etime1)
    mergesam <- samples[, c("chr", "ssc")]
    mergepop <- merge(pop, mergesam, all.x = T)
    mergepop <- mergepop[order(mergepop$chr), ]
    conf <- intersect(names(mergepop), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in flowpairs:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    attach(mergepop)
    on.exit(detach(mergepop))
    if (adj) 
        ssc <- dis.adjust(stn, ssc)
    N <- dim(mergepop)[1]
    index <- 1:N
    sam <- index[-which.na(ssc)]
    diffs <- diff(c(0, sam, N))
    n <- length(sam)
    low1 <- rep(c(NA, sam), diffs)
    low2 <- rep(c(NA, NA, sam[-n]), diffs)
    high1 <- rep(c(sam, NA), diffs)
    high2 <- rep(c(sam[-1], NA, NA), diffs)
    x <- matrix(q[c(low2, low1, high1, high2)], ncol = 4)
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
        xy <- approx(x1[!duplicated(x1)], y1[!duplicated(x1)], 
            xout = q[i])
        newssc[i] <- xy$y
    }
    unfit <- is.na(newssc)
    if (sum(unfit) != 0) 
        newssc[unfit] <- predict.loglog(q[sam], ssc[sam], q[unfit])
    coef <- coef(lm(log(ssc[sam]) ~ log(q[sam])))
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    estflux <- sum((k * newssc * q)/qfactor)
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            coef = coef, type = "pairs", meth = 2, chr = mergepop$chr, 
            predssc = newssc)
    else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
        coef = coef, type = "pairs")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, interval = 10, opt = 1, long", 
"\t = T, adj = T, units = \"cfs\")", "{", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Extracts records from pop between given starting date/time", 
"\t#  and ending date/time and merges with samples from the", 
"\t#  correct station for the same time period.  Then estimates", 
"\t#  the load using local pairwise functions of discharge.", 
"\t#  Uses approx() with the 2 points on each side (4 total).", 
"\t#  approx() will not extrapolate beyond those 4 discharges, so ", 
"\t#  a regression is applied to all data when approx returns NA.", 
"\t#  Extrapolates to (0,0) at lower end unless opt = 2, in which", 
"\t#  case a simple regression in the logs of q and ssc is applied.", 
"\t#  The reason 4 points are used instead of two is to reduce the chance", 
"\t#  that predictions will be required outside the range of x.", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\t#  Result must be passed to ttsplot() using an \"src\" argument", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsamples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", "\tif(any(is.na(pop$q)))", 
"\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tsamples <- subtime(samples, sdate1, stime1, edate1, etime1)", 
"\tmergesam <- samples[, c(\"chr\", \"ssc\")]", "\tmergepop <- merge(pop, mergesam, all.x = T)", 
"\tmergepop <- mergepop[order(mergepop$chr),  ]", "\tconf <- intersect(names(mergepop),objects(1))", 
"\tif (length(conf) > 0) {", "\t\tcat(\"The following objects conflict with object names in flowpairs:\\n\")", 
"\t\tcat(conf,\"\\n\")", "\t\tcat(\"Please remove conflicting objects before proceeding.\\n\")", 
"\t\treturn(invisible())", "\t}", "\tattach(mergepop)", "\ton.exit(detach(mergepop))", 
"\tif(adj) ssc <- dis.adjust(stn,ssc) ", "\tN <- dim(mergepop)[1]", 
"\tindex <- 1:N", "\tsam <- index[ - which.na(ssc)]", "\tdiffs <- diff(c(0, sam, N))", 
"\tn <- length(sam)", "\tlow1 <- rep(c(NA, sam), diffs)", "\tlow2 <- rep(c(NA, NA, sam[ - n]), diffs)", 
"\thigh1 <- rep(c(sam, NA), diffs)", "\thigh2 <- rep(c(sam[-1], NA, NA), diffs)", 
"\t# cubic meters per sec", "\tx <- matrix(q[c(low2, low1, high1, high2)], ncol = 4)", 
"\ty <- matrix(ssc[c(low2, low1, high1, high2)], ncol = 4)", 
"\tif(opt == 1) {", "\t\t#\t\tPermit extrapolation down to (0,0)", 
"\t\tx[is.na(x)] <- 0", "\t\ty[is.na(y)] <- 0", "\t}", "\tnewssc <- numeric(N)", 
"\tfor(i in index) {", "\t\t#\t\tEstimate ssc with local linear function of discharge", 
"\t\tx1 <- x[i,  ]", "\t\ty1 <- y[i,  ]", "\t\tx1 <- x1[!is.na(x1)]", 
"\t\ty1 <- y1[!is.na(y1)]", "\t\txy <- approx(x1[!duplicated(x1)], y1[!duplicated(x1)], xout = q[i])", 
"\t\tnewssc[i] <- xy$y", "\t}", "\tunfit <- is.na(newssc)", "\tif(sum(unfit) != 0)", 
"\t\tnewssc[unfit] <- predict.loglog(q[sam], ssc[sam], q[unfit])", 
"\tcoef <- coef(lm(log(ssc[sam]) ~ log(q[sam])))", "\tif(units == \"cfs\")", 
"\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\testflux <- sum((k * newssc * q)/qfactor)", "\tif(long)", "\t\tlist(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, coef = ", 
"\t\t\tcoef, type = \"pairs\", meth = 2, chr = mergepop$chr,", 
"\t\t\tpredssc = newssc)", "\telse list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, coef = coef,", 
"\t\t\ttype = \"pairs\")", "}"))
flowpower <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs", comp = F, opt = c(0, 0), 
    oldcomp = F) 
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
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop$q <- pop$q/qfactor
    sam$q <- sam$q/qfactor
    n <- dim(sam)[1]
    startdata <- sam[sam$q != 0 & sam$ssc != 0, ]
    startmodel <- lm(log(ssc) ~ log(q), data = startdata)
    a0 <- exp(coef(startmodel)[1])
    b0 <- coef(startmodel)[2]
    fit1 <- nls(ssc ~ a * q^b, data = sam, start = list(a = a0, 
        b = b0))
    pred1 <- predict(fit1, newdata = pop)
    yhat <- sum(k * pop$q * pred1)
    r <- cor(sam$ssc, fitted(fit1))
    s <- summary(fit1)$sigma
    coef <- coef(fit1)
    names(coef) <- c("constant", "power")
    if (comp) {
        bias <- "comp"
        var <- F
        resid <- sam$ssc - fitted(fit1)
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
        col1 <- pop$q^b1
        col2 <- b0 * log(pop$q) * col1
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
            type = "power", meth = 2, bias = bias, chr = pop$chr, 
            predssc = pred1)
    else list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef, 
        type = "power", meth = 2, bias = bias)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\", ", 
"      comp = F, opt = c(0,0), oldcomp = F)", "{", "      #  Revised 2015mar29   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbrc() for detailed explanation ", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2016jan25; added option for composite method.", 
"      #     Negative predictions of SSC are excluded from flux calculation", 
"      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Predicts concentrations using a power function of discharge", 
"\t#    estimated by non-linear least squares", "\t#  Approximates CV using the delta method", 
"\t#  sam contains sample discharge and ssc", "\t#  pop contains population discharges at intervals", 
"\t#     specified by \"interval\" argument", "\t#  sam contains minutes since midnight, pop needs only military time", 
"\t#  Extracts records from sample and population files", "\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Excludes samples whose stgcode matches a value in exclude vector", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"      bias <- NULL", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", 
"\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$q)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpop$q <- pop$q/qfactor", "\tsam$q <- sam$q/qfactor", "\tn <- dim(sam)[1]", 
"\tstartdata <- sam[sam$q != 0 & sam$ssc != 0,  ]", "\tstartmodel <- lm(log(ssc) ~ log(q), data = startdata)", 
"\ta0 <- exp(coef(startmodel)[1])", "\tb0 <- coef(startmodel)[2]", 
"\tfit1 <- nls(ssc ~ a * q^b, data = sam, start = list(a = a0, b = b0))", 
"\tpred1 <- predict(fit1, newdata = pop)", "\tyhat <- sum(k * pop$q * pred1)", 
"\tr <- cor(sam$ssc, fitted(fit1))", "\ts <- summary(fit1)$sigma", 
"\tcoef <- coef(fit1)", "\tnames(coef) <- c(\"constant\", \"power\")", 
"      if (comp) {", "         bias <- \"comp\"", "         # Turn off variance computation, interpolate residuals and add to predictions", 
"         var <- F", "         resid <- sam$ssc - fitted(fit1)  # residuals", 
"         addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"         pred1 <- pred1 + addback", "      }", "      good <- pred1 > 0", 
"      if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"      yhat <- sum(k * pop$q[good] * pred1[good])", "      if (var) {", 
"         # Use delta method to approximate covariance matrix", 
"         vcov <- vcov.nls(fit1)", "         b0 <- coef[\"constant\"]", 
"         b1 <- coef[\"power\"]", "         col1 <- pop$q^b1", 
"         col2 <- b0 * log(pop$q) * col1", "         x2 <- k * pop$q * cbind(col1, col2)", 
"         V <- try(x2 %*% vcov %*% t(x2))", "         if (inherits(V, \"try-error\")) {", 
"              print(\"Cannot calculate covariance matrix.  Time series too long\")", 
"              cv <- NA", "         }", "         else", "              cv <- (100 * sqrt(sum(V)))/yhat", 
"        }", "      else cv <- NA", "      if(long)", "          list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef,", 
"              type = \"power\", meth = 2, bias = bias, chr = pop$chr, ", 
"              predssc = pred1)", "      else list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef,", 
"              type = \"power\", meth = 2, bias = bias)", "}"
))
flowqmle <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    sam <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
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
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$q <= 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take logarithm.")
    if (any(pop$q <= 0)) 
        stop("Zero or negative value in flow data.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    qpop <- pop$q/qfactor
    xsam <- sam$q/qfactor
    result <- logxy.qmle(xsam, sam$ssc, qpop, qpop, interval, 
        var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "qmle", meth = 2, 
            chr = pop$chr, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "qmle")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude=TRUE, long = T, adj = T, var = T, units = \"cfs\")", 
"{", "      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates the load from .flo file between specified dates/times", 
"\t#  using an SRC from .sed file between specified dates/times", 
"\t#  Fit is linear in the logs of discharge and ssc", "\t#  Adjusts concentrations using DIS regressions unless adj=F.", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", "\tif(any(is.na(pop$q)))", 
"\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"\t}", "\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$", 
"\t\t\tdump, sam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$q)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$q<=0)) stop('Zero or negative value in sample discharge.  Cannot take logarithm.')", 
"      if (any(pop$q<=0)) stop('Zero or negative value in flow data.  Cannot take logarithm.')", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\t# cubic meters per second", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tqpop <- pop$q/qfactor", "\txsam <- sam$q/qfactor", "\tresult <- logxy.qmle(xsam, sam$ssc, qpop, qpop, interval, var = var)", 
"\tn <- dim(sam)[1]", "\tnewssc <- result$predssc", "\tr2 <- result$rsquare", 
"\ts <- result$s", "\tcoef <- result$betahat", "\testflux <- result$est.load", 
"\tcv <- (100 * result$est.rmse)/estflux", "\tif(long)", "\t\tlist(yhat = estflux, n = n, r2 = r2, s = s, cv = ", 
"\t\t\tcv, coef = coef, type = \"logxy\", bias = \"qmle\", meth = 2,", 
"\t\t\tchr = pop$chr, predssc = newssc)", "\telse list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"\t\t\tcoef = coef, type = \"logxy\", bias = \"qmle\")", "}"))
flowsqrt <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs", comp = F, opt = c(0, 0), 
    oldcomp = F) 
{
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
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(sam$ssc < 0)) 
        stop("Negative value in sample ssc.  Cannot take square root.")
    if (any(sam$q < 0)) 
        stop("Zero or negative value in sample discharge.  Cannot take square root.")
    if (any(pop$q < 0)) 
        stop("Zero or negative value in flow data.  Cannot take square root.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.3147
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop$q <- pop$q/qfactor
    sam$q <- sam$q/qfactor
    fit <- lm(sqrt(ssc) ~ sqrt(q), data = sam)
    betahat <- coef(fit)
    resid <- fit$residuals
    n <- length(resid)
    if (comp) {
        bias = "comp"
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        pred <- predict(fit, newdata = pop)
        newpred <- pred + addback
        if (any(newpred < 0)) {
            cat(sum(newpred < 0), "negative predictions were set to zero before squaring\n")
            newpred[newpred < 0] <- 0
        }
        predssc <- newpred^2
    }
    else {
        bias = "duan"
        duan <- duan.sqrt(fit, newdata = pop)
        predssc <- duan$corrected
    }
    summ <- summary(fit)
    rsquare <- summ$r.sq
    sigma <- summ$sigma
    flux <- k * pop$q * predssc
    yhat <- sum(flux)
    if (long) 
        list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, 
            coef = betahat, type = "sqrt", bias = bias, meth = 2, 
            chr = pop$chr, predssc = predssc)
    else list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, 
        coef = betahat, type = "sqrt", bias = bias, meth = 2)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\", ", 
"      comp = F, opt = c(0, 0), oldcomp = F)", "{", "      #  Revised 2015mar29   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbrc() for detailed explanation ", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2016jan25.  Added option for composite method", 
"      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu", "      hy <- zfill(hy, 2)", 
"\tk <- 0.06 * interval", "      pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"      sam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"      if(missing(dumps) && missing(bottles)) {", "           # Use bottles between specified start and end dates", 
"           sam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"      }", "      else if(missing(bottles)) {", "           # Use all bottles in specified dumps", 
"           sam <- sam[allmatches(dumps, sam$dump),  ]", "      }", 
"      else {", "           # Use only specified bottles from specified dumps", 
"           sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"                        sam$bottle)),  ]", "      }", "\tif (exclude & !is.null(sam$exclude))", 
"           sam <- sam[!sam$exclude,  ]", "      print(sam)", 
"      if(any(is.na(sam$ssc) | is.na(sam$q))) {", "           if(all(is.na(sam$ssc) | is.na(sam$q)))", 
"               stop(\"No samples match specified criteria\")", 
"           sam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", 
"           print(\"missing value(s) removed from sample data\")", 
"      }", "      pop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"      if(any(is.na(pop$q)))", "           stop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(sam$ssc < 0))", "\t\tstop(\"Negative value in sample ssc.  Cannot take square root.\")", 
"\tif(any(sam$q < 0))", "\t\tstop(\"Zero or negative value in sample discharge.  Cannot take square root.\")", 
"\tif(any(pop$q < 0))", "\t\tstop(\"Zero or negative value in flow data.  Cannot take square root.\")", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tif(units == \"cfs\")", 
"\t\tqfactor <- 35.3147", "\telse if(units == \"cumecs\")", "\t\tqfactor <- 1", 
"\telse stop(\"flow units must be cfs or cumecs\")", "\tpop$q <- pop$q/qfactor", 
"\tsam$q <- sam$q/qfactor", "\tfit <- lm(sqrt(ssc) ~ sqrt(q), data = sam)", 
"\tbetahat <- coef(fit)", "      resid <- fit$residuals", "      n <- length(resid)", 
"      if (comp) {", "          bias=\"comp\"", "          addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"          pred <- predict(fit, newdata = pop)", "          newpred <- pred+addback", 
"          # Set negative predictions to zero before squaring", 
"          if (any(newpred < 0)) {", "             cat(sum(newpred<0),\"negative predictions were set to zero before squaring\\n\")", 
"             newpred[newpred < 0] <- 0", "          }", "          predssc <- newpred^2", 
"      }", "      else {", "          bias=\"duan\"", "          duan <- duan.sqrt(fit, newdata = pop)", 
"          predssc <- duan$corrected", "      }", "\tsumm <- summary(fit)", 
"\trsquare <- summ$r.sq", "\tsigma <- summ$sigma", "\tflux <- k * pop$q * predssc", 
"\tyhat <- sum(flux)", "      if(long)", "           list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA,", 
"                coef = betahat, type = \"sqrt\", bias = bias, meth = 2,", 
"                chr = pop$chr, predssc = predssc)", "      else list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, coef = ", 
"                betahat, type = \"sqrt\", bias = bias, meth = 2)", 
"}"))
flowsrc <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, type = "logxy", bias = "mvue", units = "cfs", 
    span = 1, degree = 1, comp = F, opt = c(0, 0), oldcomp = F) 
{
    args <- as.list(match.call())
    for (vname in c("dumps", "bottles", "interval", "span", "degree", 
        "opt")) {
        if (vname %in% names(args) && is.character(args[[vname]])) {
            args[[vname]] <- eval(parse(text = args[[vname]]))
            assign(vname, args[[vname]])
        }
    }
    if (type != "linear" && type != "logx") {
        pass <- c("stn", "hy", "sdate1", "stime1", "edate1", 
            "etime1", "sdate2", "stime2", "edate2", "etime2", 
            "dumps", "bottles", "interval", "exclude", "long", 
            "adj", "var", "log", "units", "span", "degree", "comp", 
            "opt", "oldcomp")
        keep <- intersect(pass, names(args))
        if (type == "logxy") {
            if (comp) 
                estfunc <- "flowcomp"
            else estfunc <- paste("flow", bias, sep = "")
        }
        else estfunc <- paste("flow", type, sep = "")
        result <- do.call(estfunc, args[keep])
        return(result)
    }
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
    if (any(is.na(sam$ssc) | is.na(sam$q))) {
        if (all(is.na(sam$ssc) | is.na(sam$q))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$q), ]
        print("missing value(s) removed from sample data")
    }
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    popq <- pop$q/qfactor
    if (any(is.na(popq))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (type == "logx") {
        if (any(sam$q <= 0)) 
            stop("\nZero or negative value in sample discharge.  Cannot take logarithm.")
        x <- log(sam$q/qfactor)
        if (any(popq <= 0)) 
            stop("\nZero or negative discharges found in flo data.")
        else q <- log(popq)
    }
    else {
        x <- sam$q/qfactor
        q <- popq
    }
    n <- dim(sam)[1]
    bias <- NULL
    if (comp) {
        bias <- "comp"
        x1 <- cbind(rep(1, n), x)
        xx <- t(x1) %*% x1
        invxx <- solve(xx)
        tmp <- invxx %*% t(x1)
        coef <- tmp %*% sam$ssc
        poppredssc <- coef[1] + coef[2] * popq
        sampredssc <- coef[1] + coef[2] * x
        resid <- sam$ssc - sampredssc
        r2 <- cor(sam$ssc, sampredssc)^2
        s <- sqrt(sum(resid^2)/(n - 2))
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        newssc <- poppredssc + addback
        good <- newssc > 0
        if (sum(!good) > 0) 
            cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
        estflux <- sum(k * newssc[good] * q[good])
        cv <- NA
    }
    else {
        result <- usual.mle(x, sam$ssc, q, interval, var = var)
        newssc <- result$predssc
        r2 <- result$rsquare
        s <- result$s
        coef <- result$betahat
        estflux <- result$est.load
        cv <- (100 * result$est.rmse)/estflux
    }
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = type, meth = 2, bias = bias, 
            chr = pop$chr, predssc = newssc)
    else list(yhat = estflux, n = dim(sam)[1], r2 = r2, s = s, 
        cv = cv, coef = coef, type = type, meth = 2, bias = bias)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, type = \"logxy\", bias = \"mvue\",", 
"\tunits = \"cfs\", span = 1, degree = 1, comp = F, opt= c(0,0), oldcomp = F)", 
"{", "      #  Revised 2016apr8.  Changed defaults to units=\"cfs\" and interval=10 for CC", 
"\t#  Revised 2015mar29   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbrc() for detailed explanation ", 
"      #  Revised 2016mar24   Converts numeric arguments that were passed as strings", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2016jan25.  Composite method implemented for all types but \"pairs\"", 
"      #  Revised 2011feb10   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Revised 2007may18.  calls dis.adjust to limit extrapolated adjustments", 
"\t#  Revised 2006jul06.  pop$q replaced by popq after popq assigned.", 
"\t#          This bug caused load computations to be too high by qfactor", 
"\t#          when type=\"linear\" or type=\"logx\"", "\t#  Revised 2005jun09.  Added args exclude, type, and bias", 
"\t#  Revised 2004aug31.  Reports an error if zeros or negatives are found", 
"\t#      in discharge or SSC and a log-transformation was requested", 
"\t#  Estimates the load and its variance from a model of ssc vs flow.", 
"\t#  Obtains sample flow (discharge) and ssc from .sed object", 
"\t#  Obtains population flows from .flo object which contains", 
"\t#     population flows at intervals specified by \"interval\"", 
"\t#  Extracts population data between specified starting and ending ", 
"\t#  dates and times.  If two sets of starting and ending dates and", 
"\t#  times are specified, the population data are extracted using the", 
"\t#  first set and the sample data are extracted using the second set.", 
"\t#  If dumps and bottles are specified, dates are overridden.", 
"      #    dumpstr = expression for vector of dump numbers to include in plot", 
"      #    bottlestr = expression for vector of bottle numbers to include in plot", 
"      #    dumpstr can be included without bottlestr: all bottles for those dumps will be plotted", 
"      #    if both dumpstr and bottlestr are specified, the vectors must have same length", 
"\t#  Samples are excluded whose stgcode matches any of the values in ", 
"\t#     a numeric vector passed through the exclude argument", 
"\t#  Use long=F for abbreviated output.", "\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\t#  Skips variance calculation if var=F", "\t#  type can be \"linear\", \"logx\", \"logxy\", \"power\", \"loess\", or \"pairs\"", 
"\t#    When type==\"logxy\", meth argument selects one of 3 ", 
"\t#    bias-correction methods:  \"mvue\", \"duan\", or \"qmle\".  ", 
"\t#    \"mvue\" is the preferred method for small data sets.", 
"\t#    For large data sets (more than, a few days) \"mvue\" bogs down.  ", 
"\t#    \"duan\" (Duan's smearing correction) is preferred to \"qmle\"", 
"\t#    \"qmle\" assumes normal residuals and uses the \"naive\" exp(s2/2)", 
"\t#    correction.", "      #  When comp=T, bias will be returned as \"comp\" (as a kludge for ttsplot)", 
"      #  Output component \"predssc\" may contain negative values, but", 
"      #  only positive values of predssc are included in load estimate", 
"", "\t#  Assumes units are cumecs (m3/s) unless specified units=\"cfs\"", 
"      args <- as.list(match.call())", "      for (vname in c(\"dumps\",\"bottles\",\"interval\",\"span\",\"degree\",\"opt\")) {", 
"          if (vname %in% names(args) && is.character(args[[vname]])) {", 
"            args[[vname]] <- eval(parse(text=args[[vname]]))", 
"            assign(vname, args[[vname]])", "          }", "\t}", 
"      if(type != \"linear\" && type != \"logx\") {", "              #  Just run estimation function and quit", 
"              pass <- c(\"stn\", \"hy\", \"sdate1\", \"stime1\", \"edate1\", \"etime1\",", 
"                        \"sdate2\", \"stime2\", \"edate2\", \"etime2\", \"dumps\", \"bottles\",", 
"                        \"interval\", \"exclude\", \"long\", \"adj\", \"var\", \"log\",", 
"                        \"units\", \"span\", \"degree\", \"comp\", \"opt\", \"oldcomp\")", 
"              keep <- intersect(pass, names(args))", "              if(type == \"logxy\") {", 
"                      if (comp)", "                              estfunc <- \"flowcomp\"", 
"                      else", "                              estfunc <- paste(\"flow\", bias, sep = \"\")", 
"              }", "              else estfunc <- paste(\"flow\", type, sep = \"\")", 
"              result <- do.call(estfunc, args[keep])", "              return(result)", 
"      }", "      # remaining code is for type==\"linear\" or type==\"logx\"", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))   ", 
"\tif(missing(dumps) && missing(bottles)) {", "\t    # Use bottles between specified start and end dates", 
"\t    sam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"\t}", "\telse if(missing(bottles)) {", "\t    # Use all bottles in specified dumps", 
"\t    sam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t    # Use only specified bottles from specified dumps", "\t    sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump, sam$bottle)),  ]", 
"\t}", "\tif (exclude & !is.null(sam$exclude))", "\t\tsam <- sam[!sam$exclude,  ]", 
"\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$q))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$q)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$q),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\t# cubic meters per second", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", "\tpopq <- pop$q/qfactor", 
"\tif(any(is.na(popq)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(type == \"logx\") {", "\t\tif(any(sam$q <= 0))", "\t\t\tstop(\"\\nZero or negative value in sample discharge.  Cannot take logarithm.\"", 
"\t\t\t\t)", "\t\tx <- log(sam$q/qfactor)", "\t\tif(any(popq <= 0))", 
"\t\t\tstop(\"\\nZero or negative discharges found in flo data.\"", 
"\t\t\t\t)", "\t\telse q <- log(popq)", "\t}", "\telse {", "\t\tx <- sam$q/qfactor", 
"\t\tq <- popq", "\t}", "      n <- dim(sam)[1]", "      bias <- NULL", 
"      if (comp) {", "          bias <- \"comp\"", "          x1 <- cbind(rep(1, n), x)", 
"          xx <- t(x1) %*% x1", "          invxx <- solve(xx)", 
"          tmp <- invxx %*% t(x1)", "          coef <- tmp %*% sam$ssc", 
"          poppredssc <- coef[1] + coef[2] * popq", "          sampredssc <- coef[1] + coef[2] * x", 
"          resid <- sam$ssc - sampredssc", "          r2 <- cor(sam$ssc, sampredssc)^2", 
"          s <- sqrt(sum(resid^2)/(n - 2))", "          addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"          newssc <- poppredssc + addback", "          good <- newssc > 0", 
"          if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"          estflux <- sum(k*newssc[good]*q[good])", "          cv <- NA", 
"      }", "      else {", "          result <- usual.mle(x, sam$ssc, q, interval, var = var)", 
"          newssc <- result$predssc", "          r2 <- result$rsquare", 
"          s <- result$s", "          coef <- result$betahat", 
"          estflux <- result$est.load", "          cv <- (100 * result$est.rmse)/estflux", 
"      }", "      if(long)", "          list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"                  coef = coef, type = type, meth = 2, bias = bias, chr = pop$chr, ", 
"                  predssc = newssc)", "      else list(yhat = estflux, n = dim(sam)[1], r2 = r2, s = s, cv = cv,", 
"                  coef = coef, type = type, meth = 2, bias = bias)", 
"}"))
flumecalc <-
structure(function (stn, stg, hy = hy.default()) 
{
    a <- c(6, 10, 16, 10, 32.0542, 6, 12, 8, 16, 8, 27.16, 6, 
        6, 20, 6, 8, 12, 8, 3.95, 3.95, 8, 16, 8)
    b <- c(1.538, 1.559, 1.578, 1.559, 1.883, 1.538, 1.566, 1.55, 
        1.578, 1.55, 1.695, 1.538, 1.538, 1.587, 1.538, 1.55, 
        1.566, rep(1.55, 4), 1.578, 1.55)
    names(a) <- names(b) <- c("ban", "car", "dol", "eag", "fly", 
        "gib", "hen", "ive", "joh", "kje", "lan", "mun", "xra", 
        "xyz", "ogi", "por", "ric", "seq", "tre", "uql", "wil", 
        "yoc", "zie")
    if (hy >= 2000) {
        a["ive"] <- 3.95
        b["ive"] <- 1.55
    }
    if (hy >= 2005) {
        a["car"] <- 6
        b["car"] <- 1.538
        a["eag"] <- 6
        b["eag"] <- 1.538
        a["hen"] <- 8
        b["hen"] <- 1.55
    }
    a[stn] * stg^(b[stn])
}, source = c("function(stn, stg, hy = hy.default())", "{", "\t#  This function is designed to be called from qcalc", 
"\t#  If calling the function directly, include water years ", 
"\t#  for stations IVE, CAR, EAG, and HEN", "\ta <- c(6., 10., 16., 10., 32.0542, 6., 12., 8., 16., 8., 27.16, 6., 6, 20., 6, 8, 12, 8, 3.95, 3.95, 8, 16, 8)", 
"\tb <- c(1.538, 1.559, 1.578, 1.559, 1.883, 1.538, 1.566, 1.55, 1.578, 1.55, 1.695, 1.538, 1.538, 1.587, 1.538, 1.55, 1.566, rep(", 
"\t\t1.55, 4), 1.578, 1.55)", "\tnames(a) <- names(b) <- c(\"ban\", \"car\", \"dol\", \"eag\", \"fly\", \"gib\", \"hen\", \"ive\", \"joh\", \"kje\", \"lan\", \"mun\", \"xra\", \"xyz\", \"ogi\",", 
"\t\t\"por\", \"ric\", \"seq\", \"tre\", \"uql\", \"wil\", \"yoc\", \"zie\")", 
"\tif(hy >= 2000.) {", "\t\t# New IVE flume: downsized from 2ft to 1ft", 
"\t\ta[\"ive\"] <- 3.95", "\t\tb[\"ive\"] <- 1.55", "\t}", "\tif(hy >= 2005.) {", 
"\t\t# Note: new flume at DOL is same size as old one", "\t\t# \"car\" and \"eag\" were downsized from 2.5ft to 1.5ft", 
"\t\t# \"hen\" was downsized from 3ft to 2ft", "\t\ta[\"car\"] <- 6.", 
"\t\tb[\"car\"] <- 1.538", "\t\ta[\"eag\"] <- 6.", "\t\tb[\"eag\"] <- 1.538", 
"\t\ta[\"hen\"] <- 8.", "\t\tb[\"hen\"] <- 1.55", "\t}", "\ta[stn] * stg^(b[stn])", 
"}"))
format.times <-
structure(function (x, format. = "h:m:s", simplify = FALSE, ...) 
{
    if (!as.logical(length(x))) 
        return("")
    if (all(is.na(x))) 
        return(rep("NA", length = length(x)))
    if (!is.numeric(x)) 
        stop(paste(deparse(substitute(x)), "must be numeric"))
    att <- attributes(x)
    if (inherits(x, "times")) {
        if (missing(format.)) 
            format. <- switch(mode(att$format), character = , 
                list = rev(att$format)[[1]], name = , `function` = att$format, 
                `NULL` = format., stop("invalid output times format"))
        class(x) <- NULL
    }
    if (!is.character(format.)) {
        FUN <- switch(mode(format.), `function` = format., name = eval(format.), 
            stop(paste("unrecognized time format", deparse(substitute(format.)))))
        return(FUN(unclass(x), ...))
    }
    else format. <- rev(format.)[1]
    nas <- is.na(x)
    days <- abs(trunc(x))
    att$class <- att$format <- att$origin <- NULL
    if (any(days[!nas] > 0)) {
        attributes(x) <- att
        return(format(x))
    }
    sec <- round(24 * 3600 * abs(x))
    hh <- sec%/%3600
    mm <- (sec - hh * 3600)%/%60
    ss <- trunc(sec - hh * 3600 - 60 * mm)
    out <- list(h = substring(paste("0", hh, sep = ""), nchar(paste(hh))), 
        m = substring(paste("0", mm, sep = ""), nchar(paste(mm))), 
        s = substring(paste("0", ss, sep = ""), nchar(paste(ss))))
    style <- parse.format(format.)
    o <- style$periods
    if (!simplify) 
        out <- paste(out[[o[1]]], out[[o[2]]], out[[o[3]]], sep = style$sep)
    else {
        if (simplify == 1) {
            o <- o[o != "s"]
            out <- paste(out[[o[1]]], out[[o[2]]], sep = style$sep)
        }
        else out <- out$h
    }
    if (any(x[!nas] < 0)) 
        out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
    out[nas] <- NA
    out[x == Inf] <- "Inf"
    out[x == -Inf] <- "-Inf"
    attributes(out) <- att
    out
}, source = c("function (x, format. = \"h:m:s\", simplify = FALSE, ...) ", 
"{", "    if (!as.logical(length(x))) ", "        return(\"\")", 
"    if (all(is.na(x))) ", "        return(rep(\"NA\", length = length(x)))", 
"    if (!is.numeric(x)) ", "        stop(paste(deparse(substitute(x)), \"must be numeric\"))", 
"    att <- attributes(x)", "    if (inherits(x, \"times\")) {", 
"        if (missing(format.)) ", "            format. <- switch(mode(att$format), character = , ", 
"                list = rev(att$format)[[1]], name = , \"function\" = att$format, ", 
"                \"NULL\" = format., stop(\"invalid output times format\"))", 
"        class(x) <- NULL", "    }", "    if (!is.character(format.)) {", 
"        FUN <- switch(mode(format.), \"function\" = format., name = eval(format.), ", 
"            stop(paste(\"unrecognized time format\", deparse(substitute(format.)))))", 
"        return(FUN(unclass(x), ...))", "    }", "    else format. <- rev(format.)[1]", 
"    nas <- is.na(x)", "    days <- abs(trunc(x))", "    att$class <- att$format <- att$origin <- NULL", 
"    if (any(days[!nas] > 0)) {", "        attributes(x) <- att", 
"        return(format(x))", "    }", "    sec <- round(24 * 3600 * abs(x))", 
"    hh <- sec%/%3600", "    mm <- (sec - hh * 3600)%/%60", "    ss <- trunc(sec - hh * 3600 - 60 * mm)", 
"    out <- list(h = substring(paste(\"0\", hh, sep = \"\"), nchar(paste(hh))), ", 
"        m = substring(paste(\"0\", mm, sep = \"\"), nchar(paste(mm))), ", 
"        s = substring(paste(\"0\", ss, sep = \"\"), nchar(paste(ss))))", 
"    style <- parse.format(format.)", "    o <- style$periods", 
"    if (!simplify) ", "        out <- paste(out[[o[1]]], out[[o[2]]], out[[o[3]]], sep = style$sep)", 
"    else {", "        if (simplify == 1) {", "            o <- o[o != \"s\"]", 
"            out <- paste(out[[o[1]]], out[[o[2]]], sep = style$sep)", 
"        }", "        else out <- out$h", "    }", "    if (any(x[!nas] < 0)) ", 
"        out <- paste(ifelse(x < 0, \"-\", \" \"), out, sep = \"\")", 
"    out[nas] <- NA", "    out[x == Inf] <- \"Inf\"", "    out[x == -Inf] <- \"-Inf\"", 
"    attributes(out) <- att", "    out", "}"))
four <-
structure(function () 
par(mfrow = c(2, 2)), source = "function() par(mfrow=c(2,2))")
ftrcalc <-
structure(function (stg, hy = hy.default()) 
{
    if (hy <= 2001) {
        a <- 68.349
        b <- 1.677
        return(a * stg^b)
    }
    else {
        a <- 39.217
        b <- 34.423
        c <- -8.5
        return(a * stg^2 + b * stg + c)
    }
}, source = c("function(stg, hy = hy.default())", "{", "\tif(hy <= 2001) {", 
"\t\ta <- 68.349", "\t\tb <- 1.677", "\t\treturn(a * stg^b)", 
"\t}", "\telse {", "\t\ta <- 39.217", "\t\tb <- 34.423", "\t\tc <- -8.5", 
"\t\treturn(a * stg^2 + b * stg + c)", "\t}", "}"))
get.stagetics <-
structure(function (q, stn, hy, nticks = 4) 
{
    stg <- 0
    min.q <- min(q)
    max.q <- max(q)
    if (all(is.na(qcalc(stn, c(1, 10, 100), hy)))) 
        return(NULL)
    while (qcalc(stn, stg, hy) < min.q) stg <- stg + 1
    min.stg <- stgcalc(min.q, stg - 1, stg + 0.1, stn, hy)
    while (qcalc(stn, stg, hy) < max.q) stg <- stg + 1
    max.stg <- stgcalc(max.q, stg - 1, stg + 0.1, stn, hy)
    if (is.na(min.stg)) 
        min.stg <- 0
    if (is.na(max.stg)) 
        return(NULL)
    pretty(c(min.stg, max.stg), n = nticks)
}, source = c("function(q, stn, hy, nticks=4)", "{", "        # Dec 13, 2010: changed all(is.null) to all(is.na)", 
"        # compute stage tick locations for a discharge vector q", 
"        # stn is 3-letter station name", "        stg <- 0", 
"        min.q <- min(q)", "        max.q <- max(q)", "        # If no rating equation, return NULL", 
"        if (all(is.na(qcalc(stn,c(1,10,100),hy)))) return(NULL)", 
"        while (qcalc(stn,stg,hy) < min.q) stg <- stg + 1", "        # stg now exceeds min.q   Look between stg-1 and stg+.1 for ", 
"        # the stage that corresponds to min.q", "        min.stg <- stgcalc(min.q,stg-1,stg+.1,stn,hy)", 
"        while (qcalc(stn,stg,hy) < max.q) stg <- stg + 1", "        # stg now exceeds max.q   Look between stg-1 and stg+.1 for", 
"        # the stage that corresponds to max.q", "        max.stg <- stgcalc(max.q,stg-1,stg+.1,stn,hy)", 
"        if (is.na(min.stg)) min.stg <- 0", "        if (is.na(max.stg)) return(NULL)", 
"        pretty(c(min.stg,max.stg),n=nticks)", "}"))
get.thresh <-
structure(function (tstart, tmax, n, f = sqrt, finv = function(x) x^2) 
{
    dx <- (f(tmax) - f(tstart))/(n - 1)
    x <- seq(from = f(tstart), by = dx, length = n)
    round(finv(x))
}, source = c("function(tstart, tmax, n, f = sqrt, finv = function(x)", 
"x^2)", "{", "        # With TTS rev 5.0 no longer adds zero threshold", 
"        # Returns a vector of thresholds ", "        # Vector starts with tstart, and ends with tmax", 
"\tdx <- (f(tmax) - f(tstart))/(n - 1)", "\tx <- seq(from = f(tstart), by = dx, length = n)", 
"\tround(finv(x))", "}"))
getflows <-
structure(function (hy, stations) 
{
    allflows <- NULL
    allpoor <- NULL
    allareasknown <- F
    if (!exists("hectares")) {
        cat("'hectares' not found. All results will be in m3\n")
        area <- 1
    }
    else {
        if (all(toupper(stations) %in% names(hectares))) {
            allareasknown <- T
            cat("All results will be in m3/ha\n")
        }
    }
    for (STN in stations) {
        stn <- tolower(STN)
        STN <- toupper(STN)
        data <- read.flo(stn, hy)
        sd <- read.allstorms(stn, hy, match = F)
        nstorms <- dim(sd)[[1]]
        if (nstorms == 0) 
            stop("No storm dates found for ", STN, "\n")
        flows <- numeric(nstorms)
        pctpoor <- numeric(nstorms)
        if (exists("hectares")) {
            if (STN %in% names(hectares)) {
                area <- hectares[STN]
                if (!allareasknown) 
                  cat("Results for ", STN, " will be in m3/ha\n")
            }
            else {
                area <- 1
                cat("Watershed area not found for ", STN, "\n")
                cat("Results for ", STN, " will be in m3\n")
            }
        }
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            flows[i] <- round((sum(data$q[storm]) * 600)/35.315/area, 
                2)
            if (any(is.na(storm))) {
                pctpoor[i] <- NA
            }
            else {
                pctpoor[i] <- sum(data$stgcode[storm] %in% c(1, 
                  6, 7))/sum(storm)
                if (pctpoor[i] >= 0.25) 
                  flows[i] <- -flows[i]
            }
        }
        cat(STN, " ", flows, "\n")
        allflows <- c(allflows, flows)
        allpoor <- c(allpoor, pctpoor)
    }
    if (length(allflows) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    flow.mtx <- matrix(allflows, nr = nstorms, dimnames = dn)
    poor.mtx <- matrix(allpoor, nr = nstorms, dimnames = dn)
    list(flow = flow.mtx, pctpoor = round(poor.mtx, 3))
}, source = c("function(hy, stations)", "{", "\t# Modified 10/11/2011: ", 
"\t#    More friendly handling of missing hectares", "\t#    Also returns an error if no storm dates found for a station", 
"\t# Extracts flow volumes for all stations", "\t# Volume is converted from cfs to m3/hectare", 
"\t# If 25% or more of the data is quality 1, 6, or 7, ", "\t#   the volume is reported as a negative number.", 
"\t#   1=questionable, ", "\t#   6=reconstructed from another station", 
"\t#   7=free-hand reconstruction", "\t# Returns matrix of flow volumes and matrix of %poor", 
"\tallflows <- NULL", "\tallpoor <- NULL", "\tallareasknown <- F", 
"\tif (!exists(\"hectares\")) {", "\t\tcat(\"'hectares' not found. All results will be in m3\\n\")", 
"\t\tarea <- 1", "\t}", "\telse {", "\t\tif (all(toupper(stations) %in% names(hectares))) {", 
"\t\t\tallareasknown <- T", "\t\t\tcat(\"All results will be in m3/ha\\n\")", 
"\t\t}", "\t}", "\tfor(STN in stations) {", "\t\tstn <- tolower(STN)", 
"\t\tSTN <- toupper(STN)", "\t\tdata <- read.flo(stn,hy)", "\t\tsd <- read.allstorms(stn,hy,match=F)", 
"\t\tnstorms <- dim(sd)[[1]]", "\t\tif (nstorms == 0)", "\t\t\tstop(\"No storm dates found for \",STN,\"\\n\")", 
"\t\tflows <- numeric(nstorms)", "\t\tpctpoor <- numeric(nstorms)", 
"\t\tif (exists(\"hectares\")) {", "\t\t\tif (STN %in% names(hectares)) {", 
"\t\t\t\tarea <- hectares[STN]", "\t\t\t\tif (!allareasknown) ", 
"\t\t\t\t\tcat(\"Results for \",STN,\" will be in m3/ha\\n\")", 
"\t\t\t}", "\t\t\telse {", "\t\t\t\tarea <- 1", "\t\t\t\tcat(\"Watershed area not found for \",STN,\"\\n\")", 
"\t\t\t\tcat(\"Results for \",STN,\" will be in m3\\n\")", "\t\t\t}", 
"\t\t}", "\t\tfor(i in 1:nstorms) {", "\t\t\tstorm <- (data$chr >= sd$schron[i] & data$chr <= sd$", 
"\t\t\t\techron[i])", "\t\t\tflows[i] <- round((sum(data$q[storm]) * 600)/", 
"\t\t\t\t35.315/area, 2)", "\t\t\tif(any(is.na(storm))) {", "\t\t\t\tpctpoor[i] <- NA", 
"\t\t\t}", "\t\t\telse {", "\t\t\t\tpctpoor[i] <- sum(data$stgcode[storm] %in% c(1,6,7))/sum(storm)", 
"\t\t\t\tif(pctpoor[i] >= 0.25)", "\t\t\t\t\tflows[i] <-  - flows[i]", 
"\t\t\t}", "\t\t}", "\t\tcat(STN, \" \", flows, \"\\n\")", "\t\tallflows <- c(allflows, flows)", 
"\t\tallpoor <- c(allpoor, pctpoor)", "\t}", "\tif(length(allflows) != nstorms * length(stations))", 
"\t\tstop(\"Unequal numbers of storms\")", "\tdn <- list(sd$number, stations)", 
"\tflow.mtx <- matrix(allflows, nr = nstorms, dimnames = dn)", 
"\tpoor.mtx <- matrix(allpoor, nr = nstorms, dimnames = dn)", 
"\tlist(flow = flow.mtx, pctpoor = round(poor.mtx, 3))", "}"))
getORs <-
structure(function (stn, st, en, ttshome = getTTSenv("TTSHOME")) 
{
    for (i in st:en) {
        hy <- i
        file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
        abs.path <- paste(ttshome, stn, file, sep = "/")
        print(abs.path)
        columns <- c("yr", "mth", "day", "time", "dump", "bot", 
            "botcodes", "stg", "cstg", "stgcode", "Q", "turb", 
            "cturb", "tcode", "origQ", "S_QUAL", "S_TYPE")
        qdata <- read.csv(abs.path, header = FALSE, strip.white = T, 
            sep = ",")[, 1:length(columns)]
        names(qdata) <- columns
        ndata <- qdata[qdata$S_TYPE == 1, ]
        if (nrow(ndata) > 0) {
            ndata <- cbind(ndata, NA1 = NA, NA2 = NA, NA3 = NA)
            sdata <- ndata[, c("yr", "mth", "day", "time", "dump", 
                "NA1", "NA2", "stg", "NA3")]
            if (stn == "sfc") {
                sdata <- ndata[, c("yr", "mth", "day", "time", 
                  "dump", "NA1", "NA2", "stg", "NA3")]
            }
            else {
                if (stn == "nfc" || stn == "que") {
                  sdata <- ndata[, c("yr", "mth", "day", "time", 
                    "dump", "NA1", "stg", "NA2", "NA3")]
                }
                else {
                  sdata <- ndata[, c("yr", "mth", "day", "time", 
                    "dump", "stg", "NA1", "NA2", "NA3")]
                }
            }
            sdata$day <- zfill(sdata$day, 2)
            sdata$mth <- zfill(sdata$mth, 2)
            sdata$time <- zfill(sdata$time, 4)
            fileout1 <- paste(stn, zfill(hy, 2), ".or", sep = "")
            out.path1 <- paste(ttshome, stn, fileout1, sep = "/")
            write.table(sdata, out.path1, quote = FALSE, row.names = FALSE, 
                col.names = FALSE, sep = ",")
        }
        else {
            cat(stn, " had no observer record codes listed in the streamflow file for HY ", 
                hy, sep = "")
        }
    }
}, source = c("function(stn,st,en,ttshome = getTTSenv(\"TTSHOME\"))", 
"{", "#22 April 2016 JSeehafer", "#function to compile data corresponding to observer records from 62-95 streamflow files", 
"#input files are converted flo files (converted previously from \"old\" to \"new/current\" format)", 
"#example: 1994,08,01,0000,00,00,BX,0.199,0.199,-1,      NA,0000,  NA,2,7.65E-02,1,3", 
"#format : year,mth, day, time, dump, bottle, sample/threshold codes, stage, corr stage,", 
"#stage code, Adjuster-generated discharge, turb, corr turb, turb code,", 
"#original discharge, stage quality, stage type code ", "# STAGE QUALITYcode table:", 
"#1  good", "#2  fairly good", "#3  fairly poor", "#4  poor", 
"#5  missing stage or missing discharge (records of -0.999)", 
"", "# STAGE TYPE code table:", "# 0  no estimate available", 
"# 1  observer record", "# 2  chart recorder (digitized and interpolated)", 
"# 3  reconstructed", "# 4  HP-71 calculator (HY 1986-1995) or TattleTale (HY 1996-1997)", 
"# 5  projected from last 2 recorded HP-71 stages (HY86-HY90 only)", 
"", "#output (*.or files that Adjuster can read)", "#format: year,mth,day,time,dump,stilling well, right, left, average of R/L", 
"#order: stilling well,right staff,left staff,average", "#examples post-HY95:", 
"#(sfc): 2014,12,10,1540,9,NA,NA,1.44,NA", "#(nfc): 2005,09,07,1240,3,NA,0.31,NA,NA", 
"#(que): 2002,01,02,0910,15,NA,1.70,NA,NA", "#(arf): 2000,12,20,1348,11,NA,0.03,0.05,0.04", 
"#(hen): 2001,02,25,1015,14,0.45,0.41,0.44,0.425", "# adding all 85-95 NF tributary stages as well as ARF as \"stilling well\" if only one stage given", 
"", "for (i in st:en){", " hy<-i", " file <- paste(stn, zfill(hy, 2), \".flo\", sep = \"\")", 
" abs.path <- paste(ttshome,stn, file, sep=\"/\")", " print(abs.path)", 
" columns <- c(\"yr\",\"mth\",\"day\",\"time\",\"dump\",\"bot\",\"botcodes\",\"stg\",\"cstg\",\"stgcode\",\"Q\",\"turb\",\"cturb\",\"tcode\",\"origQ\",\"S_QUAL\",\"S_TYPE\")", 
" qdata <- read.csv(abs.path, header=FALSE, strip.white=T, sep = \",\")[, 1:length(columns)]", 
" names(qdata) <- columns", " ndata<-qdata[qdata$S_TYPE==1,]", 
" if (nrow(ndata) >0){", "  ndata<-cbind(ndata,\"NA1\"=NA,\"NA2\"=NA,\"NA3\"=NA)", 
"  sdata<-ndata[,c(\"yr\",\"mth\",\"day\",\"time\",\"dump\",\"NA1\",\"NA2\",\"stg\",\"NA3\")]", 
"", "  if(stn==\"sfc\"){", "   sdata<-ndata[,c(\"yr\",\"mth\",\"day\",\"time\",\"dump\",\"NA1\",\"NA2\",\"stg\",\"NA3\")]", 
"  } else {", "   if(stn==\"nfc\"||stn==\"que\"){", "     sdata<-ndata[,c(\"yr\",\"mth\",\"day\",\"time\",\"dump\",\"NA1\",\"stg\",\"NA2\",\"NA3\")]", 
"   } else {", "     sdata<-ndata[,c(\"yr\",\"mth\",\"day\",\"time\",\"dump\",\"stg\",\"NA1\",\"NA2\",\"NA3\")]", 
"   }", "  }", "  sdata$day<-zfill(sdata$day,2)", "  sdata$mth<-zfill(sdata$mth,2)", 
"  sdata$time<-zfill(sdata$time,4)", "  fileout1 <- paste(stn, zfill(hy, 2), \".or\", sep = \"\")", 
"  out.path1 <- paste(ttshome,stn, fileout1, sep=\"/\")", "  write.table(sdata,out.path1,quote=FALSE,row.names=FALSE,col.names = FALSE,sep=\",\")", 
" } else {", "  cat(stn, \" had no observer record codes listed in the streamflow file for HY \",hy,sep=\"\")", 
" } ", " }", "}"))
getpeaks <-
structure(function (hy, stations, match = T) 
{
    allpeaks <- NULL
    allqual <- NULL
    alltimes <- NULL
    allareasknown <- F
    if (!exists("hectares")) {
        cat("'hectares' not found. All results will be in m3/sec\n")
        area <- 1
    }
    else {
        if (all(toupper(stations) %in% names(hectares))) {
            allareasknown <- T
            cat("All results will be in m3/sec/ha\n")
        }
    }
    for (STN in stations) {
        stn <- tolower(STN)
        STN <- toupper(STN)
        data <- read.flo(stn, hy)
        sd <- read.allstorms(stn, hy, match)
        nstorms <- dim(sd)[[1]]
        if (nstorms == 0) 
            stop("No storm dates found for ", STN, "\n")
        peaks <- numeric(nstorms)
        qual <- numeric(nstorms)
        time <- numeric(nstorms)
        if (exists("hectares")) {
            if (STN %in% names(hectares)) {
                area <- hectares[STN]
                if (!allareasknown) 
                  cat("Results for ", STN, " will be in m3/sec/ha\n")
            }
            else {
                area <- 1
                cat("Watershed area not found for ", STN, "\n")
                cat("Results for ", STN, " will be in m3/sec\n")
            }
        }
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            qmax <- max(data$q[storm])
            maxima <- (data$q[storm] == qmax)
            peaks[i] <- signif(qmax/35.315/area, 4)
            qual[i] <- min(data$stgcode[storm][maxima])
            time[i] <- min(data$chr[storm][maxima])
            if (qual[i] %in% c(5, 6, 7)) 
                peaks[i] <- -peaks[i]
        }
        cat(STN, " ", peaks, "\n")
        allpeaks <- c(allpeaks, peaks)
        allqual <- c(allqual, qual)
        alltimes <- c(alltimes, time)
    }
    if (length(allpeaks) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    peak.mtx <- matrix(allpeaks, nr = nstorms, dimnames = dn)
    qual.mtx <- matrix(allqual, nr = nstorms, dimnames = dn)
    time.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)
    list(peak = peak.mtx, qual = qual.mtx, time = chron(time.mtx))
}, source = c("function(hy, stations, match = T)", "{", "\t# Modified 10/11/2011: ", 
"\t#    More friendly handling of missing hectares", "\t#    Also returns an error if no storm dates found for a station", 
"\t# Extracts storm peaks for all stations", "\t# Range of times can be restricted by using an alternate storm", 
"\t# dates file, if match=T, using the extension .alt", "\t# Peak is converted from cfs to m3/sec/hectare", 
"\t# Returns matrixes of peak volumes, codes, and peak times", 
"\t# If quality of peak = 1 ,6, or 7, returns negative value", 
"\t#    i.e. questionable or reconstructed", "\t# If multiple equal maxima, returns time of first maximum", 
"\tallpeaks <- NULL", "\tallqual <- NULL", "\talltimes <- NULL", 
"\tallareasknown <- F", "\tif (!exists(\"hectares\")) {", "\t\tcat(\"'hectares' not found. All results will be in m3/sec\\n\")", 
"\t\tarea <- 1", "\t}", "\telse {", "\t\tif (all(toupper(stations) %in% names(hectares))) {", 
"\t\t\tallareasknown <- T", "\t\t\tcat(\"All results will be in m3/sec/ha\\n\")", 
"\t\t}", "\t}", "\tfor(STN in stations) {", "\t\tstn <- tolower(STN)", 
"\t\tSTN <- toupper(STN)", "\t\tdata <- read.flo(stn,hy)", "\t\tsd <- read.allstorms(stn,hy,match)", 
"\t\tnstorms <- dim(sd)[[1]]", "\t\tif (nstorms == 0)", "\t\t\tstop(\"No storm dates found for \",STN,\"\\n\")", 
"\t\tpeaks <- numeric(nstorms)", "\t\tqual <- numeric(nstorms)", 
"\t\ttime <- numeric(nstorms)", "\t\tif (exists(\"hectares\")) {", 
"\t\t\tif (STN %in% names(hectares)) {", "\t\t\t\tarea <- hectares[STN]", 
"\t\t\t\tif (!allareasknown) ", "                              cat(\"Results for \",STN,\" will be in m3/sec/ha\\n\")", 
"\t\t\t}", " \t\t\telse {", "\t\t\t\tarea <- 1", "\t\t\t\tcat(\"Watershed area not found for \",STN,\"\\n\")", 
"\t\t\t\tcat(\"Results for \",STN,\" will be in m3/sec\\n\")", 
"\t\t\t}", "\t\t}", "\t\tfor(i in 1:nstorms) {", "\t\t\tstorm <- (data$chr >= sd$schron[i] & data$chr <= sd$", 
"\t\t\t\techron[i])", "\t\t\tqmax <- max(data$q[storm])", "\t\t\tmaxima <- (data$q[storm] == qmax)", 
"\t\t\tpeaks[i] <- signif(qmax/35.315/area, 4)", "\t\t\tqual[i] <- min(data$stgcode[storm][maxima])", 
"\t\t\ttime[i] <- min(data$chr[storm][maxima])", "\t\t\tif(qual[i] %in% c(5,6,7))", 
"\t\t\t\tpeaks[i] <-  - peaks[i]", "\t\t}", "\t\tcat(STN, \" \", peaks, \"\\n\")", 
"\t\tallpeaks <- c(allpeaks, peaks)", "\t\tallqual <- c(allqual, qual)", 
"\t\talltimes <- c(alltimes, time)", "\t}", "\tif(length(allpeaks) != nstorms * length(stations))", 
"\t\tstop(\"Unequal numbers of storms\")", "\tdn <- list(sd$number, stations)", 
"\tpeak.mtx <- matrix(allpeaks, nr = nstorms, dimnames = dn)", 
"\tqual.mtx <- matrix(allqual, nr = nstorms, dimnames = dn)", 
"\ttime.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)", 
"\tlist(peak = peak.mtx, qual = qual.mtx, time = chron(time.mtx))", 
"}"))
getpeakstage <-
structure(function (hy, stations, match = T) 
{
    allpeaks <- NULL
    allqual <- NULL
    alltimes <- NULL
    for (STN in stations) {
        stn <- tolower(STN)
        STN <- toupper(STN)
        data <- read.flo(stn, hy)
        sd <- read.allstorms(stn, hy, match)
        nstorms <- dim(sd)[[1]]
        if (nstorms == 0) 
            stop("No storm dates found for ", STN, "\n")
        peaks <- numeric(nstorms)
        qual <- numeric(nstorms)
        time <- numeric(nstorms)
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            peaks[i] <- max(data$stg[storm])
            maxima <- (data$stg[storm] == peaks[i])
            qual[i] <- min(data$stgcode[storm][maxima])
            time[i] <- min(data$chr[storm][maxima])
            if (qual[i] %in% c(5, 6, 7)) 
                peaks[i] <- -peaks[i]
        }
        cat(STN, " ", peaks, "\n")
        allpeaks <- c(allpeaks, peaks)
        allqual <- c(allqual, qual)
        alltimes <- c(alltimes, time)
    }
    if (length(allpeaks) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    peak.mtx <- matrix(allpeaks, nr = nstorms, dimnames = dn)
    qual.mtx <- matrix(allqual, nr = nstorms, dimnames = dn)
    time.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)
    list(peak = peak.mtx, qual = qual.mtx, time = chron(time.mtx))
}, source = c("function(hy, stations, match = T)", "{", "\t# Created 10/16/2010 from getpeaks function.", 
"\t# Extracts peak storm stages for all stations", "\t# Range of times can be restricted by using an alternate storm", 
"\t# dates file, if match=T, using the extension .alt", "\t# Returns matrixes of stage peaks, codes, and peak times", 
"\t# If quality of peak = 1 ,6, or 7, returns negative value", 
"\t#    i.e. questionable or reconstructed", "\t# If multiple equal maxima, returns time of first maximum", 
"\tallpeaks <- NULL", "\tallqual <- NULL", "\talltimes <- NULL", 
"\tfor(STN in stations) {", "\t\tstn <- tolower(STN)", "\t\tSTN <- toupper(STN)", 
"\t\tdata <- read.flo(stn,hy)", "\t\tsd <- read.allstorms(stn,hy,match)", 
"\t\tnstorms <- dim(sd)[[1]]", "\t\tif (nstorms == 0)", "\t\t\tstop(\"No storm dates found for \",STN,\"\\n\")", 
"\t\tpeaks <- numeric(nstorms)", "\t\tqual <- numeric(nstorms)", 
"\t\ttime <- numeric(nstorms)", "\t\tfor(i in 1:nstorms) {", 
"\t\t\tstorm <- (data$chr >= sd$schron[i] & data$chr <= sd$", 
"\t\t\t\techron[i])", "\t\t\tpeaks[i] <- max(data$stg[storm])", 
"\t\t\tmaxima <- (data$stg[storm] == peaks[i])", "\t\t\tqual[i] <- min(data$stgcode[storm][maxima])", 
"\t\t\ttime[i] <- min(data$chr[storm][maxima])", "\t\t\tif(qual[i] %in% c(5,6,7))", 
"\t\t\t\tpeaks[i] <-  - peaks[i]", "\t\t}", "\t\tcat(STN, \" \", peaks, \"\\n\")", 
"\t\tallpeaks <- c(allpeaks, peaks)", "\t\tallqual <- c(allqual, qual)", 
"\t\talltimes <- c(alltimes, time)", "\t}", "\tif(length(allpeaks) != nstorms * length(stations))", 
"\t\tstop(\"Unequal numbers of storms\")", "\tdn <- list(sd$number, stations)", 
"\tpeak.mtx <- matrix(allpeaks, nr = nstorms, dimnames = dn)", 
"\tqual.mtx <- matrix(allqual, nr = nstorms, dimnames = dn)", 
"\ttime.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)", 
"\tlist(peak = peak.mtx, qual = qual.mtx, time = chron(time.mtx))", 
"}"))
getStormdates <-
structure(function (stn, hy) 
{
    objects2 <- objects(pat = paste(stn, "...ssc", sep = ""), 
        pos = 1)
    objects3 <- objects(pat = paste(stn, "....ssc", sep = ""), 
        pos = 1)
    objects4 <- objects(pat = paste(stn, ".....ssc", sep = ""), 
        pos = 1)
    objects <- c(objects2, objects3, objects4)
    if (length(objects) == 0) 
        stop("No objects found matching templates 'stn...ssc' or 'stn....ssc'")
    number <- schron <- echron <- numeric(0)
    for (newobj in objects) {
        newdata <- eval(as.name(newobj))
        if (data.class(newdata) != "data.frame") 
            cat(paste("Object", newobj, "is not a data frame\n"))
        newnum <- as.numeric(substring(newobj, 4, nchar(newobj) - 
            4))
        number <- c(number, newnum)
        schron <- c(schron, newdata$chr[1])
        echron <- c(echron, last.val(newdata$chr))
    }
    data <- data.frame(number = number, schron = as.chron(schron), 
        echron = as.chron(echron))
    data <- data[data$number%/%100 == as.numeric(hy), ]
    data <- data[!(data$number%%100 %in% c(0, 99)), ]
    data[order(data$schron), ]
}, source = c("function(stn, hy)", "{", "  # Gets storm dates by Looking at all objects whose names match one", 
"  # of the templates stn...ssc or stn....ssc ( . means any character)", 
"  # Hopefully these are the 10-minute SSC event files created by tts.ssc", 
"  # stn = 3-character station name", "  # First digit(s) of storm number is(are) assumed to be water year", 
"  ", "  objects2 <- objects(pat = paste(stn, \"...ssc\", sep = \"\"),pos=1)", 
"  objects3 <- objects(pat = paste(stn, \"....ssc\", sep = \"\"),pos=1)", 
"  objects4 <- objects(pat = paste(stn, \".....ssc\", sep = \"\"),pos=1)", 
"  objects <- c(objects2, objects3, objects4)", "  if (length(objects) == 0)", 
"    stop(\"No objects found matching templates 'stn...ssc' or 'stn....ssc'\")", 
"  number <- schron <- echron <- numeric(0)", "  for(newobj in objects) {", 
"    newdata <- eval(as.name(newobj))", "    if (data.class(newdata) != \"data.frame\")", 
"      cat(paste(\"Object\",newobj,\"is not a data frame\\n\"))", 
"    newnum <- as.numeric(substring(newobj, 4, nchar(newobj) - 4))", 
"    number <- c(number,newnum)", "    schron <- c(schron, newdata$chr[1])", 
"    echron <- c(echron, last.val(newdata$chr))", "  }", "  data <- data.frame(number=number,schron=as.chron(schron),echron=as.chron(echron))", 
"  data <- data[data$number %/% 100 == as.numeric(hy), ]  # current water year only", 
"  data <- data[!(data$number %% 100 %in% c(0,99)),]  # storm numbers 1-99 only", 
"  data[order(data$schron),]", "}"))
getTTSenv <-
structure(function (varname) 
{
    if (!file.exists("TTSenvironment.txt")) 
        return(data.frame(row.names = c("TTSHOME", "LOGGERHOME"), 
            value = c(NA, NA), stringsAsFactors = F))
    data <- read.table("TTSenvironment.txt", sep = "=", row.names = 1, 
        as.is = T, header = F, col.names = c("name", "value"))
    if (missing(varname)) 
        data
    else data[varname, "value"]
}, source = c("function(varname) {", "   if (!file.exists(\"TTSenvironment.txt\")) ", 
"      return(data.frame(row.names=c(\"TTSHOME\",\"LOGGERHOME\"),value=c(NA,NA),stringsAsFactors=F))", 
"   data <- read.table(\"TTSenvironment.txt\",sep=\"=\",row.names=1,as.is=T,header=F,col.names=c(\"name\",\"value\"))", 
"   if (missing(varname)) data", "   else data[varname,\"value\"]", 
"}"))
getturb <-
structure(function (hy, stations, match = T, opt = 0) 
{
    allturbs <- NULL
    alltimes <- NULL
    for (STN in stations) {
        stn <- tolower(STN)
        data <- read.flo(stn, hy)
        sd <- read.allstorms(stn, hy, match)
        nstorms <- dim(sd)[[1]]
        turb <- numeric(nstorms)
        time <- numeric(nstorms)
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            stormturb <- data$turb[storm]
            turbcodes <- data$turbcode[storm]
            turbcodes <- rev(turbcodes[order(stormturb)])
            sortturb <- rev(stormturb[order(stormturb)])
            if (opt > 0) {
                turb[i] <- sortturb[6 * opt]
                nflags <- sum(sortturb[1:(6 * opt)] %in% c(1, 
                  6, 7))
                if (nflags/6 * opt > 0.5) 
                  turb[i] = -turb[i]
            }
            else {
                turb[i] <- sortturb[1]
                maxima <- (data$turb[storm] == sortturb[1])
                time[i] <- min(data$chr[storm][maxima])
                if (turbcodes[1] %in% c(1, 6, 7)) 
                  turb[i] <- -turb[i]
            }
        }
        cat(STN, " ", turb, "\n")
        allturbs <- c(allturbs, turb)
        if (opt == 0) 
            alltimes <- c(alltimes, time)
    }
    if (length(allturbs) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    turb.mtx <- matrix(allturbs, nr = nstorms, dimnames = dn)
    if (opt == 0) {
        time.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)
        return(list(turb = turb.mtx, time = chron(time.mtx)))
    }
    else return(turb.mtx)
}, source = c("function(hy, stations, match=T, opt=0)", "{", 
"\t# Extracts turbidity for all stations", "\t# Options are peak (opt=0), or exeedence values (opt=# hrs exceeded)", 
"\t# Returns matrixes of turbidity and, if opt=0, times of peak turbidity", 
"      # If multiple peaks of equal turbidity, first time is returned", 
"\t# For opt=0, if quality of peak = 1 ,6, or 7, returns negative value", 
"\t#    i.e. questionable or reconstructed", "\t# For opt=exceedence, returns negative value if >50% of higher turbidity", 
"\t#    values have code 1, 6, or 7.", "\tallturbs <- NULL", 
"\talltimes <- NULL", "\tfor(STN in stations) {", "\t\tstn <- tolower(STN)", 
"\t\tdata <- read.flo(stn,hy)", "\t\tsd <- read.allstorms(stn,hy,match)", 
"\t\tnstorms <- dim(sd)[[1]]", "\t\tturb <- numeric(nstorms)", 
"\t\ttime <- numeric(nstorms)", "\t\tfor(i in 1:nstorms) {", 
"\t\t\tstorm <- (data$chr >= sd$schron[i] & data$chr <= sd$", 
"\t\t\t\techron[i])", "                  stormturb <- data$turb[storm]", 
"                  turbcodes <- data$turbcode[storm]", "                  turbcodes <- rev(turbcodes[order(stormturb)])", 
"\t\t\tsortturb <- rev(stormturb[order(stormturb)])", "                  if (opt > 0) {", 
"                      turb[i] <- sortturb[6*opt]", "                      nflags <- sum(sortturb[1:(6*opt)] %in% c(1,6,7))", 
"                      if (nflags/6*opt > 0.50) turb[i] = - turb[i]", 
"                  }", "                  else {", "                      turb[i] <- sortturb[1]", 
"\t\t\t    maxima <- (data$turb[storm] == sortturb[1])", "\t\t\t    time[i] <- min(data$chr[storm][maxima])", 
"\t\t\t    if(turbcodes[1] %in% c(1,6,7))", "\t\t\t\t turb[i] <-  - turb[i]", 
"                  }", "\t\t}", "\t\tcat(STN, \" \", turb, \"\\n\")", 
"\t\tallturbs <- c(allturbs, turb)", "\t\tif (opt == 0) alltimes <- c(alltimes, time)", 
"\t}", "\tif(length(allturbs) != nstorms * length(stations))", 
"\t\tstop(\"Unequal numbers of storms\")", "\tdn <- list(sd$number, stations)", 
"      turb.mtx <- matrix(allturbs, nr = nstorms, dimnames = dn)", 
"      if (opt==0) {", "\t   time.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)", 
"\t   return(list(turb = turb.mtx, time = chron(time.mtx)))", 
"      }", "      else", "         return(turb.mtx)", "}"))
hy.default <-
structure(function () 
{
    today <- date()
    mon <- substring(today, 5, 7)
    nc <- nchar(today)
    year <- as.numeric(substring(today, nc - 3, nc))
    ifelse(mon %in% c("Aug", "Sep", "Oct", "Nov", "Dec"), year + 
        1, year)
}, source = c("function () ", "{", "# Returns hydro year of today's date", 
"  today <- date()", "  mon <- substring(today,5,7)", "  nc <- nchar(today)", 
"  year <- as.numeric(substring(today,nc-3,nc))", "  ifelse (mon %in% c(\"Aug\",\"Sep\",\"Oct\",\"Nov\",\"Dec\"), year+1, year)", 
"}"))
hydro.year <-
structure(function (chr, start = "Aug") 
{
    yr <- fac2num(years(chr))
    ifelse(months(chr) < start, yr, yr + 1)
}, source = c("function(chr, start=\"Aug\") {", "#  Start of hydroyear is first day of given month.  Must match one of:", 
"#  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec   ", 
"   yr <- fac2num(years(chr))", "   ifelse(months(chr) < start, yr, yr+1)", 
"}"))
interp.chron <-
structure(function (data, chr, var) 
{
    t1 <- round2tenmin(chr, "down")
    t2 <- round2tenmin(chr, "up")
    t <- sort(unique(c(t1, t2)))
    rownames <- format(chron(t))
    row.names(data) <- format(data$chr)
    matchdat <- data[rownames, ]
    approx(matchdat[, "chr"], matchdat[, var], chr, rule = 1)$y
}, source = c("function(data,chr,var) {", "    # Interpolates a variable in a data frame based on a chron object", 
"    # 'data' is a data frame with a chron variable named \"chr\"", 
"    # var is a character variable containing the name of a column", 
"    #   in the 'data' data frame that is to be interpolated", 
"    # Interpolates var to the times given by the chron object chr", 
"    # Method:", "    #   Reduce the size of the problem by getting just the data records ", 
"    #   preceding and following each value of chr", "    #   Then uses the approx function to interpolate", 
"    t1 <- round2tenmin(chr,\"down\")", "    t2 <- round2tenmin(chr,\"up\")", 
"    t <- sort(unique(c(t1,t2)))", "    rownames <- format(chron(t))", 
"    row.names(data) <- format(data$chr)", "    matchdat <- data[rownames, ]", 
"    approx(matchdat[,\"chr\"], matchdat[,var], chr, rule=1)$y", 
"}"))
last.val <-
structure(function (x) 
x[length(x)], source = "function(x) x[length(x)]")
lineartime <-
structure(function (stn, hy, sdate, stime, edate, etime, interval = 10, 
    ssc1 = 0, ssc2 = 0, long = T, adj = T, exclude = TRUE, units = "cfs") 
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
    if (n == 0) 
        xy <- approx(x, y, xout = index, rule = 2)
    else xy <- approx(x, y, xout = index[-sam], rule = 2)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    disch <- pop$q/qfactor
    ssc0[xy$x] <- xy$y
    estflux <- sum(k * disch * ssc0)
    detach(2)
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            type = "linear", meth = 3, chr = mergepop$chr, predssc = ssc0)
    else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
        type = "linear", meth = 3)
}, source = c("function(stn, hy, sdate, stime, edate, etime, interval = 10, ssc1 = 0, ssc2 = 0,", 
"        long = T, adj = T, exclude = TRUE, units = \"cfs\")", 
"{", "       #  Revised 2016apr8.  Changed defaults to units=\"cfs\" and interval=10 for CC (jes, 1/2/2109: and adj=T)", 
"\t #  Revised 2011feb10   Added exclude option.  The exclude argument", 
"       #     is a logical variable, by default TRUE, governing whether to exclude", 
"       #     samples from analyses according to the column of the same name,", 
"       #     \"exclude\", in the sediment data frame.  This column is set and", 
"       #     modified interactively from the TTS menu.", "       #  5/18/07 now calls dis.adjust to control extrapolated adjustments", 
"       #  5/5/2006 made it work even if there are no samples in specified period", 
"       #  7/26/2004 fixed bug when x already included 1 or N", 
"       #  4/16/2003 fixed bug x <- c(0,sam,N) changed to c(1,sam,N)", 
"       #  Effect was negligible except for extremely short sequences", 
"       #", "       #  Extracts records from pop between given starting date/time", 
"       #  and ending date/time and merges with samples from the", 
"       #  correct station for the same time period.  Then estimates", 
"       #  the load using local linear functions of time.", "       #  Uses approx() with the 2 points on each side (4 total).", 
"       #  approx() will not extrapolate beyond those 4 discharges, ", 
"       #  so a log-log fit is applied to all data when approx returns NA.", 
"       #  Extrapolates to (0,0) at lower end.", "       #  Assumes time interval of 10 minutes if not specified", 
"       #  ssc1 and ssc2 specify endpoint SSC when no sample exists", 
"       #      at sdate,stime or edate,etime.  No effect otherwise.", 
"       #  long=F gives abbreviated output", "       #  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"       #      adj=T does not affect the values ssc1 and ssc2", 
"       #  Set units=\"cumecs\" if discharge is in m3/sec instead of cfs", 
"", "      hy <- zfill(hy, 2)", "      k <- 0.06 * interval", 
"      pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"      pop <- subtime(pop, sdate, stime, edate, etime)", "\tif(any(is.na(pop$q)))", 
"\t\tstop(\"\\nSorry, the specified segment has missing discharge values\")", 
"      samples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"      samples <- subtime(samples, sdate, stime, edate, etime)", 
"      if (exclude & !is.null(samples$exclude))", "           samples <- samples[!samples$exclude,  ]", 
"      mergesam <- samples[, c(\"chr\", \"ssc\")]", "\tif (dim(samples)[1] == 1)", 
"\t\tmergesam <- as.data.frame(mergesam)", "\tif (dim(mergesam)[1] == 0) {", 
"\t\tmergepop <- pop", "\t\tmergepop$ssc <- rep(NA, dim(mergepop)[1])", 
"\t}", "\telse {", "        \tmergepop <- merge(pop, mergesam, all.x = T)", 
"        \tmergepop <- mergepop[order(mergepop$chr),  ]", "\t}", 
"\tconf <- intersect(names(mergepop),objects(1))", "\tif (length(conf) > 0) {", 
"\t\tcat(\"The following objects conflict with object names in lineartime:\\n\")", 
"\t\tcat(conf,\"\\n\")", "\t\tcat(\"Please remove conflicting objects before proceeding.\\n\")", 
"\t\treturn(invisible())", "\t}", "\tconf <- intersect(names(mergepop),objects(1))", 
"\tif (length(conf) > 0) {", "\t\tcat(\"The following objects conflict with object names in lineartime:\\n\")", 
"\t\tcat(conf,\"\\n\")", "\t\tcat(\"Please remove conflicting objects before proceeding.\\n\")", 
"\t\treturn(invisible())", "\t}", "      attach(mergepop)", "\tssc0 <- ssc", 
"\tif(adj) ssc0 <- dis.adjust(stn, ssc0)", "        N <- dim(mergepop)[1]", 
"        index <- 1:N", "        sam <- index[ - which.na(ssc0)]", 
"        n <- length(sam)", "        x <- sam", "        y <- ssc0[sam]", 
"        if (!(1 %in% x)) {", "                x <- c(1,x)", 
"                y <- c(ssc1,y)", "        }", "        if (!(N %in% x)) {", 
"                x <- c(x,N)", "                y <- c(y,ssc2)", 
"        }", "        if(n == 0)", "                xy <- approx(x, y, xout = index, rule = 2)", 
"        else xy <- approx(x, y, xout = index[ - sam], rule = 2)", 
"        if(units == \"cfs\")", "                qfactor <- 35.31467", 
"        else if(units == \"cumecs\")", "                qfactor <- 1", 
"        else stop(\"flow units must be cfs or cumecs\")", "        disch <- pop$q/qfactor", 
"        # cubic meters per sec", "        ssc0[xy$x] <- xy$y", 
"        estflux <- sum(k * disch * ssc0)", "        detach(2)", 
"        if(long)", "                list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, type=\"linear\", meth = 3, chr = ", 
"                        mergepop$chr, predssc = ssc0)", "        else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, type=\"linear\", meth = 3)", 
"}"))
lineartime.gui <-
structure(function () 
{
    model <- function(panel) {
        with(panel, {
            stn <- tolower(pars["stn"])
            hy4 <- pars["hy4"]
            firstdt1 <- pars["firstdt1"]
            lastdt1 <- pars["lastdt1"]
            interval <- as.numeric(pars["interval"])
            ssc1 <- as.numeric(pars["ssc1"])
            ssc2 <- as.numeric(pars["ssc2"])
            result <- pars["result"]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- as.vector(substring(hy4, 3, 4))
            setTTSenv(c("STN", "HY", "SDATE", "EDATE", "INTERVAL", 
                "ADJUST"), c(stn, hy4, firstdt1, lastdt1, interval, 
                checkvars["adj"]))
            sdate <- paste(substring(firstdt1, 7, 8), substring(firstdt1, 
                1, 2), substring(firstdt1, 4, 5), sep = "")
            stime <- paste(substring(firstdt1, 10, 11), substring(firstdt1, 
                13, 14), sep = "")
            edate <- paste(substring(lastdt1, 7, 8), substring(lastdt1, 
                1, 2), substring(lastdt1, 4, 5), sep = "")
            etime <- paste(substring(lastdt1, 10, 11), substring(lastdt1, 
                13, 14), sep = "")
            arglist <- list(stn, hy2, sdate, stime, edate, etime, 
                interval = interval, ssc1 = ssc1, ssc2 = ssc2, 
                exclude = checkvars["exclude"], adj = checkvars["adj"])
            res <- do.call("lineartime", arglist)
            saveCommand(stn, hy2, "lineartime", arglist, result, 
                checkvars["savecmd"])
            if (result != "") {
                assign(result, res, envir = .GlobalEnv)
                cat("Result saved in workspace as", result, "\n")
            }
            else cat("To save results, enter name of output object and press OK\n")
        })
        panel
    }
    nothing <- function(panel) panel
    env <- getTTSenv()
    init.stn <- env["STN", ]
    init.hy <- env["HY", ]
    init.sdate <- env["SDATE", ]
    init.edate <- env["EDATE", ]
    interval <- env["INTERVAL", ]
    init.ssc1 <- 0
    init.ssc2 <- 0
    init.adjust <- env["ADJUST", ]
    if (is.na(init.adjust)) 
        init.adjust <- F
    pars <- c(init.stn, init.hy, interval, init.sdate, init.edate, 
        init.ssc1, init.ssc2, "")
    panel <- rp.control("Linear time model for SSC")
    my.textentry(panel, pars, labels = c("Station", "Water year", 
        "Recording interval (mins)", "Starting date/time (m/d/y h:m)", 
        "Ending date/time (m/d/y h:m)", "SSC at start of period", 
        "SSC at end of period", "Output object name"), names = c("stn", 
        "hy4", "interval", "firstdt1", "lastdt1", "ssc1", "ssc2", 
        "result"), title = "Enter values", initval = pars)
    rp.checkbox(panel, checkvars, initval = c(T, init.adjust, 
        T), title = "Other options", labels = c("Exclude previously flagged points (those with exclude=T)", 
        "Adjust SSC to depth-integrated equivalent", "Save command to file"), 
        names = c("exclude", "adj", "savecmd"), action = nothing)
    rp.button(panel, action = model, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run lineartime()  ", 
"  model <- function(panel) {", "    with(panel, {", "      stn <- tolower(pars[\"stn\"])", 
"      hy4 <- pars[\"hy4\"]", "      firstdt1 <- pars[\"firstdt1\"]", 
"      lastdt1 <- pars[\"lastdt1\"]", "      interval <- as.numeric(pars[\"interval\"])", 
"      ssc1 <- as.numeric(pars[\"ssc1\"])", "      ssc2 <- as.numeric(pars[\"ssc2\"])", 
"      result <- pars[\"result\"]", "      ", "      if (nchar(hy4) < 3) {", 
"        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- as.vector(substring(hy4,3,4))  # as.vector required to get rid of the name \"hy4\"", 
"      # Save current values in TTS Environment", "      setTTSenv(c(\"STN\",\"HY\",\"SDATE\",\"EDATE\",\"INTERVAL\",\"ADJUST\"),", 
"                c(stn,hy4,firstdt1,lastdt1,interval,checkvars[\"adj\"]))", 
"      sdate <- paste(substring(firstdt1,7,8),substring(firstdt1,1,2),substring(firstdt1,4,5),sep=\"\")", 
"      stime <- paste(substring(firstdt1,10,11),substring(firstdt1,13,14),sep=\"\")", 
"      edate <- paste(substring(lastdt1,7,8),substring(lastdt1,1,2),substring(lastdt1,4,5),sep=\"\")", 
"      etime <- paste(substring(lastdt1,10,11),substring(lastdt1,13,14),sep=\"\")", 
"      arglist <- list(stn,hy2,sdate,stime,edate,etime,interval=interval,ssc1=ssc1,ssc2=ssc2,exclude=checkvars[\"exclude\"],adj=checkvars[\"adj\"])", 
"      # print(arglist)", "      ", "      res <- do.call(\"lineartime\",arglist)", 
"      saveCommand(stn,hy2,\"lineartime\",arglist,result,checkvars[\"savecmd\"])", 
"      if (result!=\"\") {", "        assign(result,res,envir=.GlobalEnv)", 
"        cat(\"Result saved in workspace as\",result,\"\\n\")", 
"      }", "      else ", "        cat(\"To save results, enter name of output object and press OK\\n\")", 
"    })", "    panel", "  }", "  nothing <- function(panel) panel", 
"  env <- getTTSenv()", "  init.stn <- env[\"STN\",]", "  init.hy <- env[\"HY\",]", 
"  init.sdate <- env[\"SDATE\",]", "  init.edate <- env[\"EDATE\",]", 
"  interval <- env[\"INTERVAL\",]", "  init.ssc1 <- 0", "  init.ssc2 <- 0", 
"  init.adjust <- env[\"ADJUST\",]", "  if (is.na(init.adjust))", 
"    init.adjust <- F", "  ", "  pars <- c(init.stn,init.hy,interval,init.sdate,init.edate,init.ssc1,init.ssc2,\"\")", 
"  panel <- rp.control(\"Linear time model for SSC\")", "  my.textentry(panel,pars,labels=c(\"Station\",\"Water year\",\"Recording interval (mins)\",\"Starting date/time (m/d/y h:m)\",\"Ending date/time (m/d/y h:m)\",", 
"                                   \"SSC at start of period\",\"SSC at end of period\",\"Output object name\"),", 
"               names=c(\"stn\",\"hy4\",\"interval\",\"firstdt1\",\"lastdt1\",\"ssc1\",\"ssc2\",\"result\"),", 
"               title=\"Enter values\",initval=pars)", "  rp.checkbox(panel, checkvars, initval = c(T,init.adjust,T), title=\"Other options\", ", 
"              labels=c(\"Exclude previously flagged points (those with exclude=T)\",", 
"                       \"Adjust SSC to depth-integrated equivalent\",\"Save command to file\"),", 
"              names=c(\"exclude\",\"adj\",\"savecmd\"),action=nothing)", 
"  rp.button(panel, action = model, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
lineartime.vr <-
structure(function (stn, hy, sdate, stime, edate, etime, interval = 10, 
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
}, source = c("function(stn, hy, sdate, stime, edate, etime, interval = 10, ssc1 = 0, ssc2 = 0,", 
"\tlong = T, adj = F, units = \"cfs\")", "{", "\t#  Created 8/30/2004 JL", 
"\t#", "\t#  Extracts records from vineyard runoff flo and lab data", 
"\t#  between given starting and ending dates/times", "", "\t#  Then estimates the load using local linear functions of time.", 
"\t#  Uses approx() with the 2 points on each side (4 total).", 
"\t#  approx() will not extrapolate beyond those 4 discharges, ", 
"", "\t#  Extrapolates to (0,0) at lower end if ssc1 and ssc2 not given.", 
"\t#  Assumes time interval of 10 minutes if not specified", 
"\t#  ssc1 and ssc2 specify endpoint SSC when no sample exists", 
"\t#      at sdate,stime or edate,etime.  No effect otherwise.", 
"\t#  long=F gives abbreviated output", "\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\t#      adj=T does not affect the values ssc1 and ssc2", "\t#  Set units=\"cumecs\" if discharge is in m3/sec instead of cfs", 
"", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tS <- toupper(substring(stn, 1, 2))", 
"\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tpop <- subtime(pop, sdate, stime, edate, etime)", "\tsamples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tsamples <- subtime(samples, sdate, stime, edate, etime)", 
"\tsamples <- samples[!is.na(samples$ssc),]", "\tsedtimes <- format(samples$chr)", 
"\tn <- dim(samples)[1]", "\tN <- dim(pop)[1]", "\tif (!(format(pop$chr[1]) %in% sedtimes)) {", 
"\t\tsamples <- samples[c(1,1:n),]", "\t\tn <- n+1", "\t\tsamples[1,\"chr\"] <- pop$chr[1]", 
"\t\tsamples[1,\"ssc\"] <- ssc1", "\t}", "\tif (!(format(pop$chr[N]) %in% sedtimes)) {", 
"\t\tsamples <- samples[c(1:n,n),]", "\t\tn <- n+1", "\t\tsamples[n,\"chr\"] <- pop$chr[N]", 
"\t\tsamples[n,\"ssc\"] <- ssc2", "\t}", "\tssc <- approx(samples$chr, samples$ssc, pop$chr)$y\t", 
"\tif(adj)", "\t\tssc <- exp(discoef[S, \"a\"]) * ssc^discoef[S, \"b\"]", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tdisch <- pop$q/qfactor", "\t# cubic meters per sec", "\testflux <- sum(k * disch * ssc)", 
"", "\tif(long)", "\t\tlist(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, type=\"linear\", meth = 3, chr = ", 
"\t\t\tpop$chr, predssc = ssc)", "\telse list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, type=\"linear\", meth = 3)", 
"}"))
listfuncs <-
structure(function (env = .GlobalEnv, data.class = "function", 
    ...) 
{
    objnames <- objects(env)
    dclass <- sapply(objnames, function(x) data.class(eval(as.name(x))))
    objnames[dclass == data.class]
}, source = c("function(env=.GlobalEnv,data.class=\"function\",...)", 
"{", "# By default, returns the names of all functions in .GlobalEnv", 
"# Can in general be used to return object names for any data class", 
"# Arguments are the same as for built-in objects() function", 
"   objnames <- objects(env)", "   dclass <- sapply(objnames, function(x) data.class(eval(as.name(x))))", 
"   objnames[dclass==data.class]", "}"))
localflowsrc <-
structure(function (stn, hy, sdate, stime, edate, etime, interval = 10, 
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
        cat("The following objects conflict with object names in localflowsrc:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    attach(mergepop)
    if (adj) 
        ssc <- exp(discoef[S, "a"]) * ssc^discoef[S, "b"]
    N <- dim(mergepop)[1]
    index <- 1:N
    sam <- index[-which.na(ssc)]
    diffs <- diff(c(0, sam, N))
    n <- length(sam)
    low1 <- rep(c(NA, sam), diffs)
    low2 <- rep(c(NA, NA, sam[-n]), diffs)
    high1 <- rep(c(sam, NA), diffs)
    high2 <- rep(c(sam[-1], NA, NA), diffs)
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    q <- q/qfactor
    x <- matrix(q[c(low2, low1, high1, high2)], ncol = 4)
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
        xy <- approx(x1, y1, xout = q[i])
        newssc[i] <- xy$y
    }
    unfit <- is.na(newssc)
    if (sum(unfit) != 0) 
        newssc[unfit] <- predict.loglog(q[sam], ssc[sam], q[unfit])
    estflux <- sum(k * q * newssc)
    detach(2)
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            chr = mergepop$chr, predssc = newssc, meth = 2)
    else list(yhat = estflux, n = n)
}, source = c("function(stn, hy, sdate, stime, edate, etime, interval = 10, opt = 1, long = T,", 
"        adj = T, units = \"cfs\")", "{", "        #  Extracts records from pop between given starting date/time", 
"        #  and ending date/time and merges with samples from the", 
"        #  correct station for the same time period.  Then estimates", 
"        #  the load using local linear functions of discharge.", 
"        #  Uses approx() with the 2 points on each side (4 total).", 
"        #  approx() will not extrapolate beyond those 4 discharges, ", 
"        #  so a log-log fit is applied to all data when approx returns NA.", 
"        #  Assumes a 10-minute recording interval if not specified", 
"        #  Extrapolates to (0,0) at lower end unless opt = 2, in which", 
"        #  case the log-log fit is applied.", "        #  long=F gives abbreviated output", 
"        #  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"        #  Use units=\"cumecs\" if discharge is in m3/sec and not cfs", 
"        hy <- zfill(hy, 2)", "        k <- 0.06 * interval", 
"        S <- toupper(substring(stn, 1, 2))", "        pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"        samples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"        pop <- subtime(pop, sdate, stime, edate, etime)", "        samples <- subtime(samples, sdate, stime, edate, etime)", 
"        mergesam <- samples[, c(\"chr\", \"ssc\")]", "        mergepop <- merge(pop, mergesam, all.x = T)", 
"        mergepop <- mergepop[order(mergepop$chr),  ]", "        conf <- intersect(names(mergepop),objects(1))", 
"        if (length(conf) > 0) {", "                cat(\"The following objects conflict with object names in localflowsrc:\\n\")", 
"                cat(conf,\"\\n\")", "                cat(\"Please remove conflicting objects before proceeding.\\n\")", 
"                return(invisible())", "        }", "        attach(mergepop)", 
"        if(adj)", "                ssc <- exp(discoef[S, \"a\"]) * ssc^discoef[S, \"b\"]", 
"        N <- dim(mergepop)[1]", "        index <- 1:N", "        sam <- index[ - which.na(ssc)]", 
"        diffs <- diff(c(0, sam, N))", "        n <- length(sam)", 
"        low1 <- rep(c(NA, sam), diffs)", "        low2 <- rep(c(NA, NA, sam[ - n]), diffs)", 
"        high1 <- rep(c(sam, NA), diffs)", "        high2 <- rep(c(sam[-1], NA, NA), diffs)", 
"        if(units == \"cfs\")", "                qfactor <- 35.31467", 
"        else if(units == \"cumecs\")", "                qfactor <- 1", 
"        else stop(\"flow units must be cfs or cumecs\")", "        q <- q/qfactor", 
"        # cubic meters per sec", "        x <- matrix(q[c(low2, low1, high1, high2)], ncol = 4)", 
"        y <- matrix(ssc[c(low2, low1, high1, high2)], ncol = 4)", 
"        if(opt == 1) {", "                #               Permit extrapolation down to (0,0)", 
"                x[is.na(x)] <- 0", "                y[is.na(y)] <- 0", 
"        }", "        newssc <- numeric(N)", "        for(i in index) {", 
"                #               Estimate ssc with local linear function of discharge", 
"                x1 <- x[i,  ]", "                y1 <- y[i,  ]", 
"                x1 <- x1[!is.na(x1)]", "                y1 <- y1[!is.na(y1)]", 
"                xy <- approx(x1, y1, xout = q[i])", "                newssc[i] <- xy$y", 
"        }", "        unfit <- is.na(newssc)", "        if(sum(unfit) != 0)", 
"                newssc[unfit] <- predict.loglog(q[sam], ssc[sam], q[unfit])", 
"        estflux <- sum(k * q * newssc)", "        detach(2)", 
"        if(long)", "                list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, chr = ", 
"                        mergepop$chr, predssc = newssc, meth = 2)", 
"        else list(yhat = estflux, n = n)", "}"))
localturbsrc <-
structure(function (stn, hy, sdate, stime, edate, etime, interval = 10, 
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
}, source = c("function(stn, hy, sdate, stime, edate, etime, interval = 10, opt = 1, long = T,", 
"        adj = T, units = \"cfs\")", "{", "        #  Extracts records from pop between given starting date/time", 
"        #  and ending date/time and merges with samples from the", 
"        #  correct station for the same time period.  Then estimates", 
"        #  the load using local pairwise functions of turbidity.", 
"        #  Uses approx() with the 2 points on each side (4 total).", 
"        #  approx() will not extrapolate beyond those 4 turbidities, so ", 
"        #  a simple regression is applied to all data when approx returns NA.", 
"        #  Extrapolates to (0,0) at lower end unless opt = 2, in which", 
"        #  case the simple regression is applied.", "        #  The reason 4 points are used instead of two is to reduce the chance", 
"        #  that predictions will be required outside the range of x.", 
"        #  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"        #  Result must be passed to ttsplot() using an \"src\" argument", 
"        # ", "        hy <- zfill(hy, 2)", "        k <- 0.06 * interval", 
"        S <- toupper(substring(stn, 1, 2))", "        pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"        samples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"        pop <- subtime(pop, sdate, stime, edate, etime)", "        samples <- subtime(samples, sdate, stime, edate, etime)", 
"        mergesam <- samples[, c(\"chr\", \"ssc\")]", "        mergepop <- merge(pop, mergesam, all.x = T)", 
"        mergepop <- mergepop[order(mergepop$chr),  ]", "        conf <- intersect(names(mergepop),objects(1))", 
"        if (length(conf) > 0) {", "                cat(\"The following objects conflict with object names in localturbsrc:\\n\")", 
"                cat(conf,\"\\n\")", "                cat(\"Please remove conflicting objects before proceeding.\\n\")", 
"                return(invisible())", "        }", "        attach(mergepop)", 
"        if(adj)", "                ssc <- exp(discoef[S, \"a\"]) * ssc^discoef[S, \"b\"]", 
"        N <- dim(mergepop)[1]", "        turbsav <- turb", "        index <- 1:N", 
"        sam <- index[ - which.na(ssc)]", "        diffs <- diff(c(0, sam, N))", 
"        n <- length(sam)", "        low1 <- rep(c(NA, sam), diffs)", 
"        low2 <- rep(c(NA, NA, sam[ - n]), diffs)", "        high1 <- rep(c(sam, NA), diffs)", 
"        high2 <- rep(c(sam[-1], NA, NA), diffs)", "        # cubic meters per sec", 
"        x <- matrix(turb[c(low2, low1, high1, high2)], ncol = 4)", 
"        y <- matrix(ssc[c(low2, low1, high1, high2)], ncol = 4)", 
"        if(opt == 1) {", "                #               Permit extrapolation down to (0,0)", 
"                x[is.na(x)] <- 0", "                y[is.na(y)] <- 0", 
"        }", "        newssc <- numeric(N)", "        for(i in index) {", 
"                #               Estimate ssc with local linear function of discharge", 
"                x1 <- x[i,  ]", "                y1 <- y[i,  ]", 
"                x1 <- x1[!is.na(x1)]", "                y1 <- y1[!is.na(y1)]", 
"                xy <- approx(x1, y1, xout = turb[i])", "                newssc[i] <- xy$y", 
"        }", "        unfit <- is.na(newssc)", "        if(sum(unfit) != 0)", 
"                newssc[unfit] <- predict.simple(turb[sam], ssc[sam], turb[", 
"                        unfit])", "        coef <- coef(lm(ssc[sam] ~ turb[sam]))", 
"        if(units == \"cfs\")", "                qfactor <- 35.31467", 
"        else if(units == \"cumecs\")", "                qfactor <- 1", 
"        else stop(\"flow units must be cfs or cumecs\")", "        estflux <- sum((k * newssc * q)/qfactor)", 
"        detach(2)", "        if(long)", "                list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, chr = ", 
"                        mergepop$chr, turb = turbsav, predssc = newssc, coef = ", 
"                        coef, log = \"\", meth = 1)", "        else list(yhat = estflux, n = n)", 
"}"))
logline <-
structure(function (x, y, ...) 
{
    fit <- lm(log(y) ~ log(x), na.action = na.omit)
    x0 <- range(x, na.rm = T)
    y0 <- exp(coef(fit)[1] + coef(fit)[2] * log(x0))
    lines(x0, y0, ...)
    fit
}, source = c("function(x, y, ...)", "{", "\t# Adds a regression line to a log-log plot created using log=\"xy\"", 
"\t# Regresion is computed from the logarithms of x and y", "\tfit <- lm(log(y) ~ log(x), na.action = na.omit)", 
"\tx0 <- range(x, na.rm = T)", "\ty0 <- exp(coef(fit)[1.] + coef(fit)[2.] * log(x0))", 
"\tlines(x0, y0, ...)", "\tfit", "}"))
logxy.duan <-
structure(function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
{
    k <- 0.06 * interval
    xsample <- log(xsam)
    ysample <- log(ysam)
    n <- length(ysample)
    N <- length(qpop)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysample
    yhat <- x1 %*% betahat
    resid <- ysample - yhat
    rsquare <- (cor(ysample, yhat))^2
    if (n == 2) 
        s2 <- 0
    else s2 <- sum(resid^2)/(n - 2)
    x2 <- cbind(rep(1, N), log(xpop))
    if (var) {
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Covariance matrix too large: not computed")
            var <- F
        }
    }
    if (var) {
        V.diag <- diag(V)
        tmp1 <- matrix(V.diag, ncol = N, nrow = N)
        tmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)
        newyhat <- drop(x2 %*% betahat)
        tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
        tmp4 <- tmp3 * tmp2 * t(tmp3)
        Vsam <- x1 %*% invxx %*% t(x1)
        Vsam.diag <- diag(Vsam)
        tmp5 <- (1 - Vsam.diag) * s2/2
        ebcf <- mean(exp(tmp5))
        tmp6 <- matrix(Vsam.diag, ncol = n, nrow = n)
        tmp7 <- exp((2 - 2 * Vsam - tmp6 - t(tmp6)) * s2/2)
        diag(tmp7) <- 0
        ebcf2 <- (sum(exp(4 * tmp5)) + sum(tmp7))/(n * n)
        EXY <- tmp4 * ebcf2
        murc <- exp(newyhat + (V.diag * s2/2))
        musmear <- murc * ebcf
        muumve <- exp(newyhat + s2/2)
        predssc <- exp(newyhat) * mean(exp(resid))
        flux <- k * sum(qpop * predssc)
        bias <- k * sum(qpop * (musmear - muumve))
        EX <- matrix(musmear, ncol = N, nrow = N)
        EY <- t(EX)
        COVXY <- EXY - EX * EY
        qmat <- k * matrix(qpop, ncol = N, nrow = N)
        COVXY2 <- qmat * COVXY * t(qmat)
        RMSE <- sqrt(sum(COVXY2) + bias^2)
    }
    else {
        newyhat <- drop(x2 %*% betahat)
        predssc <- exp(newyhat) * mean(exp(resid))
        flux <- k * sum(qpop * predssc)
        RMSE <- NA
    }
    list(predssc = predssc, est.load = flux, rsquare = rsquare, 
        betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
}, source = c("function(xsam, ysam, xpop, qpop, var=T, interval = 10)", 
"{", "\t#  Rev. 2.0  JL  12aug2005", "\t#  Computes the smearing estimate of load and its estimated RMSE for the ", 
"\t#  whole storm, based on a log-linear regression of ssc vs q from a", 
"\t#  sample (xsam,ysam).  The regression is used to estimate ssc for ", 
"\t#  all intervals including the sampled ones.", "\t#  Based on Gilroy et al (1990) formulas 11,15,17,19,24 for smearing method.", 
"\t#  The x variable may be flow, turbidity or anything else", 
"\t#  Units: q (m3/sec), ssc (mg/l), interval (min)", "\tk <- 0.06 * interval", 
"\txsample <- log(xsam)", "\tysample <- log(ysam)", "\tn <- length(ysample)", 
"\tN <- length(qpop)", "\tx1 <- cbind(rep(1, n), xsample)", "\txx <- t(x1) %*% x1", 
"\tinvxx <- solve(xx)", "\tbetahat <- invxx %*% t(x1) %*% ysample", 
"\tyhat <- x1 %*% betahat", "\tresid <- ysample - yhat", "\trsquare <- (cor(ysample, yhat))^2", 
"\tif(n == 2)", "\t\ts2 <- 0", "\telse s2 <- sum(resid^2)/(n - 2)", 
"\tx2 <- cbind(rep(1, N), log(xpop))", "\tif (var) { ", "\t\tV <- try(x2 %*% invxx %*% t(x2))", 
"\t    \tif (inherits(V,\"try-error\")) {", "\t\t\tprint(\"Covariance matrix too large: not computed\")", 
"               \t\tvar <- F", "        \t}", "\t}", "\tif (var) {", 
"\t\tV.diag <- diag(V)           ", "\t\ttmp1 <- matrix(V.diag, ncol = N, nrow = N)        ", 
"\t\ttmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)", "\t\tnewyhat <- drop(x2 %*% betahat)", 
"\t\ttmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)", "\t\ttmp4 <- tmp3 * tmp2 * t(tmp3)", 
"\t\tVsam <- x1 %*% invxx %*% t(x1)", "\t\tVsam.diag <- diag(Vsam)", 
"\t\ttmp5 <- (1-Vsam.diag)*s2/2", "\t\tebcf <- mean(exp(tmp5))", 
"\t\ttmp6 <- matrix(Vsam.diag, ncol = n, nrow = n)", "\t\ttmp7 <- exp((2-2*Vsam-tmp6-t(tmp6))*s2/2)", 
"\t\tdiag(tmp7) <- 0", "\t\tebcf2 <- (sum(exp(4*tmp5))+sum(tmp7))/(n*n)", 
"\t\tEXY <- tmp4 * ebcf2                     ", "\t\tmurc <- exp(newyhat + (V.diag * s2/2))", 
"\t\tmusmear <- murc * ebcf", "\t\tmuumve <- exp(newyhat + s2/2)", 
"\t\tpredssc <- exp(newyhat) * mean(exp(resid))", "\t\tflux <- k * sum(qpop * predssc)", 
"\t\tbias <- k * sum(qpop * (musmear - muumve))", "\t\tEX <- matrix(musmear, ncol = N, nrow = N)", 
"\t\tEY <- t(EX)", "\t\tCOVXY <- EXY - EX * EY", "\t\tqmat <- k * matrix(qpop, ncol = N, nrow = N)", 
"\t\tCOVXY2 <- qmat * COVXY * t(qmat)", "\t\tRMSE <- sqrt(sum(COVXY2) + bias^2)\t\t", 
"\t}", "\telse {", "\t\tnewyhat <- drop(x2 %*% betahat)", "\t\tpredssc <- exp(newyhat) * mean(exp(resid))", 
"\t\tflux <- k * sum(qpop * predssc)", "\t\tRMSE <- NA", "    \t}", 
"\tlist(predssc = predssc, est.load = flux, rsquare = rsquare,", 
"\t\t\tbetahat = betahat, s = sqrt(s2), est.rmse = RMSE)", "}"
))
logxy.mvue <-
structure(function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
{
    k <- 0.06 * interval
    xsample <- log(xsam)
    ysample <- log(ysam)
    n <- length(ysample)
    N <- length(qpop)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysample
    yhat <- x1 %*% betahat
    resid <- ysample - yhat
    rsquare <- (cor(ysample, yhat))^2
    if (n == 2) 
        s2 <- 0
    else s2 <- sum(resid^2)/(n - 2)
    x2 <- cbind(rep(1, N), log(xpop))
    if (var) {
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Covariance matrix too large: not computed")
            var <- F
        }
    }
    if (var) {
        V.diag <- diag(V)
        tmp1 <- matrix(V.diag, ncol = N, nrow = N)
        ttmp1 <- t(tmp1)
        tmp2 <- exp(((tmp1 + ttmp1 + 2 * V) * s2)/2)
        newyhat <- drop(x2 %*% betahat)
        tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
        tmp4 <- tmp3 * tmp2 * t(tmp3)
        m <- n - 2
        tmp5 <- exp(((2 - ttmp1 - tmp1) * s2)/2)
        tmp6 <- finney(m, ((1 - tmp1) * (1 - ttmp1) * (s2^2) * 
            (m + 1))/(2 * m^2))
        predssc <- exp(newyhat) * finney(m, ((1 - V.diag) * s2 * 
            (m + 1))/(2 * m))
        flux <- k * sum(qpop * predssc)
        EXY <- tmp4 * tmp5 * tmp6
        muumve <- exp(newyhat + s2/2)
        EX <- matrix(muumve, ncol = N, nrow = N)
        EY <- t(EX)
        COVXY <- EXY - EX * EY
        qmat <- k * matrix(qpop, ncol = N, nrow = N)
        COVXY2 <- qmat * COVXY * t(qmat)
        RMSE <- sqrt(sum(COVXY2))
    }
    else {
        V.diag <- drop((x2 %*% invxx[, 1]) + x2[, 2] * (x2 %*% 
            invxx[, 2]))
        newyhat <- drop(x2 %*% betahat)
        m <- n - 2
        predssc <- exp(newyhat) * finney(m, ((1 - V.diag) * s2 * 
            (m + 1))/(2 * m))
        flux <- k * sum(qpop * predssc)
        RMSE <- NA
    }
    list(predssc = predssc, est.load = flux, rsquare = rsquare, 
        betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
}, source = c("function(xsam, ysam, xpop, qpop, var=T, interval = 10)", 
"{", "\t#  Rev. 2.0  JL  12aug2005", "\t#  Computes the MVUE estimate of load and its estimated RMSE for the ", 
"\t#  whole storm, based on a log-linear regression of ssc vs x from a", 
"\t#  sample (xsam,ysam).  The regression is used to estimate ssc for ", 
"\t#  all intervals including the sampled ones.", "\t#  Based on Gilroy et al (1990) formulas 17,20,21,22 for MVUE method.", 
"\t#  The x variable may be flow, turbidity or anything else", 
"\t#  Units: q (m3/sec), ssc (mg/l), interval (min)", "\tk <- 0.06 * interval", 
"\txsample <- log(xsam)", "\tysample <- log(ysam)", "\tn <- length(ysample)", 
"\tN <- length(qpop)", "\tx1 <- cbind(rep(1, n), xsample)", "\txx <- t(x1) %*% x1", 
"\tinvxx <- solve(xx)", "\tbetahat <- invxx %*% t(x1) %*% ysample", 
"\tyhat <- x1 %*% betahat", "\tresid <- ysample - yhat", "\trsquare <- (cor(ysample, yhat))^2", 
"\tif(n == 2)", "\t\ts2 <- 0", "\telse s2 <- sum(resid^2)/(n - 2)", 
"\tx2 <- cbind(rep(1, N), log(xpop))", "\tif (var) { ", "\t\tV <- try(x2 %*% invxx %*% t(x2))", 
"\t\tif (inherits(V,\"try-error\")) {", "               \t\tprint(\"Covariance matrix too large: not computed\")", 
"                \tvar <- F", "           \t}", "\t}", "\tif (var) {", 
"\t\tV.diag <- diag(V)", "\t\ttmp1 <- matrix(V.diag, ncol = N, nrow = N)", 
"\t\tttmp1 <- t(tmp1)", " \t\ttmp2 <- exp(((tmp1 + ttmp1 + 2 * V) * s2)/2)", 
"\t\tnewyhat <- drop(x2 %*% betahat)", "\t\ttmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)", 
"\t\ttmp4 <- tmp3 * tmp2 * t(tmp3)", "\t\tm <- n - 2", "\t\ttmp5 <- exp(((2 - ttmp1 - tmp1) * s2)/2)", 
"\t\ttmp6 <- finney(m, ((1 - tmp1) * (1 - ttmp1) * (s2^2) * (m + 1))/(2 * m^2))", 
"\t\tpredssc <- exp(newyhat) * finney(m, ((1 - V.diag) * s2 * (m + 1))/(2 * m))", 
"\t\tflux <- k * sum(qpop * predssc)", "\t\tEXY <- tmp4 * tmp5 * tmp6", 
"\t\tmuumve <- exp(newyhat + s2/2)", "\t\tEX <- matrix(muumve, ncol = N, nrow = N)", 
"\t\tEY <- t(EX)", "\t\tCOVXY <- EXY - EX * EY", "\t\tqmat <- k * matrix(qpop, ncol = N, nrow = N)", 
"\t\tCOVXY2 <- qmat * COVXY * t(qmat)", "\t\tRMSE <- sqrt(sum(COVXY2))", 
"\t}", "\telse {", "\t\tV.diag <- drop((x2 %*% invxx[, 1]) + x2[, 2] * (x2 %*% invxx[, 2]))", 
"\t\tnewyhat <- drop(x2 %*% betahat)", "\t\tm <- n - 2", "\t\tpredssc <- exp(newyhat) * finney(m, ((1 - V.diag) * s2 * (m + 1))/(2 * m))", 
"\t\tflux <- k * sum(qpop * predssc)", "\t\tRMSE <- NA", "\t}", 
"\tlist(predssc = predssc, est.load = flux, rsquare = rsquare,", 
"\t\tbetahat = betahat, s = sqrt(s2), est.rmse = RMSE)", "}"))
logxy.qmle <-
structure(function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
{
    k <- 0.06 * interval
    xsample <- log(xsam)
    ysample <- log(ysam)
    n <- length(ysample)
    N <- length(qpop)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysample
    yhat <- x1 %*% betahat
    resid <- ysample - yhat
    rsquare <- (cor(ysample, yhat))^2
    if (n == 2) 
        s2 <- 0
    else s2 <- sum(resid^2)/(n - 2)
    x2 <- cbind(rep(1, N), log(xpop))
    if (var) {
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Covariance matrix too large: not computed")
            var <- F
        }
    }
    if (var) {
        V.diag <- diag(V)
        tmp1 <- matrix(V.diag, ncol = N, nrow = N)
        tmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)
        newyhat <- drop(x2 %*% betahat)
        tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
        tmp4 <- tmp3 * tmp2 * t(tmp3)
        m <- n - 2
        ebcf <- (1 - s2/m)^(-m/2)
        ebcf2 <- (1 - (2 * s2)/m)^(-m/2)
        EXY <- tmp4 * ebcf2
        murc <- exp(newyhat + (V.diag * s2)/2)
        muqmle <- murc * ebcf
        muumve <- exp(newyhat + s2/2)
        qmle <- k * qpop * muumve
        bias <- k * sum(qpop * (muqmle - muumve))
        EX <- matrix(muqmle, ncol = N, nrow = N)
        EY <- t(EX)
        COVXY <- EXY - EX * EY
        qmat <- k * matrix(qpop, ncol = N, nrow = N)
        COVXY2 <- qmat * COVXY * t(qmat)
        RMSE <- sqrt(sum(COVXY2) + bias^2)
    }
    else {
        newyhat <- drop(x2 %*% betahat)
        muumve <- exp(newyhat + s2/2)
        qmle <- k * qpop * muumve
        RMSE <- NA
    }
    list(predssc = muumve, est.load = sum(qmle), rsquare = rsquare, 
        betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
}, source = c("function(xsam, ysam, xpop, qpop, var=T, interval = 10)", 
"{", "\t#  Rev. 2.0  JL  12aug2005", "\t#  Computes the QMLE estimate of load and its estimated RMSE for the ", 
"\t#  whole storm, based on a log-linear regression of ssc vs x from a", 
"\t#  sample (xsam,ysam).  The regression is used to estimate ssc for ", 
"\t#  all intervals including the sampled ones, i.e. for xpop.", 
"\t#  Based on Gilroy et al (1990) formulas 17,18,21,22 for QMLE method.", 
"\t#  The x variable may be flow, turbidity or anything else", 
"\t#  Units: q (m3/sec), ssc (mg/l), interval (min)", "\tk <- 0.06 * interval", 
"\txsample <- log(xsam)", "\tysample <- log(ysam)", "\tn <- length(ysample)", 
"\tN <- length(qpop)", "\tx1 <- cbind(rep(1, n), xsample)", "\txx <- t(x1) %*% x1", 
"\tinvxx <- solve(xx)", "\tbetahat <- invxx %*% t(x1) %*% ysample", 
"\tyhat <- x1 %*% betahat", "\tresid <- ysample - yhat", "\trsquare <- (cor(ysample, yhat))^2", 
"\tif(n == 2)", "\t\ts2 <- 0", "\telse s2 <- sum(resid^2)/(n - 2)", 
"\tx2 <- cbind(rep(1, N), log(xpop))", "\tif (var) { ", "\t\tV <- try(x2 %*% invxx %*% t(x2))", 
"\t\tif (inherits(V,\"try-error\")) {", "\t\t\tprint(\"Covariance matrix too large: not computed\")", 
"\t\t\tvar <- F", "\t\t}", "\t}", "\tif (var) {", "        \tV.diag <- diag(V)", 
"\t\ttmp1 <- matrix(V.diag, ncol = N, nrow = N)", "\t\ttmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)", 
"\t\tnewyhat <- drop(x2 %*% betahat)", "\t\ttmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)", 
"\t\ttmp4 <- tmp3 * tmp2 * t(tmp3)", "\t\tm <- n - 2", "\t\tebcf <- (1 - s2/m)^( - m/2)", 
"\t\tebcf2 <- (1 - (2 * s2)/m)^( - m/2)", "\t\tEXY <- tmp4 * ebcf2", 
"\t\tmurc <- exp(newyhat + (V.diag * s2)/2)", "\t\tmuqmle <- murc * ebcf", 
"\t\tmuumve <- exp(newyhat + s2/2)", "\t\tqmle <- k * qpop * muumve", 
"\t\tbias <- k * sum(qpop * (muqmle - muumve))", "\t\tEX <- matrix(muqmle, ncol = N, nrow = N)", 
"\t\tEY <- t(EX)", "\t\tCOVXY <- EXY - EX * EY", "\t\tqmat <- k * matrix(qpop, ncol = N, nrow = N)", 
"\t\tCOVXY2 <- qmat * COVXY * t(qmat)", "\t\tRMSE <- sqrt(sum(COVXY2) + bias^2)", 
"\t}", "\telse {", "\t\tnewyhat <- drop(x2 %*% betahat)", "\t\tmuumve <- exp(newyhat + s2/2)", 
"\t\tqmle <- k * qpop * muumve", "\t\tRMSE <- NA", "\t}", "\tlist(predssc = muumve, est.load = sum(qmle), rsquare = rsquare,", 
"\t\tbetahat = betahat, s = sqrt(s2), est.rmse = RMSE)", "}"))
make.chr <-
structure(function (year, mo, dy, time) 
{
    chron(paste(mo, dy, year, sep = "/")) + mt2msm(time)/1440
}, source = c("function(year, mo, dy, time)", "{", "\t# Makes a chron object from a year, month, day, and military time", 
"\tchron(paste(mo, dy, year, sep = \"/\")) + mt2msm(time)/1440", 
"}"))
make.chron <-
structure(function (date, time, out = "day-mon-year", origin) 
{
    if (missing(origin)) 
        origin <- options()$chron.origin
    ymd2date(date, out = out, origin = origin) + mt2msm(time)/1440
}, source = c("function(date, time, out = \"day-mon-year\", origin)", 
"{", "\t# Makes a chron object from a date and military time", 
"\t# Date and time can be character or numeric vectors", "\t# out is the format for the date only", 
"      if (missing(origin)) origin <- options()$chron.origin", 
"\tymd2date(date, out = out, origin = origin) + mt2msm(time)/1440.", 
"}"))
merge.flo <-
structure(function (stn, hy, all.lab = F, all.flo = F, na.rm = T) 
{
    lab <- eval(as.name(paste(stn, zfill(hy, 2), ".lab", sep = "")))
    flo <- eval(as.name(paste(stn, zfill(hy, 2), ".flo", sep = "")))
    flo <- flo[flo$bottle > 0, ]
    newdata <- suppressWarnings(merge(lab, flo, by = c("dump", 
        "bottle"), all.x = all.lab, all.y = all.flo))
    codes <- zfill(newdata$labcodes, 2)
    newdata$labcode1 <- substring(codes, 1, 1)
    newdata$labcode2 <- substring(codes, 2, 2)
    newdata <- newdata[, c("chr", "dump", "bottle", "ssc", "turb", 
        "turbcode", "q", "labcode1", "labcode2")]
    newdata <- newdata[order(newdata$dump, newdata$bottle), ]
    row.names(newdata) <- 1:dim(newdata)[1]
    attr(newdata, "stn") <- stn
    if (na.rm) 
        newdata[!is.na(newdata$ssc), ]
    else newdata
}, source = c("function(stn,hy,all.lab=F,all.flo=F,na.rm=T) {", 
"# Revised Sep 2, 2012 added \"stn\" attribute to output data", 
"# Revised Sep 28, 2006 for RSL only: labcodes are now read into file", 
"#     records with SSC values of NA are removed if na.rm=T", 
"# Revised August 5, 2004:  row numbers made sequential", "# Merges the .flo and .lab data frames by dump and bottle number", 
"# Keeps unmatched bottles in the .lab data frame if all.lab=T", 
"# Keeps unmatched bottles in the .flo data frame if all.flo=T ", 
"# Data frame names must be <stn><hy>.flo and <stn><hy>.lab ", 
"   lab <- eval(as.name(paste(stn,zfill(hy,2),\".lab\",sep=\"\")))", 
"   flo <- eval(as.name(paste(stn,zfill(hy,2),\".flo\",sep=\"\")))", 
"   flo <- flo[flo$bottle > 0, ]", "   newdata <- suppressWarnings(", 
"       merge(lab,flo,by=c(\"dump\",\"bottle\"),all.x=all.lab,all.y=all.flo)", 
"   )", "   codes <- zfill(newdata$labcodes,2)", "   newdata$labcode1 <- substring(codes,1,1)", 
"   newdata$labcode2 <- substring(codes,2,2)", "   newdata <- newdata[,c(\"chr\",\"dump\",\"bottle\",\"ssc\",\"turb\",\"turbcode\",\"q\",\"labcode1\",\"labcode2\")]", 
"   newdata <- newdata[order(newdata$dump,newdata$bottle), ]", 
"   row.names(newdata) <- 1:dim(newdata)[1]", "   attr(newdata,\"stn\") <- stn   # Preserves station identity even if renamed later", 
"   if (na.rm)", "      newdata[!is.na(newdata$ssc), ]", "   else", 
"      newdata", "}"))
merge.flo.sand <-
structure(function (stn, hy, all.lab = F, all.flo = F, na.rm = T) 
{
    lab <- eval(as.name(paste(stn, zfill(hy, 2), ".lab", sep = "")))
    flo <- eval(as.name(paste(stn, zfill(hy, 2), ".flo", sep = "")))
    flo <- flo[flo$bottle > 0, ]
    newdata <- merge(lab, flo, by = c("dump", "bottle"), all.x = all.lab, 
        all.y = all.flo)
    codes <- zfill(newdata$labcodes, 2)
    newdata$labcode1 <- substring(codes, 1, 1)
    newdata$labcode2 <- substring(codes, 2, 2)
    newdata <- newdata[, c("chr", "dump", "bottle", "sand", "totssc", 
        "turb", "turbcode", "q", "labcode1", "labcode2")]
    newdata <- newdata[order(newdata$dump, newdata$bottle), ]
    row.names(newdata) <- 1:dim(newdata)[1]
    if (na.rm) 
        newdata[!is.na(newdata$totssc) & !is.na(newdata$sand), 
            ]
    else newdata
}, source = c("function(stn,hy,all.lab=F,all.flo=F,na.rm=T) {", 
"# Created Mar 07, 2010 from merge.flo to handle sand fraction as well as ssc", 
"# Revised Sep 28, 2006 for RSL only: labcodes are now read into file", 
"#     records with SSC values of NA are removed if na.rm=T", 
"# Revised August 5, 2004:  row numbers made sequential", "# Merges the .flo and .lab data frames by dump and bottle number", 
"# Keeps unmatched bottles in the .lab data frame if all.lab=T", 
"# Keeps unmatched bottles in the .flo data frame if all.flo=T ", 
"# Data frame names must be <stn><hy>.flo and <stn><hy>.lab ", 
"   lab <- eval(as.name(paste(stn,zfill(hy,2),\".lab\",sep=\"\")))", 
"   flo <- eval(as.name(paste(stn,zfill(hy,2),\".flo\",sep=\"\")))", 
"   flo <- flo[flo$bottle > 0, ]", "   newdata <- merge(lab,flo,by=c(\"dump\",\"bottle\"),all.x=all.lab,all.y=all.flo)", 
"   codes <- zfill(newdata$labcodes,2)", "   newdata$labcode1 <- substring(codes,1,1)", 
"   newdata$labcode2 <- substring(codes,2,2)", "   newdata <- newdata[,c(\"chr\",\"dump\",\"bottle\",\"sand\",\"totssc\",\"turb\",\"turbcode\",\"q\",\"labcode1\",\"labcode2\")]", 
"   newdata <- newdata[order(newdata$dump,newdata$bottle), ]", 
"   row.names(newdata) <- 1:dim(newdata)[1]", "   if (na.rm)", 
"      newdata[!is.na(newdata$totssc) & !is.na(newdata$sand), ]", 
"   else", "      newdata", "}"))
merge.flo.vr <-
structure(function (stn, hy, all.lab = F) 
{
    lab <- eval(as.name(paste(stn, zfill(hy, 2), ".lab", sep = "")))
    flo <- eval(as.name(paste(stn, zfill(hy, 2), ".flo", sep = "")))
    lab$q <- approx(flo$chr, flo$q, lab$chr)$y
    lab
}, source = c("function(stn,hy,all.lab=F) {", "# Created Aug. 30, 2004 for vineyard runoff plots: ", 
"# Adds discharge to the .lab data by interpolating in the .flo data", 
"# Keeps unmatched bottles in the .lab data frame if all.lab=T", 
"# all.flo is not an option since the flo file has no clue about samples", 
"# Data frame names must be <stn><hy>.flo and <stn><hy>.lab ", 
"   lab <- eval(as.name(paste(stn,zfill(hy,2),\".lab\",sep=\"\")))", 
"   flo <- eval(as.name(paste(stn,zfill(hy,2),\".flo\",sep=\"\")))", 
"   lab$q <- approx(flo$chr, flo$q, lab$chr)$y", "   lab", "}"
))
minortics <-
structure(function (side) 
{
    if (side%%2 == 0) 
        axp <- par()$yaxp
    else axp <- par()$xaxp
    axmin <- axp[1]
    axmax <- axp[2]
    nints <- axp[3]
    unit <- (axmax - axmin)/nints
    axmax <- axmax + unit
    axmin <- axmin - unit
    nints <- nints + 2
    type <- round(unit/10^floor(log10(unit) + .Machine$double.eps))
    tinytics <- seq(axmin, axmax, length = 10 * nints + 1)
    if (type == 1 || type == 2) 
        midtics <- seq(axmin, axmax, length = 2 * nints + 1)
    else if (type == 5) 
        midtics <- seq(axmin, axmax, length = 5 * nints + 1)
    else return("Unexpected tick interval.  No minor ticks plotted.")
    axis(side, at = midtics, label = F, tck = -0.01)
    axis(side, at = tinytics, label = F, tck = -0.005)
}, source = c("function(side) ", "{", "# Adds minor tick marks to an axis", 
"# side = 1,2,3, or 4 for bottom, left, top, and right axes", 
"    if (side%%2 == 0) ", "        axp <- par()$yaxp", "    else axp <- par()$xaxp", 
"    axmin <- axp[1]", "    axmax <- axp[2]", "    nints <- axp[3]", 
"    unit <- (axmax - axmin)/nints", "    axmax <- axmax + unit", 
"    axmin <- axmin - unit", "    nints <- nints + 2", "    type <- round(unit/10^floor(log10(unit) + .Machine$double.eps))", 
"    tinytics <- seq(axmin, axmax, length = 10 * nints + 1)", 
"    if (type == 1 || type == 2) ", "        midtics <- seq(axmin, axmax, length = 2 * nints + 1)", 
"    else if (type == 5) ", "        midtics <- seq(axmin, axmax, length = 5 * nints + 1)", 
"    else return(\"Unexpected tick interval.  No minor ticks plotted.\")", 
"    axis(side, at = midtics, label = F, tck = -0.01)", "    axis(side, at = tinytics, label = F, tck = -0.005)", 
"}"))
mismatches <-
structure(function (stn, hy) 
{
    tmp1 <- merge.flo(stn, hy, all.lab = T, na.rm = F)
    tmp2 <- merge.flo(stn, hy, all.flo = T, na.rm = F)
    tmp3 <- merge.flo(stn, hy, na.rm = F)
    row.names(tmp1) <- paste(tmp1$dump, tmp1$bottle, sep = ":")
    row.names(tmp2) <- paste(tmp2$dump, tmp2$bottle, sep = ":")
    row.names(tmp3) <- paste(tmp3$dump, tmp3$bottle, sep = ":")
    labmatches <- row.names(tmp1) %in% row.names(tmp3)
    flomatches <- row.names(tmp2) %in% row.names(tmp3)
    lab <- tmp1[!labmatches, ]
    if (dim(lab)[1] == 0) 
        lab <- NULL
    flo <- tmp2[!flomatches, ]
    if (dim(flo)[1] == 0) 
        flo <- NULL
    list(unmatched.lab = lab, unmatched.flo = flo)
}, source = c("function(stn,hy) {", "# identifies unmatched bottles in both flo and lab data", 
"# Calls RSL version of merge.flo (na.rm arg does not exist in public version)", 
"tmp1 <- merge.flo(stn,hy,all.lab=T,na.rm=F)", "tmp2 <- merge.flo(stn,hy,all.flo=T,na.rm=F)", 
"tmp3 <- merge.flo(stn,hy,na.rm=F)", "row.names(tmp1) <- paste(tmp1$dump,tmp1$bottle,sep=\":\")", 
"row.names(tmp2) <- paste(tmp2$dump,tmp2$bottle,sep=\":\")", 
"row.names(tmp3) <- paste(tmp3$dump,tmp3$bottle,sep=\":\")", 
"labmatches <- row.names(tmp1) %in% row.names(tmp3)", "flomatches <- row.names(tmp2) %in% row.names(tmp3)", 
"lab <- tmp1[!labmatches, ]", "if (dim(lab)[1] == 0) lab <- NULL", 
"flo <- tmp2[!flomatches, ]", "if (dim(flo)[1] == 0) flo <- NULL", 
"list(unmatched.lab=lab, unmatched.flo=flo)", "}"))
model.objects <-
structure(function () 
{
    objnames <- objects(envir = .GlobalEnv)
    objtypes <- sapply(objnames, function(x) data.class(eval(as.name(x))))
    lists <- objnames[objtypes == "list"]
    firstcomp <- sapply(lists, function(x) names(eval(as.name(x)))[1])
    lists[firstcomp == "yhat"]
}, source = c("function() {", "    # Identifies model objects by name of first component (=\"yhat\")", 
"    objnames <- objects(envir=.GlobalEnv)", "    objtypes <- sapply(objnames,function(x) data.class(eval(as.name(x))))", 
"    lists <- objnames[objtypes == \"list\"]", "    firstcomp <- sapply(lists, function(x) names(eval(as.name(x)))[1])", 
"    lists[firstcomp==\"yhat\"]", "}"))
modelssc.gui <-
structure(function () 
{
    model <- function(panel) {
        with(panel, {
            stn <- tolower(pars["stn"])
            hy4 <- pars["hy4"]
            firstdt1 <- pars["firstdt1"]
            lastdt1 <- pars["lastdt1"]
            firstdt2 <- pars["firstdt2"]
            lastdt2 <- pars["lastdt2"]
            dumpexpr <- as.vector(pars["dumpexpr"])
            botexpr <- as.vector(pars["botexpr"])
            result <- pars["result"]
            interstorm <- checkvars["interstorm"]
            adj <- checkvars["adj"]
            var <- checkvars["var"]
            exclude <- checkvars["exclude"]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- as.vector(substring(hy4, 3, 4))
            setTTSenv(c("STN", "HY", "SDATE", "EDATE", "DUMPS", 
                "BOTTLES", "ADJUST", "INTERSTORM"), c(stn, hy4, 
                firstdt1, lastdt1, dumpexpr, botexpr, adj, interstorm))
            span <- as.numeric(loesspars["span"])
            degree <- as.numeric(loesspars["degree"])
            sdate1 <- paste(substring(firstdt1, 7, 8), substring(firstdt1, 
                1, 2), substring(firstdt1, 4, 5), sep = "")
            stime1 <- paste(substring(firstdt1, 10, 11), substring(firstdt1, 
                13, 14), sep = "")
            edate1 <- paste(substring(lastdt1, 7, 8), substring(lastdt1, 
                1, 2), substring(lastdt1, 4, 5), sep = "")
            etime1 <- paste(substring(lastdt1, 10, 11), substring(lastdt1, 
                13, 14), sep = "")
            arglist <- list(type = type, exclude = exclude, adj = adj, 
                var = var)
            sta <- check.interstorm(stn, hy2, interstorm, surrogate, 
                checkflo = F)
            if (type == "loess") 
                arglist <- c(arglist, degree = degree, span = span)
            if (type == "logxy") 
                arglist <- c(arglist, bias = bias)
            if (subsetby == "Alternate period") {
                sdate2 <- paste(substring(firstdt2, 7, 8), substring(firstdt2, 
                  1, 2), substring(firstdt2, 4, 5), sep = "")
                stime2 <- paste(substring(firstdt2, 10, 11), 
                  substring(firstdt2, 13, 14), sep = "")
                edate2 <- paste(substring(lastdt2, 7, 8), substring(lastdt2, 
                  1, 2), substring(lastdt2, 4, 5), sep = "")
                etime2 <- paste(substring(lastdt2, 10, 11), substring(lastdt2, 
                  13, 14), sep = "")
                arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                  etime1, sdate2, stime2, edate2, etime2, arglist)
            }
            else if (subsetby == "Specific dumps/bottles") {
                if (botexpr == "") {
                  arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                    etime1, dumps = dumpexpr, arglist)
                }
                else {
                  arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                    etime1, dumps = dumpexpr, bottles = botexpr, 
                    arglist)
                }
            }
            else {
                arglist <- c(sta, hy2, sdate1, stime1, edate1, 
                  etime1, arglist)
            }
            if (surrogate == "turbidity") 
                funcname <- "turbsrc"
            else funcname <- "flowsrc"
            modelfunc <- get(funcname, envir = .GlobalEnv)
            res <- do.call("modelfunc", arglist)
            saveCommand(stn, hy2, funcname, arglist, result, 
                checkvars["savecmd"])
            if (result != "") {
                assign(result, res, envir = .GlobalEnv)
                cat("Result saved in workspace as", result, "\n")
            }
            else cat("To save results, enter name of output object and press OK\n")
        })
        panel
    }
    nothing <- function(panel) panel
    env <- getTTSenv()
    init.stn <- env["STN", ]
    init.hy <- env["HY", ]
    init.sdate <- env["SDATE", ]
    init.edate <- env["EDATE", ]
    init.sdump <- env["SDUMP", ]
    init.edump <- env["EDUMP", ]
    dumpexpr <- env["DUMPS", ]
    botexpr <- env["BOTTLES", ]
    init.adjust <- env["ADJUST", ]
    init.interstorm <- env["INTERSTORM", ]
    if (is.na(init.adjust)) 
        init.adjust <- F
    if (is.na(init.interstorm)) 
        init.interstorm <- F
    pars <- c(init.stn, init.hy, init.sdate, init.edate, init.sdate, 
        init.edate, dumpexpr, botexpr, "")
    loesspars <- c(1, 1)
    panel <- rp.control("Regression model for SSC")
    rp.radiogroup(panel, surrogate, c("turbidity", "flow"), title = "Sediment surrogate", 
        action = nothing)
    my.textentry(panel, pars, labels = c("Station", "Water year", 
        "Starting date/time (m/d/y h:m)", "Ending date/time (m/d/y h:m)", 
        "Alternate start for samples (m/d/y h:m)", "Alternate end for samples (m/d/y h:m)", 
        "Dumps (R expression)", "Corresponding bottles (R expression)", 
        "Output object name"), names = c("stn", "hy4", "firstdt1", 
        "lastdt1", "firstdt2", "lastdt2", "dumpexpr", "botexpr", 
        "result"), title = "Enter values", initval = pars)
    rp.radiogroup(panel, subsetby, c("Estimation period", "Alternate period", 
        "Specific dumps/bottles"), title = "Method of selecting samples", 
        action = nothing, initval = "Estimation period")
    rp.radiogroup(panel, type, c("linear", "logx", "logxy", "power", 
        "loess", "pairs"), title = "Model to fit", initval = "linear", 
        action = nothing)
    rp.radiogroup(panel, bias, c("mvue", "duan", "qmle"), initval = "mvue", 
        title = "Bias correction method (for logxy only)", action = nothing)
    my.textentry(panel, loesspars, labels = c("span", "degree"), 
        names = c("span", "degree"), title = "Loess parameters", 
        initval = loesspars)
    rp.checkbox(panel, checkvars, initval = c(T, init.interstorm, 
        init.adjust, T, T), title = "Other options", labels = c("Exclude previously flagged points (those with exclude=T)", 
        "Interstorm data only", "Adjust SSC to depth-integrated equivalent", 
        "Calculate variance", "Save command to file"), names = c("exclude", 
        "interstorm", "adj", "var", "savecmd"), action = nothing)
    rp.button(panel, action = model, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run turbsrc() and flowsrc() ", 
"  model <- function(panel) {", "    with(panel, {", "      stn <- tolower(pars[\"stn\"])", 
"      hy4 <- pars[\"hy4\"]", "      firstdt1 <- pars[\"firstdt1\"]", 
"      lastdt1 <- pars[\"lastdt1\"]", "      firstdt2 <- pars[\"firstdt2\"]", 
"      lastdt2 <- pars[\"lastdt2\"]", "      dumpexpr <- as.vector(pars[\"dumpexpr\"])  # necessary when arglist is assembled later", 
"      botexpr <- as.vector(pars[\"botexpr\"])    # necessary when arglist is assembled later", 
"      result <- pars[\"result\"]", "      interstorm <- checkvars[\"interstorm\"]", 
"      adj <- checkvars[\"adj\"]", "      var <- checkvars[\"var\"]", 
"      exclude <- checkvars[\"exclude\"]", "", "      if (nchar(hy4) < 3) {", 
"        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- as.vector(substring(hy4,3,4))  # as.vector required to get rid of the name \"hy4\"", 
"      # Save current values in TTS Environment", "      setTTSenv(c(\"STN\",\"HY\",\"SDATE\",\"EDATE\",\"DUMPS\",\"BOTTLES\",\"ADJUST\",\"INTERSTORM\"),", 
"                c(stn,hy4,firstdt1,lastdt1,dumpexpr,botexpr,adj,interstorm))", 
"      ", "      span <- as.numeric(loesspars[\"span\"])", "      degree <- as.numeric(loesspars[\"degree\"]) ", 
"      sdate1 <- paste(substring(firstdt1,7,8),substring(firstdt1,1,2),substring(firstdt1,4,5),sep=\"\")", 
"      stime1 <- paste(substring(firstdt1,10,11),substring(firstdt1,13,14),sep=\"\")", 
"      edate1 <- paste(substring(lastdt1,7,8),substring(lastdt1,1,2),substring(lastdt1,4,5),sep=\"\")", 
"      etime1 <- paste(substring(lastdt1,10,11),substring(lastdt1,13,14),sep=\"\")", 
"      arglist <- list(type=type,exclude=exclude,adj=adj,var=var)", 
"      sta <- check.interstorm(stn, hy2, interstorm, surrogate,checkflo=F)", 
"      if (type==\"loess\") arglist <- c(arglist,degree=degree,span=span)", 
"      if (type==\"logxy\") arglist <- c(arglist, bias=bias)", 
"      if (subsetby == \"Alternate period\") {", "        sdate2 <- paste(substring(firstdt2,7,8),substring(firstdt2,1,2),substring(firstdt2,4,5),sep=\"\")", 
"        stime2 <- paste(substring(firstdt2,10,11),substring(firstdt2,13,14),sep=\"\")", 
"        edate2 <- paste(substring(lastdt2,7,8),substring(lastdt2,1,2),substring(lastdt2,4,5),sep=\"\")", 
"        etime2 <- paste(substring(lastdt2,10,11),substring(lastdt2,13,14),sep=\"\")", 
"        arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,sdate2,stime2,edate2,etime2,arglist)", 
"      }", "      else if(subsetby==\"Specific dumps/bottles\") {", 
"        if (botexpr == \"\") {", "          arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,dumps=dumpexpr,arglist)", 
"        }", "        else {", "          arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,dumps=dumpexpr,bottles=botexpr,arglist)", 
"        }", "      }", "      else {", "        arglist <- c(sta,hy2,sdate1,stime1,edate1,etime1,arglist)", 
"      }", "      # print(arglist)", "      ", "      if (surrogate == \"turbidity\")", 
"        funcname <- \"turbsrc\"", "      else ", "        funcname <- \"flowsrc\"", 
"      modelfunc <- get(funcname,envir=.GlobalEnv)", "      res <- do.call(\"modelfunc\",arglist)", 
"      saveCommand(stn,hy2,funcname,arglist,result,checkvars[\"savecmd\"])", 
"      if (result!=\"\") {", "          assign(result,res,envir=.GlobalEnv)", 
"          cat(\"Result saved in workspace as\",result,\"\\n\")", 
"      }", "      else ", "        cat(\"To save results, enter name of output object and press OK\\n\")", 
"    })", "    panel", "  }", "  nothing <- function(panel) panel", 
"  env <- getTTSenv()", "  init.stn <- env[\"STN\",]", "  init.hy <- env[\"HY\",]", 
"  init.sdate <- env[\"SDATE\",]", "  init.edate <- env[\"EDATE\",]", 
"  init.sdump <- env[\"SDUMP\",]", "  init.edump <- env[\"EDUMP\",] ", 
"  dumpexpr <- env[\"DUMPS\",]", "  botexpr <- env[\"BOTTLES\",]", 
"  init.adjust <- env[\"ADJUST\",]", "  init.interstorm <- env[\"INTERSTORM\",]", 
"  if (is.na(init.adjust))", "    init.adjust <- F", "  if (is.na(init.interstorm))", 
"    init.interstorm <- F", "", "  pars <- c(init.stn,init.hy,init.sdate,init.edate,init.sdate,init.edate,dumpexpr,botexpr,\"\")", 
"  loesspars <- c(1,1)", "  panel <- rp.control(\"Regression model for SSC\")", 
"  rp.radiogroup(panel, surrogate, c(\"turbidity\",\"flow\"),title=\"Sediment surrogate\",action=nothing)", 
"  my.textentry(panel,pars,labels=c(\"Station\",\"Water year\",\"Starting date/time (m/d/y h:m)\",\"Ending date/time (m/d/y h:m)\",", 
"                                   \"Alternate start for samples (m/d/y h:m)\",\"Alternate end for samples (m/d/y h:m)\",", 
"                                   \"Dumps (R expression)\",\"Corresponding bottles (R expression)\",\"Output object name\"),", 
"               names=c(\"stn\",\"hy4\",\"firstdt1\",\"lastdt1\",\"firstdt2\",\"lastdt2\",\"dumpexpr\",\"botexpr\",\"result\"),", 
"               title=\"Enter values\",initval=pars)", "  rp.radiogroup(panel, subsetby, c(\"Estimation period\",\"Alternate period\",\"Specific dumps/bottles\"),", 
"                title=\"Method of selecting samples\",action=nothing,initval=\"Estimation period\")", 
"  rp.radiogroup(panel, type, c(\"linear\", \"logx\", \"logxy\", \"power\", \"loess\", \"pairs\"), title=\"Model to fit\", initval=\"linear\",action=nothing)", 
"  rp.radiogroup(panel, bias,c(\"mvue\",\"duan\",\"qmle\"),initval=\"mvue\",title=\"Bias correction method (for logxy only)\",action=nothing)", 
"  my.textentry(panel,loesspars,labels=c(\"span\",\"degree\"),names=c(\"span\",\"degree\"),title=\"Loess parameters\",initval=loesspars)", 
"  rp.checkbox(panel, checkvars, initval = c(T,init.interstorm,init.adjust,T,T), title=\"Other options\", ", 
"              labels=c(\"Exclude previously flagged points (those with exclude=T)\",\"Interstorm data only\",", 
"                       \"Adjust SSC to depth-integrated equivalent\",\"Calculate variance\",\"Save command to file\"),", 
"              names=c(\"exclude\",\"interstorm\",\"adj\",\"var\",\"savecmd\"),action=nothing)", 
"  rp.button(panel, action = model, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
msm2mt <-
structure(function (msm) 
{
    hr <- msm%/%60
    mn <- msm%%60
    round(100 * hr + mn)
}, source = c("function(msm)", "{", "# Converts minutes since midnight to military time", 
"\thr <- msm %/% 60.", "\tmn <- msm %% 60.", "\tround(100. * hr + mn)", 
"}"))
mt2msm <-
structure(function (time) 
{
    if (data.class(time) == "character") 
        time <- as.numeric(time)
    min <- time%%100
    hour <- time%/%100
    60 * hour + min
}, source = c("function (time) ", "{", "# Converts military time to minutes since midnight", 
"  if(data.class(time)==\"character\")", "    time<-as.numeric(time)", 
"  min<-time%%100.", "  hour<-time%/%100.", "  60.*hour+min", 
"}"))
mvue <-
structure(function (mydata, sam = which(!is.na(mydata$ssc)), 
    x = "turb", interval = 10) 
{
    conf <- intersect(names(mydata), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in mvue:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    if (!missing(mydata)) 
        attach(mydata)
    k <- 0.06 * interval
    lnx <- log(get(x))
    y <- log(ssc)
    ysample <- y[sam]
    n <- length(ysample)
    N <- length(y)
    q <- q/35.3147
    x1 <- cbind(rep(1, n), lnx[sam])
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysample
    yhat <- x1 %*% betahat
    resid <- ysample - yhat
    rsquare <- (cor(ysample, yhat))^2
    s2 <- sum(resid^2)/(n - 2)
    x2 <- cbind(rep(1, N), lnx)
    V <- x2 %*% invxx %*% t(x2)
    tmp1 <- matrix(diag(V), ncol = N, nrow = N)
    ttmp1 <- t(tmp1)
    tmp2 <- exp(((tmp1 + ttmp1 + 2 * V) * s2)/2)
    newyhat <- drop(x2 %*% betahat)
    tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
    tmp4 <- tmp3 * tmp2 * t(tmp3)
    m <- n - 2
    tmp5 <- exp(((2 - ttmp1 - tmp1) * s2)/2)
    tmp6 <- finney(m, ((1 - tmp1) * (1 - ttmp1) * (s2^2) * (m + 
        1))/(2 * m^2))
    mvue <- k * q * exp(newyhat) * finney(m, ((1 - diag(V)) * 
        s2 * (m + 1))/(2 * m))
    EXY <- tmp4 * tmp5 * tmp6
    mumvue <- exp(newyhat + s2/2)
    EX <- matrix(mumvue, ncol = N, nrow = N)
    EY <- t(EX)
    COVXY <- EXY - EX * EY
    qmat <- k * matrix(q, ncol = N, nrow = N)
    COVXY2 <- qmat * COVXY * t(qmat)
    MSE <- sum(COVXY2)
    if (!missing(mydata)) 
        detach(2)
    list(est.load = sum(mvue), betahat = betahat, rsquare = rsquare, 
        est.rmse = sqrt(MSE))
}, source = c("function(mydata, sam = which(!is.na(mydata$ssc)), x = \"turb\", interval = 10)", 
"{", "\t#  Rev. 1.1  JL  2002-May-24, introduced x argument", 
"\t#  Rev. 1.0  JL  1995-Aug-18", "\t#  MVUE load and variance estimator.  This is an alternate form works ", 
"\t#    that works on a data frame; it is not called by turbsrc & flowsrc", 
"\t#  Computes the MVUE load (in kg) and its estimated RMSE for whole", 
"\t#  storm, based on a log-linear regression of ssc vs turbidity or", 
"\t#  discharge from a sample. The regression is used to estimate ssc ", 
"\t#  for all intervals including the sampled ones. ", "\t#  ", 
"\t#  Based on Gilroy et al (1990) formulas 17,20,21,22 for MVUE method.", 
"\t#  The method is altered so that the regression predicts ssc instead ", 
"\t#  of flux.  For sediment rating curves (x=\"q\"), the estimates of ", 
"\t#  load and its variance are unchanged.", "\t#", "\t#  Arguments:", 
"\t#    mydata = data frame containing q (cfs), turbidity, and ssc (mg/l)", 
"\t#\tat fixed time intervals (ssc is NA for records not sampled)", 
"\t#\tturbidity is not required if x=\"q\"", "\t#    sam = vector of indices to sampled records in mydata", 
"\t#\t(defaults to record numbers in which ssc is not NA)", "\t#    x = name of covariate: \"turb\" (default) or \"q\"", 
"\t#    interval = time between records in minutes (default = 10)", 
"\tconf <- intersect(names(mydata),objects(1))", "\tif (length(conf) > 0) {", 
"\t\tcat(\"The following objects conflict with object names in mvue:\\n\")", 
"\t\tcat(conf,\"\\n\")", "\t\tcat(\"Please remove conflicting objects before proceeding.\\n\")", 
"\t\treturn(invisible())", "\t}", "\tif(!missing(mydata)) attach(mydata)", 
"\tk <- 0.06 * interval", "\tlnx <- log(get(x))", "\ty <- log(ssc)", 
"\tysample <- y[sam]", "\tn <- length(ysample)", "\tN <- length(y)", 
"\tq <- q/35.3147", "\t# convert to metric (assumes input in cfs)", 
"\tx1 <- cbind(rep(1, n), lnx[sam])", "\txx <- t(x1) %*% x1", 
"\tinvxx <- solve(xx)", "\tbetahat <- invxx %*% t(x1) %*% ysample", 
"\tyhat <- x1 %*% betahat", "\tresid <- ysample - yhat", "\trsquare <- (cor(ysample, yhat))^2", 
"\ts2 <- sum(resid^2)/(n - 2)", "\tx2 <- cbind(rep(1, N), lnx)", 
"\tV <- x2 %*% invxx %*% t(x2)", "\ttmp1 <- matrix(diag(V), ncol = N, nrow = N)", 
"\tttmp1 <- t(tmp1)", "\ttmp2 <- exp(((tmp1 + ttmp1 + 2 * V) * s2)/2)", 
"\tnewyhat <- drop(x2 %*% betahat)", "\ttmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)", 
"\ttmp4 <- tmp3 * tmp2 * t(tmp3)", "\tm <- n - 2", "\ttmp5 <- exp(((2 - ttmp1 - tmp1) * s2)/2)", 
"\ttmp6 <- finney(m, ((1 - tmp1) * (1 - ttmp1) * (s2^2) * (m + 1))/(2 *", 
"\t\tm^2))", "\tmvue <- k * q * exp(newyhat) * finney(m, ((1 - diag(V)) * s2 * (m +", 
"\t\t1))/(2 * m))", "\tEXY <- tmp4 * tmp5 * tmp6", "\tmumvue <- exp(newyhat + s2/2)", 
"\tEX <- matrix(mumvue, ncol = N, nrow = N)", "\tEY <- t(EX)", 
"\tCOVXY <- EXY - EX * EY", "\tqmat <- k * matrix(q, ncol = N, nrow = N)", 
"\tCOVXY2 <- qmat * COVXY * t(qmat)", "\tMSE <- sum(COVXY2)", 
"\tif(!missing(mydata))", "\t\tdetach(2)", "\tlist(est.load = sum(mvue), betahat = betahat, rsquare = rsquare, ", 
"\t\test.rmse = sqrt(MSE))", "}"))
my.textentry <-
structure(function (panel, var, action = I, labels = NA, names = labels, 
    title = NA, initval = NA, parent = window, pos = NULL, signal = "<KeyRelease>", 
    ...) 
{
    ischar <- is.character(panel)
    if (ischar) {
        panelname <- panel
        panel <- .geval(panel)
    }
    else {
        panelname <- panel$intname
        panelreturn <- deparse(substitute(panel))
        .gassign(panel, panelname)
    }
    pos = .newpos(pos, ...)
    if (.checklayout(pos)) {
        varname <- deparse(substitute(var))
        if ((varname %in% names(panel)) && (all(is.na(initval)))) {
            initval <- .geval(panelname, "$", varname)
        }
        if ((length(initval) == 1) && (is.na(initval))) {
            if ((length(labels) == 1) && (is.na(labels))) {
                nboxes <- 1
                if (is.na(title)) 
                  title <- varname
                labels <- varname
            }
            else {
                nboxes <- length(labels)
                if (is.na(title) & (nboxes == 1)) 
                  title <- labels
            }
            initval <- rep(NA, nboxes)
        }
        else {
            nboxes <- length(initval)
            if ((length(labels) == 1) && (is.na(labels))) 
                if (nboxes != 1) {
                  labels <- paste(varname, 1:nboxes, sep = "")
                }
                else {
                  labels <- varname
                }
            else if (length(labels) != nboxes) 
                stop("lengths of labels and initval do not match.")
        }
        if ((nboxes == 1) & (!is.na(title))) 
            labels <- title
        .geval(panelname, "$", varname, " <- vector(length=", 
            nboxes, ")")
        if (nboxes > 1) 
            if (is.na(title)) 
                title <- varname
        if ((!is.list(pos)) || (is.null(pos$grid))) {
            gd = panel$window
        }
        else {
            gd = .geval(panelname, "$", pos$grid)
        }
        if (nboxes > 1) {
            frame <- tkwidget(gd, "labelframe", text = title, 
                padx = 2, pady = 2)
        }
        else {
            frame <- tkframe(gd)
        }
        if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {
            .rp.layout(frame, pos)
        }
        else {
            if (is.null(pos$sticky)) {
                pos$sticky <- "w"
            }
            if (is.null(pos$rowspan)) {
                pos$rowspan = 1
            }
            if (is.null(pos$columnspan)) {
                pos$columnspan = 1
            }
            tkgrid(frame, row = pos$row, column = pos$column, 
                sticky = pos$sticky, `in` = gd, rowspan = pos$rowspan, 
                columnspan = pos$columnspan)
        }
        for (i in 1:nboxes) {
            if (is.na(initval[i])) 
                initval[i] <- "NA"
            inittclvalue <- .rp.initialise(panelname, paste(varname, 
                i, sep = ""), initval[i])
            tclvariable <- .geval(panelname, "$", varname, i, 
                ".tcl <- tclVar(", deparse(inittclvalue), ")")
            if (is.numeric(inittclvalue)) {
                .geval(panelname, "$", varname, "[", i, "] <- deparse(", 
                  inittclvalue, ")")
            }
            else {
                .geval(panelname, "$", varname, "[", i, "] <- '", 
                  inittclvalue, "'")
            }
            if (!any(is.na(names))) {
                .geval("names(", panelname, "$", varname, ")[", 
                  i, "] <- '", names[i], "'")
            }
            f <- function() {
                for (i in 1:nboxes) {
                  .geval(panelname, "$", varname, "[", i, "] <- tclvalue(", 
                    panelname, "$", varname, i, ".tcl)")
                  if (!any(is.na(names))) {
                    .geval("names(", panelname, "$", varname, 
                      ")[", i, "] <- '", names[i], "'")
                  }
                }
                panel <- action(.geval(panelname))
                if (!is.null(panel$intname)) {
                  .gassign(panel, panelname)
                }
                else {
                  stop("The panel was not passed back from the action function.")
                }
            }
            if (!any(is.na(names))) {
                .geval("names(", panelname, "$", varname, ")[", 
                  i, "] <- '", names[i], "'")
            }
            label <- tklabel(frame, text = labels[i], height = "1")
            if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {
                entry <- tkentry(frame, textvariable = tclvariable)
            }
            else {
                entry <- tkentry(frame, textvariable = tclvariable, 
                  width = pos$width)
            }
            tkgrid(label, entry)
            tkgrid.configure(label, sticky = "w")
            tkgrid.configure(entry, sticky = "e")
            tkbind(entry, signal, f)
        }
    }
    if (ischar) 
        invisible(panelname)
    else assign(panelreturn, .geval(panelname), envir = parent.frame())
}, source = c("function (panel, var, action = I, labels = NA, names = labels, ", 
"    title = NA, initval = NA, parent = window, pos = NULL, signal=\"<KeyRelease>\", ...) ", 
"{", "# Modification of rpanel's rptextentry. ", "# This version will accept text input on any signal recognized by tkbind, e.g.", 
"# <Key>, <KeyRelease>, <Key-Return>, <Button>, <ButtonRelease> , etc, etc", 
"# So you don't have to press <ENTER> to have your text accepted!", 
"    ischar <- is.character(panel)", "    if (ischar) {", "        panelname <- panel", 
"        panel <- .geval(panel)", "    }", "    else {", "        panelname <- panel$intname", 
"        panelreturn <- deparse(substitute(panel))", "        .gassign(panel, panelname)", 
"    }", "    pos = .newpos(pos, ...)", "    if (.checklayout(pos)) {", 
"        varname <- deparse(substitute(var))", "        if ((varname %in% names(panel)) && (all(is.na(initval)))) {", 
"            initval <- .geval(panelname, \"$\", varname)", "        }", 
"        if ((length(initval) == 1) && (is.na(initval))) {", 
"            if ((length(labels) == 1) && (is.na(labels))) {", 
"                nboxes <- 1", "                if (is.na(title)) ", 
"                  title <- varname", "                labels <- varname", 
"            }", "            else {", "                nboxes <- length(labels)", 
"                if (is.na(title) & (nboxes == 1)) ", "                  title <- labels", 
"            }", "            initval <- rep(NA, nboxes)", "        }", 
"        else {", "            nboxes <- length(initval)", "            if ((length(labels) == 1) && (is.na(labels))) ", 
"                if (nboxes != 1) {", "                  labels <- paste(varname, 1:nboxes, sep = \"\")", 
"                }", "                else {", "                  labels <- varname", 
"                }", "            else if (length(labels) != nboxes) ", 
"                stop(\"lengths of labels and initval do not match.\")", 
"        }", "        if ((nboxes == 1) & (!is.na(title))) ", 
"            labels <- title", "        .geval(panelname, \"$\", varname, \" <- vector(length=\", ", 
"            nboxes, \")\")", "        if (nboxes > 1) ", "            if (is.na(title)) ", 
"                title <- varname", "        if ((!is.list(pos)) || (is.null(pos$grid))) {", 
"            gd = panel$window", "        }", "        else {", 
"            gd = .geval(panelname, \"$\", pos$grid)", "        }", 
"        if (nboxes > 1) {", "            frame <- tkwidget(gd, \"labelframe\", text = title, ", 
"                padx = 2, pady = 2)", "        }", "        else {", 
"            frame <- tkframe(gd)", "        }", "        if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {", 
"            .rp.layout(frame, pos)", "        }", "        else {", 
"            if (is.null(pos$sticky)) {", "                pos$sticky <- \"w\"", 
"            }", "            if (is.null(pos$rowspan)) {", "                pos$rowspan = 1", 
"            }", "            if (is.null(pos$columnspan)) {", 
"                pos$columnspan = 1", "            }", "            tkgrid(frame, row = pos$row, column = pos$column, ", 
"                sticky = pos$sticky, `in` = gd, rowspan = pos$rowspan, ", 
"                columnspan = pos$columnspan)", "        }", 
"        for (i in 1:nboxes) {", "            if (is.na(initval[i])) ", 
"                initval[i] <- \"NA\"", "            inittclvalue <- .rp.initialise(panelname, paste(varname, ", 
"                i, sep = \"\"), initval[i])", "            tclvariable <- .geval(panelname, \"$\", varname, i, ", 
"                \".tcl <- tclVar(\", deparse(inittclvalue), \")\")", 
"            if (is.numeric(inittclvalue)) {", "                .geval(panelname, \"$\", varname, \"[\", i, \"] <- deparse(\", ", 
"                  inittclvalue, \")\")", "            }", "            else {", 
"                .geval(panelname, \"$\", varname, \"[\", i, \"] <- '\", ", 
"                  inittclvalue, \"'\")", "            }", "            if (!any(is.na(names))) {", 
"                .geval(\"names(\", panelname, \"$\", varname, \")[\", ", 
"                  i, \"] <- '\", names[i], \"'\")", "            }", 
"            f <- function() {", "                for (i in 1:nboxes) {", 
"                  .geval(panelname, \"$\", varname, \"[\", i, \"] <- tclvalue(\", ", 
"                    panelname, \"$\", varname, i, \".tcl)\")", 
"                  if (!any(is.na(names))) {", "                    .geval(\"names(\", panelname, \"$\", varname, ", 
"                      \")[\", i, \"] <- '\", names[i], \"'\")", 
"                  }", "                }", "                panel <- action(.geval(panelname))", 
"                if (!is.null(panel$intname)) {", "                  .gassign(panel, panelname)", 
"                }", "                else {", "                  stop(\"The panel was not passed back from the action function.\")", 
"                }", "            }", "            if (!any(is.na(names))) {", 
"                .geval(\"names(\", panelname, \"$\", varname, \")[\", ", 
"                  i, \"] <- '\", names[i], \"'\")", "            }", 
"            label <- tklabel(frame, text = labels[i], height = \"1\")", 
"            if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {", 
"                entry <- tkentry(frame, textvariable = tclvariable)", 
"            }", "            else {", "                entry <- tkentry(frame, textvariable = tclvariable, ", 
"                  width = pos$width)", "            }", "            tkgrid(label, entry)", 
"            tkgrid.configure(label, sticky = \"w\")", "            tkgrid.configure(entry, sticky = \"e\")", 
"            tkbind(entry, signal, f)", "        }", "    }", 
"    if (ischar) ", "        invisible(panelname)", "    else assign(panelreturn, .geval(panelname), envir = parent.frame())", 
"}"))
mystamp <-
structure(function (cex = 0.7, string = date()) 
{
    mtext(string, side = 1, line = -1, cex = cex, outer = T, 
        adj = 1)
}, source = c("function (cex = 0.7, string = date()) ", "{", 
"    mtext(string, side = 1, line = -1, cex = cex, outer = T, ", 
"        adj = 1)", "}"))
normal.sample.size <-
structure(function (mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, 
    power = 0.8, alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, 
    one.sample = missing(sd2), alternative = "two.sided", expand.args = T, 
    exact.n = F, recompute.power = F) 
{
    compute.sample.size <- function(Zalpha, Zpower, sd1, sd2, 
        prop.n2, delta, one.sample, exact.n, ...) {
        n <- ((sd1 * (Zalpha + Zpower))/delta)^2
        if (!one.sample) {
            n1 <- n * (1 + sd2^2/(prop.n2 * sd1^2))
            if (!exact.n) {
                n1 <- ceiling(n1)
                n2 <- ceiling(prop.n2 * n1)
            }
            else {
                n2 <- prop.n2 * n1
            }
            n <- list(n1, n2)
        }
        else if (!exact.n) {
            n <- ceiling(n)
        }
        return(n)
    }
    compute.power <- function(Zalpha, sd1, sd2, n1, n2, prop.n2, 
        delta, one.sample, alternative, ...) {
        if (one.sample) {
            sigma.inv <- sqrt(n1)/sd1
        }
        else {
            sigma.inv <- sqrt(n2/(prop.n2 * sd1^2 + sd2^2))
        }
        power <- switch(alternative, greater = ifelse(delta == 
            0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)), 
            less = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(-delta * 
                sigma.inv - Zalpha)), two.sided = ifelse(delta == 
                0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - 
                Zalpha)) + ifelse(delta == 0, 1 - pnorm(Zalpha), 
                pnorm(-delta * sigma.inv - Zalpha)))
        return(power)
    }
    compute.delta <- function(Zalpha, Zpower, sd1, sd2, n1, n2, 
        prop.n2, one.sample, alternative, ...) {
        if (one.sample) {
            sigma <- sd1/sqrt(n1)
        }
        else {
            sigma <- sqrt((prop.n2 * sd1^2 + sd2^2)/n2)
        }
        delta <- (Zalpha + Zpower) * sigma
        if (alternative == "less") {
            delta <- (-delta)
        }
        return(delta)
    }
    if (!missing(alternative)) {
        alt.expanded <- char.expand(alternative, c("two.sided", 
            "greater", "less"), stop("argument 'alternative' must match one of 'greater', 'less', 'two.sided'."))
    }
    else {
        alt.expanded <- alternative
    }
    if (is.null(n1) && is.null(n2)) {
        compute.what <- "sample.size"
        compute.function <- "compute.sample.size"
        n1 <- n2 <- as.numeric(NA)
    }
    else if ((missing(mean2) || is.null(mean2)) && (missing(mean.alt) || 
        is.null(mean.alt))) {
        compute.what <- "delta"
        compute.function <- "compute.delta"
        delta <- as.numeric(NA)
        mean2 <- as.numeric(NA)
    }
    else {
        compute.what <- "power"
        compute.function <- "compute.power"
        power <- as.numeric(NA)
    }
    if (compute.what != "delta") {
        if (!(missing(mean2) || is.null(mean2))) {
            if (missing(one.sample)) {
                one.sample <- F
            }
        }
        else if (!(missing(mean.alt) || is.null(mean.alt))) {
            mean2 <- mean.alt
            if (missing(one.sample)) {
                one.sample <- T
            }
        }
        else {
            stop(paste("A second (alternative) mean is required to compute", 
                compute.what))
        }
    }
    else if (missing(one.sample)) {
        one.sample <- missing(n2) && missing(prop.n2)
    }
    if (one.sample) {
        arg.names <- c("mean1", "sd1", "mean2", "delta", "alpha", 
            "power", "n1")
        table.names <- c("mean.null", "sd1", "mean.alt", "delta", 
            "alpha", "power", "n1")
        n2 <- NULL
        prop.n2 <- NULL
        sd2 <- NULL
    }
    else {
        table.names <- c("mean1", "sd1", "mean2", "sd2", "delta", 
            "alpha", "power", "n1", "n2", "prop.n2")
        arg.names <- table.names
    }
    power.table <- build.power.table(theta1 = mean, disp1 = sd1, 
        theta2 = mean2, disp2 = sd2, alpha = alpha, power = power, 
        n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, 
        one.sample = one.sample, compute.what = compute.what)
    names(power.table) <- arg.names
    if (alt.expanded == "two.sided") {
        Zalpha <- qnorm(1 - power.table$alpha/2)
    }
    else {
        Zalpha <- qnorm(1 - power.table$alpha)
    }
    if (!all(is.na(power.table$power))) {
        Zpower <- qnorm(power.table$power)
    }
    else {
        Zpower <- as.numeric(NA)
    }
    arglist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, 
        one.sample = one.sample, exact.n = exact.n, alternative = alt.expanded))
    if (compute.what == "sample.size") {
        if (one.sample) {
            compute.what <- "n1"
        }
        else {
            compute.what <- c("n1", "n2")
        }
    }
    power.table[, compute.what] <- do.call(compute.function, 
        arglist)
    if (recompute.power && !exact.n && compute.function == "compute.sample.size") {
        if (one.sample) {
            arglist[[compute.what]] <- power.table[, compute.what]
        }
        else {
            arglist[compute.what] <- power.table[, compute.what]
        }
        power.table[, "power"] <- do.call("compute.power", arglist)
    }
    if (compute.function == "compute.delta") {
        power.table$mean2 <- switch(alt.expanded, two.sided = NA, 
            greater = power.table$mean1 + abs(power.table$delta), 
            less = power.table$mean1 - abs(power.table$delta))
    }
    names(power.table) <- table.names
    return(power.table)
}, source = c("function(mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, power = 0.8, alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, ", 
"\tone.sample = missing(sd2), alternative = \"two.sided\", expand.args = T, exact.n = F, recompute.power = F)", 
"{", "#=====================", "# ", "# formulas", "#     ", 
"#=====================", "#", "# The formulas used are from ", 
"#", "#   Biostatistics by Lloyd D. Fisher and Gerald Van Belle", 
"#   John Wiley & Sons, 1993", "#", "# Z_a = Z(1 - alpha/2) for two-tailed test, or Z(1 - alpha) for one tail.", 
"# Z_p = Z(1 - beta) ", "#", "# One-sample:", "#", "#    delta = abs(mean.null - mean.alt)", 
"#        n = {sigma * (Z_a + Z_b) / delta}^2", "#   ", "# Two-sample:", 
"#", "#    delta = abs(mean2 - mean1)", "#       n1 = {1 + sigma2^2 / (k*sigma1^2) } * {sigma1 * (Z_a + Z_b) / delta}^2 ", 
"#       n2 = k*n1", "#", "#=====================", "# ", "# functions", 
"#     ", "#=====================", "#", "#--------------------------------------------", 
"#", "# Sample Size function", "#", "#--------------------------------------------", 
"\tcompute.sample.size <- function(Zalpha, Zpower, sd1, sd2, prop.n2, delta, one.sample, exact.n, ...)", 
"\t{", "\t\tn <- ((sd1 * (Zalpha + Zpower))/delta)^2", "\t\tif(!one.sample) {", 
"\t\t\tn1 <- n * (1 + sd2^2/(prop.n2 * sd1^2))", "\t\t\tif(!exact.n) {", 
"\t\t\t\tn1 <- ceiling(n1)", "\t\t\t\tn2 <- ceiling(prop.n2 * n1)", 
"\t\t\t}", "\t\t\telse {", "\t\t\t\tn2 <- prop.n2 * n1", "\t\t\t}", 
"\t\t\tn <- list(n1, n2)", "\t\t}", "\t\telse if(!exact.n) {", 
"\t\t\tn <- ceiling(n)", "\t\t}", "\t\treturn(n)", "\t}", "#--------------------------------------------", 
"#", "# Power function", "#", "#--------------------------------------------", 
"\tcompute.power <- function(Zalpha, sd1, sd2, n1, n2, prop.n2, delta, one.sample, alternative, ...)", 
"\t{", "\t\tif(one.sample) {", "\t\t\tsigma.inv <- sqrt(n1)/sd1", 
"\t\t}", "\t\telse {", "\t\t\tsigma.inv <- sqrt(n2/(prop.n2 * sd1^2 + sd2^2))", 
"\t\t}", "\t\tpower <- switch(alternative,", "\t\t\tgreater = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)),", 
"\t\t\tless = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm( - delta * sigma.inv - Zalpha)),", 
"\t\t\ttwo.sided = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)) + ifelse(", 
"\t\t\t\tdelta == 0, 1 - pnorm(Zalpha), pnorm( - delta * sigma.inv - Zalpha)))", 
"\t\treturn(power)", "\t}", "#--------------------------------------------", 
"#", "# Delta function", "#", "#--------------------------------------------", 
"\tcompute.delta <- function(Zalpha, Zpower, sd1, sd2, n1, n2, prop.n2, one.sample, alternative, ...)", 
"\t{", "\t\tif(one.sample) {", "\t\t\tsigma <- sd1/sqrt(n1)", 
"\t\t}", "\t\telse {", "\t\t\tsigma <- sqrt((prop.n2 * sd1^2 + sd2^2)/n2)", 
"\t\t}", "\t\tdelta <- (Zalpha + Zpower) * sigma", "\t\tif(alternative == \"less\") {", 
"\t\t\tdelta <- ( - delta)", "\t\t}", "\t\treturn(delta)", "\t}", 
"#-----------------------------------", "#", "# main section", 
"#", "#-----------------------------------", "\tif(!missing(alternative)) {", 
"\t\talt.expanded <- char.expand(alternative, c(\"two.sided\", \"greater\", \"less\"), stop(", 
"\t\t\t\"argument 'alternative' must match one of 'greater', 'less', 'two.sided'.\"))", 
"\t}", "\telse {", "\t\talt.expanded <- alternative", "\t}", 
"\tif(is.null(n1) && is.null(n2)) {", "\t\tcompute.what <- \"sample.size\"", 
"\t\tcompute.function <- \"compute.sample.size\"", "\t\tn1 <- n2 <- as.numeric(NA)", 
"\t}", "\telse if((missing(mean2) || is.null(mean2)) && (missing(mean.alt) || is.null(mean.alt))) {", 
"\t\tcompute.what <- \"delta\"", "\t\tcompute.function <- \"compute.delta\"", 
"\t\tdelta <- as.numeric(NA)", "\t\tmean2 <- as.numeric(NA)", 
"\t}", "\telse {", "\t\tcompute.what <- \"power\"", "\t\tcompute.function <- \"compute.power\"", 
"\t\tpower <- as.numeric(NA)", "\t}", "\tif(compute.what != \"delta\") {", 
"\t\tif(!(missing(mean2) || is.null(mean2))) {", "\t\t\tif(missing(one.sample)) {", 
"\t\t\t\tone.sample <- F", "\t\t\t}", "\t\t}", "\t\telse if(!(missing(mean.alt) || is.null(mean.alt))) {", 
"\t\t\tmean2 <- mean.alt", "\t\t\tif(missing(one.sample)) {", 
"\t\t\t\tone.sample <- T", "\t\t\t}", "\t\t}", "\t\telse {", 
"\t\t\tstop(paste(\"A second (alternative) mean is required to compute\", compute.what))", 
"\t\t}", "\t}", "\telse if(missing(one.sample)) {", "\t\tone.sample <- missing(n2) && missing(prop.n2)", 
"\t}", "\tif(one.sample) {", "\t\targ.names <- c(\"mean1\", \"sd1\", \"mean2\", \"delta\", \"alpha\", \"power\", \"n1\")", 
"\t\ttable.names <- c(\"mean.null\", \"sd1\", \"mean.alt\", \"delta\", \"alpha\", \"power\", \"n1\")", 
"\t\tn2 <- NULL", "\t\tprop.n2 <- NULL", "\t\tsd2 <- NULL", "\t}", 
"\telse {", "\t\ttable.names <- c(\"mean1\", \"sd1\", \"mean2\", \"sd2\", \"delta\", \"alpha\", \"power\", \"n1\", \"n2\", \"prop.n2\")", 
"\t\targ.names <- table.names", "\t}", "\tpower.table <- build.power.table(theta1 = mean, disp1 = sd1, theta2 = mean2, disp2 = sd2, alpha = alpha, power", 
"\t\t = power, n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, one.sample = one.sample, ", 
"\t\tcompute.what = compute.what)", "\tnames(power.table) <- arg.names\t#", 
"###", "### Compute quantiles for alpha and power", "###", "\tif(alt.expanded == \"two.sided\") {", 
"\t\tZalpha <- qnorm(1 - power.table$alpha/2)", "\t}", "\telse {", 
"\t\tZalpha <- qnorm(1 - power.table$alpha)", "\t}", "\tif(!all(is.na(power.table$power))) {", 
"\t\tZpower <- qnorm(power.table$power)", "\t}", "\telse {", 
"\t\tZpower <- as.numeric(NA)", "\t}", "\targlist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, one.sample = one.sample, exact.n = exact.n, ", 
"\t\talternative = alt.expanded))\t#", "#", "# DEBUG: stop here to make sure arglist is correct", 
"#", "# return(arglist)", "#", "###", "### use do.call() to compute value and fill into 'compute.what' column ", 
"###", "\tif(compute.what == \"sample.size\") {", "\t\tif(one.sample) {", 
"\t\t\tcompute.what <- \"n1\"", "\t\t}", "\t\telse {", "\t\t\tcompute.what <- c(\"n1\", \"n2\")", 
"\t\t}", "\t}", "\tpower.table[, compute.what] <- do.call(compute.function, arglist)", 
"\tif(recompute.power && !exact.n && compute.function == \"compute.sample.size\") {", 
"\t\tif(one.sample) {", "\t\t\targlist[[compute.what]] <- power.table[, compute.what]", 
"\t\t}", "\t\telse {", "\t\t\targlist[compute.what] <- power.table[, compute.what]", 
"\t\t}", "\t\tpower.table[, \"power\"] <- do.call(\"compute.power\", arglist)", 
"\t}", "\tif(compute.function == \"compute.delta\") {", "\t\tpower.table$mean2 <- switch(alt.expanded,", 
"\t\t\ttwo.sided = NA,", "\t\t\tgreater = power.table$mean1 + abs(power.table$delta),", 
"\t\t\tless = power.table$mean1 - abs(power.table$delta)", "\t\t\t)", 
"\t}", "\tnames(power.table) <- table.names", "\treturn(power.table)", 
"}"))
nss <-
structure(function (mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, 
    power = 0.8, alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, 
    one.sample = missing(sd2), alternative = "two.sided", expand.args = T, 
    exact.n = F, recompute.power = F) 
{
    compute.sample.size <- function(Zalpha, Zpower, sd1, sd2, 
        prop.n2, delta, one.sample, exact.n, ...) {
        n <- ((sd1 * (Zalpha + Zpower))/delta)^2
        if (!one.sample) {
            n1 <- n * (1 + sd2^2/(prop.n2 * sd1^2))
            if (!exact.n) {
                n1 <- ceiling(n1)
                n2 <- ceiling(prop.n2 * n1)
            }
            else {
                n2 <- prop.n2 * n1
            }
            n <- list(n1, n2)
        }
        else if (!exact.n) {
            n <- ceiling(n)
        }
        return(n)
    }
    compute.power <- function(Zalpha, sd1, sd2, n1, n2, prop.n2, 
        delta, one.sample, alternative, ...) {
        if (one.sample) {
            sigma.inv <- sqrt(n1)/sd1
        }
        else {
            sigma.inv <- sqrt(n2/(prop.n2 * sd1^2 + sd2^2))
        }
        power <- switch(alternative, greater = ifelse(delta == 
            0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)), 
            less = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(-delta * 
                sigma.inv - Zalpha)), two.sided = ifelse(delta == 
                0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - 
                Zalpha)) + ifelse(delta == 0, 1 - pnorm(Zalpha), 
                pnorm(-delta * sigma.inv - Zalpha)))
        return(power)
    }
    compute.delta <- function(Zalpha, Zpower, sd1, sd2, n1, n2, 
        prop.n2, one.sample, alternative, ...) {
        if (one.sample) {
            sigma <- sd1/sqrt(n1)
        }
        else {
            sigma <- sqrt((prop.n2 * sd1^2 + sd2^2)/n2)
        }
        delta <- (Zalpha + Zpower) * sigma
        if (alternative == "less") {
            delta <- (-delta)
        }
        return(delta)
    }
    if (!missing(alternative)) {
        alt.expanded <- char.expand(alternative, c("two.sided", 
            "greater", "less"), stop("argument 'alternative' must match one of 'greater', 'less', 'two.sided'."))
    }
    else {
        alt.expanded <- alternative
    }
    if (is.null(n1) && is.null(n2)) {
        compute.what <- "sample.size"
        compute.function <- "compute.sample.size"
        n1 <- n2 <- as.numeric(NA)
    }
    else if ((missing(mean2) || is.null(mean2)) && (missing(mean.alt) || 
        is.null(mean.alt))) {
        compute.what <- "delta"
        compute.function <- "compute.delta"
        delta <- as.numeric(NA)
        mean2 <- as.numeric(NA)
    }
    else {
        compute.what <- "power"
        compute.function <- "compute.power"
        power <- as.numeric(NA)
    }
    if (compute.what != "delta") {
        if (!(missing(mean2) || is.null(mean2))) {
            if (missing(one.sample)) {
                one.sample <- F
            }
        }
        else if (!(missing(mean.alt) || is.null(mean.alt))) {
            mean2 <- mean.alt
            if (missing(one.sample)) {
                one.sample <- T
            }
        }
        else {
            stop(paste("A second (alternative) mean is required to compute", 
                compute.what))
        }
    }
    else if (missing(one.sample)) {
        one.sample <- missing(n2) && missing(prop.n2)
    }
    if (one.sample) {
        arg.names <- c("mean1", "sd1", "mean2", "delta", "alpha", 
            "power", "n1")
        table.names <- c("mean.null", "sd1", "mean.alt", "delta", 
            "alpha", "power", "n1")
        n2 <- NULL
        prop.n2 <- NULL
        sd2 <- NULL
    }
    else {
        table.names <- c("mean1", "sd1", "mean2", "sd2", "delta", 
            "alpha", "power", "n1", "n2", "prop.n2")
        arg.names <- table.names
    }
    power.table <- build.power.table(theta1 = mean, disp1 = sd1, 
        theta2 = mean2, disp2 = sd2, alpha = alpha, power = power, 
        n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, 
        one.sample = one.sample, compute.what = compute.what)
    names(power.table) <- arg.names
    if (alt.expanded == "two.sided") {
        Zalpha <- qnorm(1 - power.table$alpha/2)
    }
    else {
        Zalpha <- qnorm(1 - power.table$alpha)
    }
    if (!all(is.na(power.table$power))) {
        Zpower <- qnorm(power.table$power)
    }
    else {
        Zpower <- as.numeric(NA)
    }
    arglist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, 
        one.sample = one.sample, exact.n = exact.n, alternative = alt.expanded))
    if (compute.what == "sample.size") {
        if (one.sample) {
            compute.what <- "n1"
        }
        else {
            compute.what <- c("n1", "n2")
        }
    }
    power.table[, compute.what] <- do.call(compute.function, 
        arglist)
    if (recompute.power && !exact.n && compute.function == "compute.sample.size") {
        if (one.sample) {
            arglist[[compute.what]] <- power.table[, compute.what]
        }
        else {
            arglist[compute.what] <- power.table[, compute.what]
        }
        power.table[, "power"] <- do.call("compute.power", arglist)
    }
    if (compute.function == "compute.delta") {
        power.table$mean2 <- switch(alt.expanded, two.sided = NA, 
            greater = power.table$mean1 + abs(power.table$delta), 
            less = power.table$mean1 - abs(power.table$delta))
    }
    names(power.table) <- table.names
    return(power.table)
}, source = c("function(mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, power = 0.8, alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, ", 
"\tone.sample = missing(sd2), alternative = \"two.sided\", expand.args = T, exact.n = F, recompute.power = F)", 
"{", "#=====================", "# ", "# formulas", "#     ", 
"#=====================", "#", "# The formulas used are from ", 
"#", "#   Biostatistics by Lloyd D. Fisher and Gerald Van Belle", 
"#   John Wiley & Sons, 1993", "#", "# Z_a = Z(1 - alpha/2) for two-tailed test, or Z(1 - alpha) for one tail.", 
"# Z_p = Z(1 - beta) ", "#", "# One-sample:", "#", "#    delta = abs(mean.null - mean.alt)", 
"#        n = {sigma * (Z_a + Z_b) / delta}^2", "#   ", "# Two-sample:", 
"#", "#    delta = abs(mean2 - mean1)", "#       n1 = {1 + sigma2^2 / (k*sigma1^2) } * {sigma1 * (Z_a + Z_b) / delta}^2 ", 
"#       n2 = k*n1", "#", "#=====================", "# ", "# functions", 
"#     ", "#=====================", "#", "#--------------------------------------------", 
"#", "# Sample Size function", "#", "#--------------------------------------------", 
"\tcompute.sample.size <- function(Zalpha, Zpower, sd1, sd2, prop.n2, delta, one.sample, exact.n, ...)", 
"\t{", "\t\tn <- ((sd1 * (Zalpha + Zpower))/delta)^2", "\t\tif(!one.sample) {", 
"\t\t\tn1 <- n * (1 + sd2^2/(prop.n2 * sd1^2))", "\t\t\tif(!exact.n) {", 
"\t\t\t\tn1 <- ceiling(n1)", "\t\t\t\tn2 <- ceiling(prop.n2 * n1)", 
"\t\t\t}", "\t\t\telse {", "\t\t\t\tn2 <- prop.n2 * n1", "\t\t\t}", 
"\t\t\tn <- list(n1, n2)", "\t\t}", "\t\telse if(!exact.n) {", 
"\t\t\tn <- ceiling(n)", "\t\t}", "\t\treturn(n)", "\t}", "#--------------------------------------------", 
"#", "# Power function", "#", "#--------------------------------------------", 
"\tcompute.power <- function(Zalpha, sd1, sd2, n1, n2, prop.n2, delta, one.sample, alternative, ...)", 
"\t{", "\t\tif(one.sample) {", "\t\t\tsigma.inv <- sqrt(n1)/sd1", 
"\t\t}", "\t\telse {", "\t\t\tsigma.inv <- sqrt(n2/(prop.n2 * sd1^2 + sd2^2))", 
"\t\t}", "\t\tpower <- switch(alternative,", "\t\t\tgreater = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)),", 
"\t\t\tless = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm( - delta * sigma.inv - Zalpha)),", 
"\t\t\ttwo.sided = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)) + ifelse(", 
"\t\t\t\tdelta == 0, 1 - pnorm(Zalpha), pnorm( - delta * sigma.inv - Zalpha)))", 
"\t\treturn(power)", "\t}", "#--------------------------------------------", 
"#", "# Delta function", "#", "#--------------------------------------------", 
"\tcompute.delta <- function(Zalpha, Zpower, sd1, sd2, n1, n2, prop.n2, one.sample, alternative, ...)", 
"\t{", "\t\tif(one.sample) {", "\t\t\tsigma <- sd1/sqrt(n1)", 
"\t\t}", "\t\telse {", "\t\t\tsigma <- sqrt((prop.n2 * sd1^2 + sd2^2)/n2)", 
"\t\t}", "\t\tdelta <- (Zalpha + Zpower) * sigma", "\t\tif(alternative == \"less\") {", 
"\t\t\tdelta <- ( - delta)", "\t\t}", "\t\treturn(delta)", "\t}", 
"#-----------------------------------", "#", "# main section", 
"#", "#-----------------------------------", "\tif(!missing(alternative)) {", 
"\t\talt.expanded <- char.expand(alternative, c(\"two.sided\", \"greater\", \"less\"), stop(", 
"\t\t\t\"argument 'alternative' must match one of 'greater', 'less', 'two.sided'.\"))", 
"\t}", "\telse {", "\t\talt.expanded <- alternative", "\t}", 
"\tif(is.null(n1) && is.null(n2)) {", "\t\tcompute.what <- \"sample.size\"", 
"\t\tcompute.function <- \"compute.sample.size\"", "\t\tn1 <- n2 <- as.numeric(NA)", 
"\t}", "\telse if((missing(mean2) || is.null(mean2)) && (missing(mean.alt) || is.null(mean.alt))) {", 
"\t\tcompute.what <- \"delta\"", "\t\tcompute.function <- \"compute.delta\"", 
"\t\tdelta <- as.numeric(NA)", "\t\tmean2 <- as.numeric(NA)", 
"\t}", "\telse {", "\t\tcompute.what <- \"power\"", "\t\tcompute.function <- \"compute.power\"", 
"\t\tpower <- as.numeric(NA)", "\t}", "\tif(compute.what != \"delta\") {", 
"\t\tif(!(missing(mean2) || is.null(mean2))) {", "\t\t\tif(missing(one.sample)) {", 
"\t\t\t\tone.sample <- F", "\t\t\t}", "\t\t}", "\t\telse if(!(missing(mean.alt) || is.null(mean.alt))) {", 
"\t\t\tmean2 <- mean.alt", "\t\t\tif(missing(one.sample)) {", 
"\t\t\t\tone.sample <- T", "\t\t\t}", "\t\t}", "\t\telse {", 
"\t\t\tstop(paste(\"A second (alternative) mean is required to compute\", compute.what))", 
"\t\t}", "\t}", "\telse if(missing(one.sample)) {", "\t\tone.sample <- missing(n2) && missing(prop.n2)", 
"\t}", "\tif(one.sample) {", "\t\targ.names <- c(\"mean1\", \"sd1\", \"mean2\", \"delta\", \"alpha\", \"power\", \"n1\")", 
"\t\ttable.names <- c(\"mean.null\", \"sd1\", \"mean.alt\", \"delta\", \"alpha\", \"power\", \"n1\")", 
"\t\tn2 <- NULL", "\t\tprop.n2 <- NULL", "\t\tsd2 <- NULL", "\t}", 
"\telse {", "\t\ttable.names <- c(\"mean1\", \"sd1\", \"mean2\", \"sd2\", \"delta\", \"alpha\", \"power\", \"n1\", \"n2\", \"prop.n2\")", 
"\t\targ.names <- table.names", "\t}", "\tpower.table <- build.power.table(theta1 = mean, disp1 = sd1, theta2 = mean2, disp2 = sd2, alpha = alpha, power", 
"\t\t = power, n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, one.sample = one.sample, ", 
"\t\tcompute.what = compute.what)", "\tnames(power.table) <- arg.names\t#", 
"###", "### Compute quantiles for alpha and power", "###", "\tif(alt.expanded == \"two.sided\") {", 
"\t\tZalpha <- qnorm(1 - power.table$alpha/2)", "\t}", "\telse {", 
"\t\tZalpha <- qnorm(1 - power.table$alpha)", "\t}", "\tif(!all(is.na(power.table$power))) {", 
"\t\tZpower <- qnorm(power.table$power)", "\t}", "\telse {", 
"\t\tZpower <- as.numeric(NA)", "\t}", "\targlist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, one.sample = one.sample, exact.n = exact.n, ", 
"\t\talternative = alt.expanded))\t#", "#", "# DEBUG: stop here to make sure arglist is correct", 
"#", "# return(arglist)", "#", "###", "### use do.call() to compute value and fill into 'compute.what' column ", 
"###", "\tif(compute.what == \"sample.size\") {", "\t\tif(one.sample) {", 
"\t\t\tcompute.what <- \"n1\"", "\t\t}", "\t\telse {", "\t\t\tcompute.what <- c(\"n1\", \"n2\")", 
"\t\t}", "\t}", "\tpower.table[, compute.what] <- do.call(compute.function, arglist)", 
"\tif(recompute.power && !exact.n && compute.function == \"compute.sample.size\") {", 
"\t\tif(one.sample) {", "\t\t\targlist[[compute.what]] <- power.table[, compute.what]", 
"\t\t}", "\t\telse {", "\t\t\targlist[compute.what] <- power.table[, compute.what]", 
"\t\t}", "\t\tpower.table[, \"power\"] <- do.call(\"compute.power\", arglist)", 
"\t}", "\tif(compute.function == \"compute.delta\") {", "\t\tpower.table$mean2 <- switch(alt.expanded,", 
"\t\t\ttwo.sided = NA,", "\t\t\tgreater = power.table$mean1 + abs(power.table$delta),", 
"\t\t\tless = power.table$mean1 - abs(power.table$delta)", "\t\t\t)", 
"\t}", "\tnames(power.table) <- table.names", "\treturn(power.table)", 
"}"))
oldttsplot <-
structure(function (stn, hy, msc1 = NULL, tts1 = NULL, msc2 = NULL, 
    tts2 = NULL, msc3 = NULL, adj = T, number = T, units = "cumecs", 
    split = 0.35, grid1, grid2, ...) 
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
}, source = c("function(stn, hy, msc1 = NULL, tts1 = NULL, msc2 = NULL, tts2 = NULL, msc3 = NULL, ", 
"        adj = T, number = T, units = \"cumecs\", split=0.35, grid1, grid2,...) {", 
"        #  Modified Jan 25, 2016 can plot composite method results (uses 2 line types)", 
"        #  Modified Mar 24, 2016 default changed to units=\"cumecs\"", 
"        #  Modified Jan 26, 2011 changed NAs in arglist to NULL for running in R 2.6.x or later", 
"        #  Modified June 26, 2008 Now adds gridlines ", "        #  grid1, if supplied, sets number of gridlines in first plot", 
"        #  grid2, if supplied, sets number of gridlines in second plot", 
"        #  Modified May 18, 2007.  Now calls dis.adjust to limit extrapolated adjustments", 
"        #  Modified Jul 18, 2006.  Sqrt may need separate lines for turb and SSC", 
"        #      because of Duan's bias correction", "        #  Modified Jun 09, 2005 to recognize \"type\" component of plotting objects", 
"        #  Modified Jul 29, 2004.  Removed stagetics argument.  Stage tick locations", 
"        #      are now automatically calculated.  User must supply rating equations", 
"        #      in the qcalc function.", "        #  ttsplot() plots the discharge, turbidity and ssc for a TTS storm", 
"        #  stn  3-character station name in quotes", "        #  hy   2-digit water year", 
"        #  msc1, msc2, msc3     objects created by flowsrc or lineartime", 
"        #  tts1, tts2           objects created by turbsrc", 
"        #  extralinetype        logical value governing whether to use different line types     ", 
"        #  adj  logical value denoting whether to adjust sample concentrations to", 
"        #       the cross-sectional mean based on coefficients in object \"discoef\"", 
"        #  number       logical value denoting whether to plot sediment samples as a", 
"        #               sample number (T) or as a solid circle (F)", 
"        #  units        discharge units: \"cfs\"=ft3/sec or \"cumecs\"=m3/sec)", 
"        #  split        the proportion of the figure region that will be devoted to", 
"        #               the discharge hydrograph ", "        if(units == \"cfs\") {", 
"                cat(\"DISCHARGE WILL BE ASSUMED TO HAVE UNITS OF: \", units,", 
"                        \"\\n\")", "                cat(\"If discharge is in metric, all functions such as 'turbsrc' and 'flowsrc', \\n'lineartime', etc, as well as 'ttsplot', MUST be told explicitly using \\nthe argument 'units=\\\"cumecs\\\"'\\n\"", 
"                        )", "        }", "        hy <- zfill(hy, 2)", 
"        if(!missing(tts1) & !missing(tts2))", "                extralinetype <- T", 
"        else extralinetype <- F", "        if (missing(tts2)) {", 
"                tts2$chr <- NA", "                tts2$turb <- NA", 
"        }", "        pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"        sam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"        S <- toupper(stn)", "        fdate1 <- chron(min(tts1$chr, tts2$chr, msc1$chr, msc2$chr, msc3$chr,", 
"                na.rm = T))", "        fdate2 <- chron(max(tts1$chr, tts2$chr, msc1$chr, msc2$chr, msc3$chr,", 
"                na.rm = T))", "        numdate1 <- as.numeric(fdate1)", 
"        numdate2 <- as.numeric(fdate2)", "        sdate <- trunc(fdate1)", 
"        edate <- trunc(fdate2)", "        oldpar <- par(fig = c(0, 1, split, 1), mar = c(0, 4.1, 4.1, 4.1), mgp = c(1.75, 0.55, 0), tck = -0.012)", 
"        on.exit(par(oldpar))", "        samples <- sam[sam$chr >= fdate1 & sam$chr <= fdate2,  ]", 
"        # Express turbidity in SSC units for plotting", "        # Scaling will be based on the tts1 model", 
"        if(!missing(tts1)) {", "                if(tts1$type == \"logx\" || tts1$type == \"logxy\") {", 
"                        tts.ssc1 <- tts1$coef[1] + tts1$coef[2] * log(tts1$turb)", 
"                        tts.ssc2 <- tts1$coef[1] + tts1$coef[2] * log(tts2$turb)", 
"                        if(tts1$type == \"logxy\") {", "                                tts.ssc1 <- exp(tts.ssc1)", 
"                                tts.ssc2 <- exp(tts.ssc2)", 
"                        }", "                }", "                else if(tts1$type == \"sqrt\") {", 
"                        tts.ssc1 <- tts1$coef[1] + tts1$coef[2] * sqrt(tts1$turb)", 
"                        tts.ssc2 <- tts1$coef[1] + tts1$coef[2] * sqrt(tts2$turb)", 
"                        tts.ssc1 <- ifelse(tts.ssc1 > 0, tts.ssc1^2, -1)", 
"                        tts.ssc2 <- ifelse(tts.ssc2 > 0, tts.ssc2^2, -1)", 
"                }", "                else if(tts1$type == \"power\") {", 
"                        tts.ssc1 <- tts1$coef[1] * tts1$turb^tts1$coef[2]", 
"                        tts.ssc2 <- tts1$coef[1] * tts2$turb^tts1$coef[2]", 
"                }", "                else if(tts1$type == \"loess\") {", 
"                        tts.ssc1 <- approx(tts1$xy$x, tts1$xy$y, xout", 
"                                 = tts1$turb, rule = 2)$y", 
"                        if(missing(tts2))", "                                tts.ssc2 <- numeric(0)", 
"                        else tts.ssc2 <- approx(tts1$xy$x, tts1$xy$y, ", 
"                                xout = tts2$turb, rule = 2)$y", 
"                }", "                else {", "                        # type = \"linear\"", 
"                        tts.ssc1 <- tts1$coef[1] + tts1$coef[2] * tts1$turb", 
"                        tts.ssc2 <- tts1$coef[1] + tts1$coef[2] * tts2$turb", 
"                }", "        }", "        else {", "                tts.ssc1 <- tts.ssc2 <- NA", 
"        }", "        truessc <- samples$ssc", "        if(adj) truessc <- dis.adjust(stn,truessc)", 
"        maxssc <- max(msc1$predssc, msc2$predssc, truessc, max(tts.ssc1), max(tts.ssc2), na.rm = T)", 
"        # Plot the physical samples", "        if(length(truessc) > 1 || !is.na(truessc[1])) {", 
"                if(number) {", "                        plot(samples$chr, truessc, type = \"n\", xlim = c(", 
"                                numdate1, numdate2), ylim = c(0, maxssc), ", 
"                                axes = F, xlab = \"\", ylab = \"\",...)", 
"                                text(samples$chr, truessc, samples$bottle,", 
"                                        cex = 0.6, col = 6)", 
"                        }", "                else {", "                        plot(samples$chr, truessc, pch = 16, xlim = c(numdate1, numdate2), ", 
"                                ylim = c(0, maxssc), axes = F, xlab = \"\", ylab = \"\",...)", 
"                }", "        }", "        else {", "                #  No bottles, just set up plot", 
"                plot(0, 0, xlim = c(numdate1, numdate2), ylim = c(0, maxssc), ", 
"                        pch = 16, axes = F, xlab = \"\",ylab = \"\", ...)", 
"        }", "", "        # sets gridlines in first plot", "        if(!missing(grid1)) {", 
"            grid(nx = grid1, ny = grid1, col = \"lightgray\", lty = \"dotted\",", 
"            lwd = par(\"lwd\"), equilogs = TRUE)", "        }", 
"        axis.time(sdate, edate, side = 3, labels = F)", "        axis(side = 2, las = 2)", 
"        mtext(side = 2, line = 2.5, \"SSC (mg/l)\")", "        box()", 
"        indices <- NULL", "        # for legend labels and line types", 
"        if(!missing(tts1)) {", "                if(tts1$type == \"logxy\" || tts1$type == \"pairs\" || tts1$type == \"sqrt\" || (!is.null(tts1$bias) && tts1$bias==\"comp\")) {", 
"                        # Plot turbidity and estimated SSC with separate lines", 
"                        # unless they are nearly coincident.  They may not be", 
"                        # simply related because of complex bias corrections, or composite method was used, or", 
"                        # because ttspairs was used (type=\"pairs\"). If max difference", 
"                        # is less than 0.5% of the y range, use a single solid line", 
"                        yrange <- par()$usr[4] - par()$usr[3]", 
"                        if(max(abs(tts.ssc1 - tts1$predssc)) < 0.005 * yrange) {", 
"                                lines(tts1$chr, tts1$predssc, lty = 1, ...)", 
"                                indices <- 1", "                        }", 
"                        else {", "                                lines(tts1$chr, tts.ssc1, lty = 3, ...)", 
"                                lines(tts1$chr, tts1$predssc, lty = 2, ...)", 
"                                indices <- 2:3", "                        }", 
"                }", "                else {", "                        # Plot turb & est SSC as solid line", 
"                        # Plot negative estimates as dashed line at zero", 
"                        neg <- tts.ssc1 < 0", "                        tts.ssc1[neg] <- NA", 
"                        lines(tts1$chr, tts.ssc1, lty = 1, ...)", 
"                        zeroes <- rep(0, length(neg))", "                        zeroes[!neg] <- NA", 
"                        lines(tts1$chr, zeroes, lty = 2, ...)", 
"                        indices <- 1", "                }", 
"                if (missing(tts2)) {", "                        minturb <- min(tts1$turb, na.rm=T)", 
"                        maxturb <- max(tts1$turb, na.rm=T)", 
"                }", "                else {", "                        minturb <- min(tts1$turb, tts2$turb, na.rm = T)", 
"                        maxturb <- max(tts1$turb, tts2$turb, na.rm = T)", 
"                }", "                ticlabels <- pretty(c(minturb, maxturb))", 
"                # turb units", "                if(tts1$type == \"logx\" || tts1$type == \"logxy\") {", 
"                        ticlabels <- ticlabels[ticlabels > 0]", 
"                        ticlocs <- tts1$coef[1] + tts1$coef[2] * log(ticlabels)", 
"                        if(tts1$type == \"logxy\")", "                                ticlocs <- exp(ticlocs)", 
"                }", "                else if(tts1$type == \"sqrt\") {", 
"                        ticlabels <- ticlabels[ticlabels > 0]", 
"                        ticlocs <- tts1$coef[1] + tts1$coef[2] * sqrt(ticlabels)", 
"                        ticlocs <- ifelse(ticlocs >=0, ticlocs^2, NA)", 
"                }", "                else if(tts1$type == \"power\") {", 
"                        ticlabels <- ticlabels[ticlabels >= 0]", 
"                        ticlocs <- tts1$coef[1] * ticlabels^tts1$coef[2]", 
"                }", "                else if(tts1$type == \"loess\") {", 
"                        ticlabels <- ticlabels[ticlabels >= 0]", 
"                        ticlocs <- approx(tts1$xy$x, tts1$xy$y, ", 
"                                ticlabels, rule = 1)$y", "                }", 
"                else ticlocs <- tts1$coef[1] + tts1$coef[2] * ticlabels", 
"                axis(4, at = ticlocs, labels = ticlabels)", 
"                mtext(side = 4, line = 1.75, \"Turbidity (NTU)\")", 
"                if(!missing(tts2)) {", "                        if(extralinetype) {", 
"                                # Plot turb & est SSC separately", 
"                                lines(tts2$chr, tts.ssc2, lty = 3, ...)", 
"                                tts2.ssc3 <- tts2$predssc", 
"                                tts2.ssc3[tts2.ssc3 < 0] <- 0", 
"                                lines(tts2$chr, tts2.ssc3, lty = 2, ...)", 
"                                indices <- c(1, 3, 2)", "                        }", 
"                        else lines(tts2$chr, tts.ssc2, lty = 1, ...)", 
"                }", "        }", "        labels <- c(\"Turb & est SSC\", \"Est SSC only\", \"Turbidity only\",\"SSC sample\")", 
"        ltypes <- c(1, 2, 3, -1)", "        mrks <- c(-1, -1, -1, 4)", 
"        col <- c(1, 1, 1, 6)", "        if(!missing(msc1)) {", 
"                lines(msc1$chr, msc1$predssc, lty = 2, ...)", 
"                if(!extralinetype)", "                        indices <- union(indices, 2)", 
"        }", "        if(!missing(msc2))", "                lines(msc2$chr, msc2$predssc, lty = 2, ...)", 
"        if(!missing(msc3))", "                lines(msc3$chr, msc3$predssc, lty = 2, ...)", 
"        indices <- c(indices, 4)", "        legend(locator(1), labels[indices], lty = ltypes[indices],", 
"                pch = mrks[indices], cex = 0.6,", "                col = col[indices], ...)", 
"        mtext(paste(S, \":\", format(fdate1), \" - \", format(fdate2),", 
"                sep = \"\"), line = 1.5, cex = 1.2)", "        yhat <- sum(tts1$yhat, tts2$yhat, msc1$yhat, msc2$yhat, msc3$yhat,na.rm = T)", 
"        if(adj && (discoef[S,\"a\"] != 0 || discoef[S,\"b\"] != 1))", 
"                mtext(paste(\"DIS-adjusted load =\", round(yhat), \"kg\"),line=0.5,cex=0.8)", 
"        else ", "                mtext(paste(\"Estimated load =\", round(yhat), \"kg\"),line=0.5,cex=0.8)", 
"        par(new = T, fig = c(0, 1, 0, split), mar = c(5.1, 4.1, 0, 4.1))", 
"        pop <- pop[pop$chr >= fdate1 & pop$chr <= fdate2,  ]", 
"        if(units == \"cfs\")", "                qfactor <- 35.31467", 
"        else if(units == \"cumecs\")", "                qfactor <- 1", 
"        else stop(\"flow units must be cfs or cumecs\")", "        q <- pop$q/qfactor", 
"        plot(pop$chr, q, type = \"n\", axes = F, xlab = \"\", ylab = \"\")", 
"        lines(pop$chr, q, ...)", "        axis.time(sdate, edate, m = 2.2)", 
"        axis(side = 2, las = 2, tck = -0.012)", "        box()", 
"        mtext(side = 2, line = 2.5, \"Discharge (m3/sec)\")", 
"        if (!missing(grid2)) {", "           grid(nx = grid2, ny = grid2, col = \"lightgray\", lty = \"dotted\",", 
"           lwd = par(\"lwd\"), equilogs = TRUE)", "        } ", 
"        # Stage axis", "        stagetics <- get.stagetics(pop$q,stn,hy,nticks=4)", 
"        if (!is.null(stagetics)) {", "            axis(4, at = qcalc(stn, stagetics, hy)/qfactor, labels = stagetics)", 
"            mtext(\"Stage (ft)\", side = 4, line = 1.75)", "            # tests and sets gridlines in second plot ", 
"        }", "        yhat", "}"))
one <-
structure(function () 
par(mfrow = c(1, 1)), source = "function() par(mfrow=c(1,1))")
optimize.loess <-
structure(function (xvar, yvar, min, max, by = 0.05, degree = 1) 
{
    span <- seq(min, max, by)
    aic <- sapply(span, function(s, x, y, d) crit.loess(s, x, 
        y, d), x = xvar, y = yvar, d = degree)
    span[aic == min(aic)]
}, source = c("function(xvar, yvar, min, max, by = 0.05, degree = 1)", 
"{", "\t# Returns the span that results in lowest AIC for a loess model", 
"\t# Searches range of spans from min to max", "\tspan <- seq(min, max, by)", 
"\taic <- sapply(span, function(s, x, y, d)", "\tcrit.loess(s, x, y, d), x = xvar, y = yvar, d = degree)", 
"\tspan[aic == min(aic)]", "}"))
origin <-
structure(function (x) 
attr(x, "origin"), source = c("function (x) ", "attr(x, \"origin\")"
))
padstring <-
structure(function (string, len, char = " ", right = T, chop = T) 
{
    length <- nchar(string)
    if (right) 
        chopstring <- substring(string, 1, len)
    else chopstring <- substring(string, length - len + 1, length)
    if (chop) {
        string <- chopstring
    }
    diff <- len - nchar(chopstring)
    addstring <- sapply(diff, function(n) paste(rep(char, n), 
        collapse = ""))
    if (right) 
        paste(string, addstring, sep = "")
    else paste(addstring, string, sep = "")
}, source = c("function(string,len,char=\" \",right=T,chop=T) {", 
"# pads a vector of strings to n characters using char", "# n can be a single value or a vector of lengths", 
"# if chop=T, strings longer than n characters are chopped to size", 
"# if right=T, padding and chopping are on the right", "# if right=F, padding and chopping are on the left", 
"", " \tlength <- nchar(string)", " \tif (right) chopstring <- substring(string,1,len)", 
" \telse chopstring <- substring(string,length-len+1,length)", 
" \tif (chop) {", "\t   string <- chopstring", " \t}", " \tdiff <- len - nchar(chopstring)", 
" \taddstring <- sapply(diff, function(n) paste(rep(char, n),collapse=\"\"))", 
" \tif (right) paste(string,addstring,sep=\"\")", " \telse paste(addstring,string,sep=\"\")", 
"}"))
pairedTTest <-
structure(function () 
{
    initializeDialog(title = gettextRcmdr("Paired t-Test"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title = gettextRcmdr("First variable (pick one)"))
    yBox <- variableListBox(top, .numeric, title = gettextRcmdr("Second variable (pick one)"))
    onOK <- function() {
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall = pairedTTest, message = gettextRcmdr("You must select two variables."))
            return()
        }
        if (x == y) {
            errorCondition(recall = pairedTTest, message = gettextRcmdr("Variables must be different."))
            return()
        }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x, 
            ", ", .activeDataSet, "$", y, ", alternative='", 
            alternative, "', conf.level=", level, ", paired=TRUE)", 
            sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "t.test")
    radioButtons(top, name = "alternative", buttons = c("twosided", 
        "less", "greater"), values = c("two.sided", "less", "greater"), 
        labels = gettextRcmdr(c("Two-sided", "Difference < 0", 
            "Difference > 0")), title = gettextRcmdr("Alternative Hypothesis"))
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width = "6", 
        textvariable = confidenceLevel)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky = "nw")
    tkgrid(tklabel(confidenceFrame, text = gettextRcmdr("Confidence Level"), 
        fg = "blue"))
    tkgrid(confidenceField, sticky = "w")
    tkgrid(alternativeFrame, confidenceFrame, sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix(rows = 3, columns = 2)
}, source = c("function(){", "initializeDialog(title=gettextRcmdr(\"Paired t-Test\"))", 
".numeric <- Numeric()", "xBox <- variableListBox(top, .numeric,", 
"title=gettextRcmdr(\"First variable (pick one)\"))", "yBox <- variableListBox(top, .numeric,", 
"title=gettextRcmdr(\"Second variable (pick one)\"))", "onOK <- function(){", 
"x <- getSelection(xBox)", "y <- getSelection(yBox)", "if (length(x) == 0 | length(y) == 0){", 
"errorCondition(recall=pairedTTest,", "message=gettextRcmdr(\"You must select two variables.\"))", 
"return()", "}", "if (x == y){", "errorCondition(recall=pairedTTest,", 
"message=gettextRcmdr(\"Variables must be different.\"))", "return()", 
"}", "alternative <- as.character(tclvalue(alternativeVariable))", 
"level <- tclvalue(confidenceLevel)", "closeDialog()", ".activeDataSet <- ActiveDataSet()", 
"doItAndPrint(paste(\"t.test(\", .activeDataSet, \"$\", x, \", \",", 
".activeDataSet, \"$\", y,", "\", alternative='\", alternative, \"', conf.level=\", level,", 
"\", paired=TRUE)\", sep=\"\"))", "tkfocus(CommanderWindow())", 
"}", "OKCancelHelp(helpSubject=\"t.test\")", "radioButtons(top, name=\"alternative\",", 
"buttons=c(\"twosided\", \"less\", \"greater\"),", "values=c(\"two.sided\", \"less\", \"greater\"),", 
"labels=gettextRcmdr(c(\"Two-sided\", \"Difference < 0\",", "\"Difference > 0\")),", 
"title=gettextRcmdr(\"Alternative Hypothesis\"))", "confidenceFrame <- tkframe(top)", 
"confidenceLevel <- tclVar(\".95\")", "confidenceField <- tkentry(confidenceFrame, width=\"6\",", 
"textvariable=confidenceLevel)", "tkgrid(getFrame(xBox), getFrame(yBox), sticky=\"nw\")", 
"tkgrid(tklabel(confidenceFrame,", "text=gettextRcmdr(\"Confidence Level\"), fg=\"blue\"))", 
"tkgrid(confidenceField, sticky=\"w\")", "tkgrid(alternativeFrame, confidenceFrame, sticky=\"nw\")", 
"tkgrid(buttonsFrame, columnspan=2, sticky=\"w\")", "dialogSuffix(rows=3, columns=2)", 
"}"))
panelselectStnYear <-
structure(function () 
{
    updatevars <- function(panel) {
        with(panel, {
            stn <<- pars[1]
            hy <<- pars[2]
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control()
    init.stn <- ifelse(exists("stn", env = .GlobalEnv), stn, 
        "")
    init.hy <- ifelse(exists("hy", env = .GlobalEnv), hy, "")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    rp.button(panel, action = updatevars, title = "OK", quit = TRUE)
    rp.button(panel, action = nothing, title = "Cancel", quit = TRUE)
    rp.do(panel, updatevars)
}, source = c("function() {", "#  Problem with this is that you MUST press Enter before the", 
"#  new text is accepted", "   updatevars <- function(panel) {", 
"      with(panel, {", "         stn <<- pars[1]", "         hy <<- pars[2]", 
"      })", "   panel", "   }", "   nothing <- function(panel) panel", 
"   panel <- rp.control()", "   init.stn <- ifelse(exists(\"stn\", env=.GlobalEnv),stn,\"\")", 
"   init.hy <- ifelse(exists(\"hy\", env=.GlobalEnv),hy,\"\")", 
"   my.textentry(panel, pars, action = nothing, labels=c(\"Station\",\"Water year\"), ", 
"       title=\"Select station and year\",initval = c(init.stn, init.hy))", 
"   rp.button(panel, action = updatevars, title=\"OK\", quit=TRUE)", 
"   rp.button(panel, action = nothing, title=\"Cancel\", quit=TRUE)", 
"   rp.do(panel,updatevars)", "}"))
parse.format <-
structure(function (format, year.abb = getOption("chron.year.abb"), 
    ...) 
{
    abb <- TRUE
    mon.abb <- FALSE
    if (is.null(year.abb)) 
        year.abb <- TRUE
    if ((nf <- nchar(format)) == 5) {
        sep <- substring(format, 2, 2)
        fmt <- substring(format, first = c(1, 3, 5), last = c(1, 
            3, 5))
    }
    else if (nf == 3) {
        sep <- ""
        fmt <- substring(format, first = 1:3, last = 1:3)
    }
    else {
        abb <- FALSE
        sep <- " "
        fmt <- unlist(unpaste(format, sep = sep))
        mon.abb <- if (any(fmt == "month")) 
            FALSE
        else TRUE
    }
    periods <- substring(tolower(fmt), 1, 1)
    return(list(abb = abb, sep = sep, periods = periods, mon.abb = mon.abb, 
        year.abb = year.abb))
}, source = c("function (format, year.abb = getOption(\"chron.year.abb\"), ...) ", 
"{", "    abb <- TRUE", "    mon.abb <- FALSE", "    if (is.null(year.abb)) ", 
"        year.abb <- TRUE", "    if ((nf <- nchar(format)) == 5) {", 
"        sep <- substring(format, 2, 2)", "        fmt <- substring(format, first = c(1, 3, 5), last = c(1, ", 
"            3, 5))", "    }", "    else if (nf == 3) {", "        sep <- \"\"", 
"        fmt <- substring(format, first = 1:3, last = 1:3)", 
"    }", "    else {", "        abb <- FALSE", "        sep <- \" \"", 
"        fmt <- unlist(unpaste(format, sep = sep))", "        mon.abb <- if (any(fmt == \"month\")) ", 
"            FALSE", "        else TRUE", "    }", "    periods <- substring(tolower(fmt), 1, 1)", 
"    return(list(abb = abb, sep = sep, periods = periods, mon.abb = mon.abb, ", 
"        year.abb = year.abb))", "}"))
parseDate <-
structure(function (str) 
{
    dt <- unlist(strsplit(str, " "))
    date <- dt[1]
    time <- dt[2]
    if (nchar(time) == 5) 
        time <- paste(time, "00", sep = ":")
    chr <- suppressWarnings(try(chron(date, time), silent = TRUE))
    if (inherits(chr, "try-error")) 
        stop(paste("Invalid date format:", str))
    else return(chr)
}, source = c("function(str) {", "    dt <- unlist(strsplit(str, \" \"))", 
"    date <- dt[1]", "    time <- dt[2]", "    if(nchar(time) == 5) time <- paste(time,\"00\",sep=\":\")", 
"    chr <- suppressWarnings(try(chron(date,time),silent=TRUE))", 
"    if (inherits(chr,\"try-error\")) stop(paste(\"Invalid date format:\",str))", 
"    else return(chr)", "}"))
pdi.append <-
structure(function (stn, hy, ttshome = ".", outpath = "K:/water/caspar/website_image/sediment/samples") 
{
    hy <- zfill(hy, 2)
    disfile <- paste(stn, hy, ".dis", sep = "")
    rawdir <- paste("raw", hy, sep = "")
    full.name <- paste(ttshome, stn, rawdir, disfile, sep = "/")
    if (!file.exists(full.name)) {
        cat("No data to append.", disfile, "does not exist.\n")
        return(invisible())
    }
    else {
        cat("Reading", full.name, "\n")
    }
    columns <- c("year", "mo", "dy", "DUMP", "BOT", "ssc", "labcodes")
    disdata <- read.csv(full.name, col.names = columns, header = F)
    sedfile <- paste(stn, "sed", hy, ".txt", sep = "")
    sedpath <- paste(outpath, stn, sep = "/")
    sed.name <- paste(sedpath, sedfile, sep = "/")
    if (!file.exists(sed.name)) {
        cat(sed.name, "does not exist.\nYou may need to:\n", 
            "(1) run write.sed(), or\n", "(2) rename sed file as", 
            sedfile, ", or\n", "(3) move", sedfile, "to", sedpath, 
            "\n")
        return(invisible())
    }
    else {
        cat("Reading", sed.name, "\n")
    }
    seddata <- read.table(sed.name, header = T)
    seddata.rec <- scan(sed.name, what = "", sep = ",", skip = 1)
    seddata2 <- data.frame(DUMP = seddata$DUMP, BOT = seddata$BOT, 
        REC = seddata.rec)
    mergedata <- merge(seddata2, disdata, by = c("DUMP", "BOT"))
    nmatches <- dim(mergedata)[1]
    if (nmatches == 0) {
        cat("No matching bottles found\n")
        return(invisible())
    }
    else {
        cat(nmatches, "matches found\n")
    }
    mergedata <- mergedata[order(mergedata$DUMP, mergedata$BOT), 
        ]
    dssc <- scientific(mergedata$ssc, 3, 2)
    dcodes <- zfill(mergedata$labcodes, 2)
    outdata <- data.frame(mergedata$REC, dssc, dcodes)
    pdifile <- paste(stn, "pdi.txt", sep = "")
    pdi.name <- paste(sedpath, pdifile, sep = "/")
    bckfile <- paste(stn, "pdi.bck", sep = "")
    bck.name <- paste(sedpath, bckfile, sep = "/")
    if (file.exists(pdi.name)) {
        if (file.exists(bck.name)) 
            cat("Replacing backup", bckfile, "\n")
        else cat("Creating backup", bckfile, "\n")
        file.copy(pdi.name, bck.name, overwrite = TRUE)
        pdidata <- read.table(pdi.name, header = T)
        yr <- as.numeric(substring(pdidata$DATE, 9, 10))
        mo <- as.numeric(substring(pdidata$DATE, 1, 2))
        wy <- ifelse(mo > 7, yr + 1, yr)
        nfound <- sum(wy == as.numeric(hy))
        if (nfound == 0) 
            cat("Appending to", pdi.name, "\n")
        else {
            message <- paste(pdifile, "already contains", nfound, 
                "records from", "water year", hy, ". Do you want to replace them (y/n)? ")
            ans <- readline(message)
            if (tolower(ans) == "y") {
                pdidata2 <- scan(pdi.name, what = "", sep = ",")
                oldpdi <- pdidata2[wy < as.numeric(hy)]
                write.table(oldpdi, pdi.name, col.names = F, 
                  row.names = F, quote = F, append = F)
                cat("Reappending to", pdi.name, "\n")
            }
            else {
                cat("Data not appended\n")
                return(invisible())
            }
        }
        write.table(outdata, pdi.name, col.names = F, row.names = F, 
            quote = F, append = T)
    }
    else {
        ans <- readline(paste(pdi.name, " not found.\nCreate it (y/n)? "))
        if (tolower(ans) == "y") {
            write.table(outdata, pdi.name, col.names = F, row.names = F, 
                quote = F, append = F)
            cat("Created", pdi.name, "\n")
        }
        else {
            cat("Data not appended\n")
            return(invisible())
        }
    }
    outdata
}, source = c("function(stn,hy,ttshome=\".\", outpath=\"K:/water/caspar/website_image/sediment/samples\") {", 
"    # Function for merging DI concentrations into sed files created by write.sed", 
"    #    and appending the result to the \"PDI\" files in the same location", 
"    # Reads the sed file from outpath location", "    # Merges with this year's DI samples, if any, from ttshome", 
"    # Backs up the pdi file in the outpath location", "    # Appends output to pdi file in outpath location", 
"", "    # First read the disfile", "        hy <- zfill(hy,2)", 
"        disfile <- paste(stn, hy, \".dis\", sep = \"\")", "        rawdir <- paste(\"raw\", hy, sep=\"\")", 
"        full.name <- paste(ttshome, stn, rawdir, disfile, sep=\"/\")", 
"        if (!file.exists(full.name)) {", "             cat(\"No data to append.\",disfile,\"does not exist.\\n\")", 
"             return(invisible())", "        }", "        else {", 
"             cat(\"Reading\",full.name,\"\\n\")", "        }", 
"        columns <- c(\"year\",\"mo\",\"dy\",\"DUMP\",\"BOT\",\"ssc\",\"labcodes\")", 
"        disdata <- read.csv(full.name,col.names=columns,header=F)", 
"", "    # Look for this year's sed file in outpath location", 
"        sedfile <- paste(stn,\"sed\",hy,\".txt\",sep=\"\")", 
"        sedpath <- paste(outpath,stn,sep=\"/\")", "        sed.name <- paste(sedpath,sedfile,sep=\"/\")", 
"        if (!file.exists(sed.name)) {", "             cat(sed.name,\"does not exist.\\nYou may need to:\\n\",", 
"                 \"(1) run write.sed(), or\\n\",", "                 \"(2) rename sed file as\",sedfile,\", or\\n\",", 
"                 \"(3) move\",sedfile,\"to\",sedpath,\"\\n\")", 
"             return(invisible())", "        }", "        else {", 
"             cat(\"Reading\",sed.name,\"\\n\")", "        }", 
"", "    # Read sed data first just to get dumps and bottle numbers", 
"        seddata <- read.table(sed.name,header=T)", "    # Re-read entire sed records for exact copying to pdi file", 
"        seddata.rec <- scan(sed.name,what=\"\",sep=\",\",skip=1)", 
"        seddata2 <- data.frame(DUMP=seddata$DUMP, BOT=seddata$BOT,REC=seddata.rec)", 
"", "    # Merge the data", "        mergedata <- merge(seddata2,disdata,by=c(\"DUMP\",\"BOT\"))", 
"        nmatches <- dim(mergedata)[1]", "        if (nmatches == 0) {", 
"             cat(\"No matching bottles found\\n\")", "             return(invisible())", 
"        }", "        else {", "             cat(nmatches,\"matches found\\n\")", 
"        }", "", "    # Sort and format data for output", "        mergedata <- mergedata[order(mergedata$DUMP,mergedata$BOT),]", 
"        dssc <- scientific(mergedata$ssc,3,2)", "        dcodes <- zfill(mergedata$labcodes,2)", 
"        outdata <- data.frame(mergedata$REC,dssc,dcodes)", "", 
"    # Find pdi file, back it up, and append to it", "        pdifile <- paste(stn,\"pdi.txt\",sep=\"\")", 
"        pdi.name <- paste(sedpath,pdifile,sep=\"/\")", "        bckfile <- paste(stn,\"pdi.bck\",sep=\"\")", 
"        bck.name <- paste(sedpath,bckfile,sep=\"/\")", "", "        if (file.exists(pdi.name)) {", 
"           if (file.exists(bck.name))", "               cat(\"Replacing backup\",bckfile,\"\\n\")", 
"           else", "               cat(\"Creating backup\",bckfile,\"\\n\")                 ", 
"           file.copy(pdi.name,bck.name,overwrite=TRUE)", "", 
"           # Check to see any records from this year have already been appended", 
"           # If so, give user the option to re-append", "           pdidata <- read.table(pdi.name,header=T)", 
"           yr <- as.numeric(substring(pdidata$DATE,9,10))", 
"           mo <- as.numeric(substring(pdidata$DATE,1,2))", "           wy <- ifelse(mo>7,yr+1,yr)", 
"           nfound <- sum(wy==as.numeric(hy))", "           if (nfound == 0)", 
"               cat(\"Appending to\",pdi.name,\"\\n\")", "           else {", 
"               message <- paste(pdifile,\"already contains\",nfound,\"records from\",", 
"                  \"water year\",hy,\". Do you want to replace them (y/n)? \")", 
"               ans <- readline(message)", "               if (tolower(ans) == \"y\") {", 
"                   # Replace the old PDI file with just prior years' data     ", 
"                   pdidata2 <- scan(pdi.name,what=\"\",sep=\",\")", 
"                   oldpdi <- pdidata2[wy < as.numeric(hy)]    ", 
"                   write.table(oldpdi,pdi.name,col.names=F,row.names=F,quote=F,append=F)", 
"                   cat(\"Reappending to\",pdi.name,\"\\n\")", 
"               }", "               else {", "                    cat(\"Data not appended\\n\")", 
"                    return(invisible())", "               }", 
"           } ", "           write.table(outdata, pdi.name,col.names=F,row.names=F,quote=F,append=T)", 
"        }", "        else { ", "           ans <- readline(paste(pdi.name,\" not found.\\nCreate it (y/n)? \"))", 
"           if (tolower(ans) == \"y\") {", "               write.table(outdata, pdi.name,col.names=F,row.names=F,quote=F,append=F)", 
"               cat(\"Created\",pdi.name,\"\\n\")", "           }", 
"           else {", "               cat(\"Data not appended\\n\")", 
"               return(invisible())", "           }", "        }", 
"        outdata", "}"))
pickone <-
structure(function (values, whatitis = "value") 
{
    while (TRUE) {
        cat("Choose a", whatitis, "from the following list:\n")
        for (i in seq(along = values)) {
            cat(i, ":", values[i], "\n")
        }
        choice <- readline("Enter the number corresponding to your choice: ")
        if (choice %in% seq(along = values)) 
            break
    }
    as.numeric(choice)
}, source = c("function(values,whatitis=\"value\") {", "# User is presented with a list of values to choose from", 
"# whatitis is a character string identifying what is being chose", 
"# e.g. pickone(c(\"apple\",\"orange\",\"banana\"), \"fruit\")", 
"  while (TRUE) {", "     cat(\"Choose a\",whatitis,\"from the following list:\\n\")", 
"     for (i in seq(along=values)) {", "        cat(i,\":\",values[i],\"\\n\")", 
"     }", "     choice <- readline(\"Enter the number corresponding to your choice: \")", 
"     if (choice %in% seq(along=values)) break", "  }", "  as.numeric(choice)", 
"}"))
plot.or <-
structure(function (stn, hy, sdate, stime, edate, etime, dumps, 
    xaxis = "time", cor = F, reread = T, linear = F, span = 2/3, 
    degree = 1, choose = F, ...) 
{
    hy <- zfill(hy, 2)
    orname <- paste(stn, hy, ".or", sep = "")
    if (exists(orname) && (reread == F)) 
        ordata <- eval(as.name(orname))
    else ordata <- read.or(stn, hy, choose.staff = choose)
    assign(orname, ordata, env = .GlobalEnv)
    if (!missing(dumps)) {
        subdata <- ordata[ordata$dump %in% dumps, ]
        dumpeval <- paste(unique(eval(match.call()$dumps)), collapse = ",")
        subtitle <- paste("HY", hy, " dumps ", dumpeval, sep = "")
    }
    else if (missing(sdate) | missing(edate)) {
        subdata <- ordata
        subtitle <- paste("HY", hy, sep = "")
    }
    else {
        if (missing(stime)) 
            stime <- 0
        if (missing(etime)) 
            etime <- 2400
        subdata <- subtime(ordata, sdate, stime, edate, etime)
        subtitle <- paste(zfill(sdate, 6), ":", zfill(stime, 
            4), " - ", zfill(edate, 6), ":", zfill(etime, 4), 
            sep = "")
    }
    if (!linear && dim(subdata)[1] < 8) {
        print("Forcing linear trend (need at least 8 points for loess)")
        linear <- T
    }
    attach(subdata)
    on.exit(detach(subdata))
    if (cor) {
        elecstg <- corstg
        xlab <- "Corrected electronic stage"
        ylab <- "Staff reading minus corrected elec stage"
    }
    else {
        elecstg <- rawstg
        xlab <- "Raw electronic stage"
        ylab <- "Staff reading minus raw elec stage"
    }
    diff <- staff - elecstg
    if (xaxis == "time") {
        if (linear) {
            plot(chr, diff, axes = F, xlab = "", ylab = ylab, 
                ...)
            fit <- lm(diff ~ chr, na.action = na.omit)
        }
        else {
            scatter.smooth(chr, diff, axes = F, xlab = "", ylab = ylab, 
                span = span, degree = degree, ...)
            fit <- loess(diff ~ chr, span = span, degree = degree, 
                family = "symmetric")
        }
        axis.time(chr)
        axis(2)
        box()
    }
    else if (xaxis == "stage") {
        if (linear) {
            plot(elecstg, diff, xlab = xlab, ylab = ylab, ...)
            fit <- lm(diff ~ elecstg, na.action = na.omit)
        }
        else {
            scatter.smooth(elecstg, diff, xlab = xlab, ylab = ylab, 
                span = span, degree = degree, ...)
            fit <- loess(diff ~ elecstg, span = span, degree = degree)
        }
    }
    if (linear) 
        abline(fit)
    abline(0, 0, lty = 8)
    title(paste("Station ", toupper(stn), ": ", subtitle, sep = ""), 
        cex.main = 1)
    legend(locator(1), c("trendline", "y = 0"), lty = c(1, 8))
    summary(fit)
}, source = c("function(stn, hy, sdate, stime, edate, etime, dumps, xaxis=\"time\", cor=F, reread=T, linear=F, span=2/3, degree=1, choose=F, ...) {", 
"# Plots OR difference against time or stage", "# Adds zero reference line and trendline (loess or regression)", 
"# Returns summary stats from loess or linear regression", "# Data can be limited to specific start and end dates or dumps", 
"# Date, time, and dump arguments are optional", "# If xaxis=\"time\" plots diff vs time", 
"# If xaxis=\"stage\" plots diff vs (electronic) stage", "# If cor=T, gets the latest corrected stages from the flo file and ", 
"#    uses corrected stage in place of raw stage.", "# Saves an object to the working environment containing the OR data,", 
"#    and names it using the stnhy.or template,  e.g. ftr06.or", 
"# If an OR object exists and reread=F, then the FLO file won't be read anew.", 
"#    This saves time, but ignores any changes made by TTS Adjuster", 
"#    after the last update of the OR object (stnhy.or)", "# If linear=T, adds a regression line instead of loess curve", 
"#    and reports regression stats and significance", "# If linear=F, smoothness can be altered using span and degree", 
"# Will not try loess however without at least 8 data points", 
"# If choose=T, user can choose a staff plate from stationinfo.txt", 
"# If additional args are supplied, they will be passed to plot() ", 
"   hy <- zfill(hy,2)", "   orname <- paste(stn, hy, \".or\", sep=\"\")", 
"   if (exists(orname) && (reread==F))", "      ordata <- eval(as.name(orname))", 
"   else", "      ordata <- read.or(stn, hy, choose.staff=choose)", 
"   assign(orname, ordata, env=.GlobalEnv)", "   if(!missing(dumps)) {", 
"      subdata <- ordata[ordata$dump %in% dumps, ]", "      dumpeval <- paste(unique(eval(match.call()$dumps)), collapse = \",\")", 
"      subtitle <- paste(\"HY\",hy,\" dumps \", dumpeval, sep=\"\")", 
"   }", "   else if(missing(sdate) | missing(edate)) {", "      subdata <- ordata", 
"      subtitle <- paste(\"HY\",hy,sep=\"\")", "   }", "   else {", 
"      if(missing(stime)) stime <- 0", "      if(missing(etime)) etime <- 2400", 
"      subdata <- subtime(ordata, sdate, stime, edate, etime)", 
"      subtitle <- paste(zfill(sdate,6), \":\", zfill(stime, 4), \" - \", ", 
"                        zfill(edate,6), \":\", zfill(etime, 4),sep=\"\")", 
"   }", "   if (!linear && dim(subdata)[1] < 8) {", "      print(\"Forcing linear trend (need at least 8 points for loess)\")", 
"      linear <- T", "   }", "   attach(subdata)", "   on.exit(detach(subdata))", 
"   if (cor) {", "      elecstg <- corstg", "      xlab <- \"Corrected electronic stage\"", 
"      ylab <- \"Staff reading minus corrected elec stage\"", 
"   }", "   else {", "      elecstg <- rawstg", "      xlab <- \"Raw electronic stage\"", 
"      ylab <- \"Staff reading minus raw elec stage\"", "   }", 
"   diff <- staff - elecstg", "   if (xaxis==\"time\") {", "      if (linear) {", 
"          plot(chr, diff, axes=F, xlab=\"\", ylab=ylab,...)", 
"          fit <- lm(diff ~ chr, na.action=na.omit)", "      }", 
"      else {", "          scatter.smooth(chr, diff, axes=F, xlab=\"\", ylab=ylab, span=span, ", 
"degree=degree, ...)", "          fit <- loess(diff ~ chr, span=span, degree=degree, ", 
"family=\"symmetric\")", "      }", "      axis.time(chr)", "      axis(2)", 
"      box() ", "   }", "   else if (xaxis==\"stage\") {", "      if (linear) {", 
"          plot(elecstg, diff, xlab=xlab, ylab=ylab,...)", "          fit <- lm(diff ~ elecstg, na.action=na.omit)", 
"      }", "      else {", "          scatter.smooth(elecstg, diff, xlab=xlab, ylab=ylab, span=span, ", 
"degree=degree, ...)", "          fit <- loess(diff ~ elecstg, span=span, degree=degree)", 
"      }", "   }", "   if (linear) abline(fit)", "   abline(0,0,lty=8)", 
"   title(paste(\"Station \", toupper(stn), \": \", subtitle, sep=\"\"), cex.main=1)", 
"   legend(locator(1),c(\"trendline\",\"y = 0\"),lty=c(1,8))", 
"   summary(fit)", "}"))
plot.sim <-
structure(function (df, simout, sdate, stime, edate, etime, ylim, 
    title = "", numbers = F) 
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
}, source = c("function(df, simout, sdate, stime, edate, etime, ylim, title = \"\", numbers=F)", 
"{", "#       Revised 051104 J.Lewis   added numbers argument; added legend", 
"#         Previous versions did not plot symbols for sample code = 5", 
"#\tRevised 041027 J.Lewis   plots data above minimum stage in red", 
"#\t\t(minimum stage is obtained as an attribute of simout)", 
"#\t  Plots a TTS sampling simulation", "#\t  df = data frame created by read.campbell() or read.flo() function", 
"# \t  simout = object created by tts.sample() simulation function", 
"#\t  sdate = optional start date (yymmdd)", "#\t  stime = optional start time (hhmm)", 
"# \t  edate = optional end date (yymmdd)", "#\t  etime = optional end time (hhmm)", 
"# \t  ylim = optional ylimits, vector of length 2, e.g. c(0,500)", 
"#\t  title = optional title for plot in quotes", "#         numbers = T or F; if T then show bottle numbers; but can be VERY slow", 
"        if(missing(sdate)) {", "                schron <- min(df$chr)", 
"                sdate <- trunc(schron)", "                stime <- msm2mt(1440 * (schron - sdate))", 
"        }", "        else {", "                if(missing(stime))", 
"                        stime <- 0", "                schron <- make.chron(sdate, stime)", 
"                # fractional date", "                sdate <- trunc(schron)", 
"        }", "        if(missing(edate)) {", "                echron <- max(df$chr)", 
"                edate <- trunc(echron)", "                etime <- msm2mt(1440 * (echron - edate))", 
"        }", "        else {", "                if(missing(etime))", 
"                        etime <- 2400", "                echron <- make.chron(edate, etime)", 
"                # fractional date", "                edate <- trunc(echron)", 
"        }", "\trow.names(df) <- 1:dim(df)[1]", "        df <- df[df$chr >= schron & df$chr <= echron,  ]", 
"        nam <- as.numeric(row.names(df))", "        attach(df)", 
"        on.exit(detach(2))", "        if (missing(ylim)) ylim <- c(0,max(turb))", 
"        plot(chr, turb, xlab = \"\", axes = F, ylim=ylim, type = \"l\")", 
"\tturb2 <- turb", "\tturb2[stg<attr(simout,\"minstg\")] <- NA", 
"\tlines(chr, turb2, col=2)", "        axis(2)", "        axis.time(chr)", 
"        box()", "        t <- simout", "\tset0 <- which(nam %in% t[, 1])", 
"        set1 <- which(nam %in% t[t[, 2] == 1, 1])", "        set2 <- which(nam %in% t[t[, 2] == 2, 1])", 
"        set4 <- which(nam %in% t[t[, 2] == 4, 1])", "\tset5 <- which(nam %in% t[t[, 2] == 5, 1])", 
"        if(length(set1) > 0)", "            points(chr[set1], turb[set1], pch = 2, cex = 0.8)", 
"        if(length(set2) > 0)", "            points(chr[set2], turb[set2], pch = 6, cex = 0.8)", 
"        if(length(set4) > 0)", "            points(chr[set4], turb[set4], pch = 1, cex = 0.8)", 
"        if(length(set5) > 0)", "            points(chr[set5], turb[set5], pch = 16, cex = 0.8)", 
"        if(numbers && length(set0) > 0) {", "            dx <- 0.015 * (par()$usr[2.] - par()$usr[1.])", 
"\t    botnum <- which(t[, 1] %in% nam)", "            text(chr[set0] + dx, turb[set0], botnum, cex = 0.7)", 
"\t}", "\tusr <- par()$usr", "\tx0 <- .87*usr[2] + .13*usr[1]", 
"\ty0 <- .985*usr[4] + .015*usr[3]", "\tlegend(x0,y0,c(\"rising\",\"falling\",\"startup\",\"fixtime\"),pch=c(2,6,1,16),cex=0.75)", 
"        title(title)", "}"))
plotf <-
structure(function (panel) 
{
    with(panel, {
        pars <- as.numeric(pars)
        xgrid <- seq(0.1, max(c(pars[3], 5), na.rm = TRUE), length = 50)
        dgrid <- df(xgrid, pars[1], pars[2])
        plot(xgrid, dgrid, type = "l", col = "blue", lwd = 3)
        if (!is.na(pars[3])) {
            lines(rep(pars[3], 2), c(0, 0.95 * max(dgrid)), lty = 2, 
                col = "red")
            text(pars[3], max(dgrid), as.character(pars[3]), 
                col = "red")
        }
    })
    panel
}, source = c("function(panel) {", "      with(panel, {", " pars   <- as.numeric(pars)", 
"         xgrid <- seq(0.1, max(c(pars[3], 5), na.rm = TRUE), length = 50)", 
"         dgrid <- df(xgrid, pars[1], pars[2])", "         plot(xgrid, dgrid, type = \"l\", col = \"blue\", lwd = 3)", 
"         if (!is.na(pars[3])) {", "            lines(rep(pars[3], 2), c(0, 0.95 * max(dgrid)), lty = 2, col = \"red\")", 
"            text(pars[3], max(dgrid), as.character(pars[3]), col = \"red\")", 
"            }", "         })", "      panel", "      }"))
predict.loglog <-
structure(function (x, y, xnew, ...) 
{
    fit <- lm(log(y) ~ log(x), na.action = na.omit, ...)
    pred <- predict(fit, newdata = data.frame(x = xnew))
    sigma <- summary(fit)$sigma
    if (is.na(sigma)) 
        exp(pred)
    else exp(0.5 * sigma^2 + pred)
}, source = c("function(x, y, xnew, ...)", "{", "\t# Fits a linear model in the logs of x and y", 
"\t# Returns \"qmle\" bias-corrected predictions for xnew", "\tfit <- lm(log(y) ~ log(x), na.action = na.omit, ...)", 
"\tpred <- predict(fit, newdata = data.frame(x = xnew))", "\tsigma <- summary(fit)$sigma", 
"\tif(is.na(sigma))", "\t\texp(pred)", "\telse exp(0.5 * sigma^2. + pred)", 
"}"))
predict.nls <-
structure(function (object, newdata, se.fit = FALSE, scale = NULL, 
    df = Inf, interval = c("none", "confidence", "prediction"), 
    level = 0.95, ...) 
{
    if (missing(newdata)) 
        return(as.vector(fitted(object)))
    object$m$predict(newdata)
}, source = c("function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, ", 
"    interval = c(\"none\", \"confidence\", \"prediction\"), level = 0.95, ", 
"    ...) ", "{", "    if (missing(newdata)) ", "        return(as.vector(fitted(object)))", 
"    object$m$predict(newdata)", "}"))
predict.simple <-
structure(function (x, y, xnew) 
{
    fit <- lm(y ~ x)
    predict(fit, newdata = data.frame(x = xnew))
}, source = c("function(x, y, xnew)", "{", "\t# Fits a linear model y ~ x", 
"\t# Returns predictions for xnew", "\tfit <- lm(y ~ x)", "\tpredict(fit, newdata = data.frame(x = xnew))", 
"}"))
pwr.t2n.test <-
structure(function (n1 = NULL, n2 = NULL, d = NULL, sig.level = 0.05, 
    power = NULL, alternative = c("two.sided", "less", "greater")) 
{
    if (sum(sapply(list(n1, n2, d, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of n1, n2, d, power, and sig.level must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop(sQuote("sig.level"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power | 
        power > 1)) 
        stop(sQuote("power"), " must be numeric in [0, 1]")
    if (!is.null(n1) && n1 < 2) 
        stop("number of observations in the first group must be at least 2")
    if (!is.null(n2) && n2 < 2) 
        stop("number of observations in the second group must be at least 2")
    alternative <- match.arg(alternative)
    tsample <- 2
    ttside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
    tside <- switch(alternative, less = 1, two.sided = 2, greater = 1)
    if (tside == 2 && !is.null(d)) 
        d <- abs(d)
    if (ttside == 1) {
        p.body <- quote({
            nu <- n1 + n2 - 2
            pt(qt(sig.level/tside, nu, lower = TRUE), nu, ncp = d * 
                (1/sqrt(1/n1 + 1/n2)), lower = TRUE)
        })
    }
    if (ttside == 2) {
        p.body <- quote({
            nu <- n1 + n2 - 2
            qu <- qt(sig.level/tside, nu, lower = FALSE)
            pt(qu, nu, ncp = d * (1/sqrt(1/n1 + 1/n2)), lower = FALSE) + 
                pt(-qu, nu, ncp = d * (1/sqrt(1/n1 + 1/n2)), 
                  lower = TRUE)
        })
    }
    if (ttside == 3) {
        p.body <- quote({
            nu <- n1 + n2 - 2
            pt(qt(sig.level/tside, nu, lower = FALSE), nu, ncp = d * 
                (1/sqrt(1/n1 + 1/n2)), lower = FALSE)
        })
    }
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(n1)) 
        n1 <- uniroot(function(n1) eval(p.body) - power, c(2 + 
            1e-10, 1e+07))$root
    else if (is.null(n2)) 
        n2 <- uniroot(function(n2) eval(p.body) - power, c(2 + 
            1e-10, 1e+07))$root
    else if (is.null(d)) {
        if (ttside == 2) {
            d <- uniroot(function(d) eval(p.body) - power, c(1e-07, 
                10))$root
        }
        if (ttside == 1) {
            d <- uniroot(function(d) eval(p.body) - power, c(-10, 
                5))$root
        }
        if (ttside == 3) {
            d <- uniroot(function(d) eval(p.body) - power, c(-5, 
                10))$root
        }
    }
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    METHOD <- c("t test power calculation")
    structure(list(n1 = n1, n2 = n2, d = d, sig.level = sig.level, 
        power = power, alternative = alternative, method = METHOD), 
        class = "power.htest")
}, source = c("function (n1 = NULL, n2= NULL, d = NULL, sig.level = 0.05, power = NULL,", 
" alternative = c(\"two.sided\", ", "        \"less\",\"greater\")) ", 
"{", "    if (sum(sapply(list(n1,n2, d, power, sig.level), is.null)) != ", 
"        1) ", "        stop(\"exactly one of n1, n2, d, power, and sig.level must be NULL\")", 
"    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > ", 
"        sig.level | sig.level > 1)) ", "        stop(sQuote(\"sig.level\"), \" must be numeric in [0, 1]\")", 
"    if (!is.null(power) && !is.numeric(power) || any(0 > power | ", 
"        power > 1)) ", "        stop(sQuote(\"power\"), \" must be numeric in [0, 1]\")", 
"   if (!is.null(n1) && n1 < 2) ", "        stop(\"number of observations in the first group must be at least 2\")", 
"     if (!is.null(n2) && n2 < 2) ", "        stop(\"number of observations in the second group must be at least 2\")", 
"  ", "    alternative <- match.arg(alternative)", "    tsample <-2", 
"ttside<-switch(alternative, less = 1, two.sided = 2, greater=3)", 
"", "    tside <- switch(alternative, less = 1, two.sided = 2, greater =1)", 
"    if (tside==2 &&  !is.null(d)) ", "        d <- abs(d)", 
"    if (ttside == 1) {", "        p.body <- quote({", "            nu <- n1+n2-2", 
"            pt(qt(sig.level/tside, nu, lower = TRUE), nu, ", 
"ncp = d*(1/sqrt(1/n1+1/n2)),", " lower = TRUE)", "        })", 
"    }", "    if (ttside == 2)  {", "        p.body <- quote({", 
"           nu <- n1+n2-2", "            qu <- qt(sig.level/tside, nu, lower = FALSE)", 
"            pt(qu, nu, ncp = d*(1/sqrt(1/n1+1/n2)), lower = FALSE) + ", 
"                pt(-qu, nu,ncp = d*(1/sqrt(1/n1+1/n2)), lower = TRUE)", 
"        })", "    }", "if (ttside == 3) {", "        p.body <- quote({", 
"           nu <- n1+n2-2", "            pt(qt(sig.level/tside, nu, lower = FALSE), nu, ", 
"ncp = d*(1/sqrt(1/n1+1/n2)), lower = FALSE)", "        })", 
"    }", "", "    if (is.null(power)) ", "        power <- eval(p.body)", 
"    else if (is.null(n1)) ", "        n1 <- uniroot(function(n1) eval(p.body) - power, c(2 + ", 
"            1e-10, 1e+07))$root", "  else if (is.null(n2)) ", 
"        n2 <- uniroot(function(n2) eval(p.body) - power, c(2 + ", 
"            1e-10, 1e+07))$root", "    else if (is.null(d)) {", 
" if(ttside==2){       d <- uniroot(function(d) eval(p.body) - power, ", 
"c(1e-07, 10))$root}", "if(ttside==1){       d <- uniroot(function(d) eval(p.body) - power, ", 
"c(-10, 5))$root}", "if(ttside==3){       d <- uniroot(function(d) eval(p.body) - power, ", 
"c(-5, 10))$root}", "", "}", "    else if (is.null(sig.level)) ", 
"        sig.level <- uniroot(function(sig.level) eval(p.body) - ", 
"            power, c(1e-10, 1 - 1e-10))$root", "    else stop(\"internal error\")", 
"      METHOD <- c(\"t test power calculation\")", "    structure(list(n1 = n1,n2=n2, d = d, sig.level = sig.level, power = power, ", 
"        alternative = alternative,method = METHOD), ", "        class = \"power.htest\")", 
"}"))
qcalc <-
structure(function (stn, stg, hy) 
{
    if (stn == "nfc" || stn == "sfc") 
        weircalc(stg)
    else if (stn == "arf") 
        arfcalc(stg)
    else if (stn == "que") 
        quecalc(stg)
    else if (stn == "ftr") 
        ftrcalc(stg, hy)
    else if (stn == "ujc") 
        ujccalc(stg)
    else if (stn == "alb") 
        albcalc(stg)
    else if (!missing(hy)) 
        flumecalc(stn, stg, hy)
    else if (toupper(stn) %in% c("IVE", "CAR", "EAG", "HEN")) 
        stop(paste("qcalc needs water year for station", stn))
    else flumecalc(stn, stg)
}, source = c("function(stn, stg, hy)", "{", "\tif(stn == \"nfc\" || stn == \"sfc\")", 
"\t\tweircalc(stg)", "\telse if(stn == \"arf\")", "\t\tarfcalc(stg)", 
"\telse if(stn == \"que\")", "\t\tquecalc(stg)", "\telse if(stn == \"ftr\")", 
"\t\tftrcalc(stg, hy)", "\telse if(stn == \"ujc\")", "\t\tujccalc(stg)", 
"      else if(stn == \"alb\")", "            albcalc(stg)", 
"\telse if(!missing(hy))", "\t\tflumecalc(stn, stg, hy)", "\telse if(toupper(stn) %in% c(\"IVE\", \"CAR\", \"EAG\", \"HEN\"))", 
"\t\tstop(paste(\"qcalc needs water year for station\", stn))", 
"\telse flumecalc(stn, stg)", "}"))
qmle <-
structure(function (xsam, ysam, q, var = T, interval = 10) 
{
    {
        k <- 0.06 * interval
        xsample <- log(xsam)
        ysample <- log(ysam)
        lnq <- log(q)
        n <- length(ysample)
        N <- length(q)
        x1 <- cbind(rep(1, n), xsample)
        xx <- t(x1) %*% x1
        invxx <- solve(xx)
        betahat <- invxx %*% t(x1) %*% ysample
        yhat <- x1 %*% betahat
        resid <- ysample - yhat
        rsquare <- (cor(ysample, yhat))^2
        if (n == 2) 
            s2 <- 0
        else s2 <- sum(resid^2)/(n - 2)
        x2 <- cbind(rep(1, N), lnq)
        if (var) {
            V <- try(x2 %*% invxx %*% t(x2))
            if (inherits(V, "try-error")) {
                print("Time series too long, cannot compute covariance matrix")
                var <- F
            }
        }
        if (var) {
            V.diag <- diag(V)
            tmp1 <- matrix(V.diag, ncol = N, nrow = N)
            tmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)
            newyhat <- drop(x2 %*% betahat)
            tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)
            tmp4 <- tmp3 * tmp2 * t(tmp3)
            m <- n - 2
            ebcf <- (1 - s2/m)^(-m/2)
            ebcf2 <- (1 - (2 * s2)/m)^(-m/2)
            EXY <- tmp4 * ebcf2
            murc <- exp(newyhat + (V.diag * s2)/2)
            muqmle <- murc * ebcf
            muumve <- exp(newyhat + s2/2)
            qmle <- k * q * muumve
            bias <- k * sum(q * (muqmle - muumve))
            EX <- matrix(muqmle, ncol = N, nrow = N)
            EY <- t(EX)
            COVXY <- EXY - EX * EY
            qmat <- k * matrix(q, ncol = N, nrow = N)
            COVXY2 <- qmat * COVXY * t(qmat)
            RMSE <- sqrt(sum(COVXY2) + bias^2)
        }
        else {
            newyhat <- drop(x2 %*% betahat)
            muumve <- exp(newyhat + s2/2)
            qmle <- k * q * muumve
            RMSE <- NA
        }
        list(predssc = muumve, est.load = sum(qmle), rsquare = rsquare, 
            betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
    }
}, source = c("function(xsam, ysam, q, var=T, interval = 10)", 
"{", "\t{", "\t\t#  Rev. 1.0  JL  950816", "\t\t#  Computes the QMLE estimate of load and its estimated RMSE for the ", 
"\t\t#  whole storm, based on a log-linear regression of ssc vs q from a", 
"\t\t#  sample (xsam,ysam).  The regression is used to estimate ssc for ", 
"\t\t#  all intervals including the sampled ones.", "\t\t#  Based on Gilroy et al (1990) formulas 17,18,21,22 for QMLE method.", 
"\t\t#  The method is altered so that the regression predicts ssc instead ", 
"\t\t#  of flux.", "\t\t#  Units: q (m3/sec), ssc (mg/l), interval (min)", 
"\t\tk <- 0.06 * interval", "\t\txsample <- log(xsam)", "\t\tysample <- log(ysam)", 
"\t\tlnq <- log(q)", "\t\tn <- length(ysample)", "\t\tN <- length(q)", 
"\t\tx1 <- cbind(rep(1, n), xsample)", "\t\txx <- t(x1) %*% x1", 
"\t\tinvxx <- solve(xx)", "\t\tbetahat <- invxx %*% t(x1) %*% ysample", 
"\t\tyhat <- x1 %*% betahat", "\t\tresid <- ysample - yhat", 
"\t\trsquare <- (cor(ysample, yhat))^2", "\t\tif(n == 2)", "\t\t\ts2 <- 0", 
"\t\telse s2 <- sum(resid^2)/(n - 2)", "\t\tx2 <- cbind(rep(1, N), lnq)", 
"\t\tif (var) { ", "\t\t    V <- try(x2 %*% invxx %*% t(x2))", 
"\t\t    if (inherits(V,\"try-error\")) {", "               \t\tprint(\"Time series too long, cannot compute covariance matrix\")", 
"                \tvar <- F", "           \t    }", "        \t}", 
"        \tif (var) {", "           \t   V.diag <- diag(V)", 
"\t\t   tmp1 <- matrix(V.diag, ncol = N, nrow = N)", "\t\t   tmp2 <- exp(((tmp1 + t(tmp1) + 2 * V) * s2)/2)", 
"\t\t   newyhat <- drop(x2 %*% betahat)", "\t\t   tmp3 <- matrix(exp(newyhat), ncol = N, nrow = N)", 
"\t\t   tmp4 <- tmp3 * tmp2 * t(tmp3)", "\t\t   m <- n - 2", 
"\t\t   ebcf <- (1 - s2/m)^( - m/2)", "\t\t   ebcf2 <- (1 - (2 * s2)/m)^( - m/2)", 
"\t\t   EXY <- tmp4 * ebcf2", "\t\t   murc <- exp(newyhat + (V.diag * s2)/2)", 
"\t\t   muqmle <- murc * ebcf", "\t\t   muumve <- exp(newyhat + s2/2)", 
"\t\t   qmle <- k * q * muumve", "\t\t   bias <- k * sum(q * (muqmle - muumve))", 
"\t\t   EX <- matrix(muqmle, ncol = N, nrow = N)", "\t\t   EY <- t(EX)", 
"\t\t   COVXY <- EXY - EX * EY", "\t\t   qmat <- k * matrix(q, ncol = N, nrow = N)", 
"\t\t   COVXY2 <- qmat * COVXY * t(qmat)", "\t\t   RMSE <- sqrt(sum(COVXY2) + bias^2)", 
"\t\t}", "\t\telse {", "\t\t   newyhat <- drop(x2 %*% betahat)", 
"\t\t   muumve <- exp(newyhat + s2/2)", "\t\t   qmle <- k * q * muumve", 
"\t\t   RMSE <- NA", "\t\t}", "\t\tlist(predssc = muumve, est.load = sum(qmle), rsquare = rsquare,", 
"\t\t\tbetahat = betahat, s = sqrt(s2), est.rmse = RMSE)", "\t}", 
"}"))
qsscplot <-
structure(function (stn, hy, sdate, stime = 0, edate, etime = 2400, 
    dumpstr, bottlestr, exclude = TRUE, type = "logxy", col = T, 
    textsize = 0.6, span = 1, degree = 1, txt = "bottle", units = "cfs", 
    ...) 
{
    nhy <- as.numeric(hy)
    prevyr <- ifelse(nhy == 0, "99", zfill(nhy - 1, 2))
    hy <- zfill(hy, 2)
    df <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    station.name <- attr(df, "stn")
    stn <- toupper(stn)
    if (missing(dumpstr) && missing(bottlestr)) {
        if (missing(sdate)) 
            sdate <- paste(prevyr, "0801", sep = "")
        if (missing(edate)) 
            edate <- paste(hy, "0731", sep = "")
        df <- subtime(df, sdate, stime, edate, etime)
        subtitle <- paste(zfill(sdate, 6), ":", zfill(stime, 
            4), " - ", zfill(edate, 6), ":", zfill(etime, 4), 
            sep = "")
    }
    else if (missing(bottlestr)) {
        if (is.numeric(dumpstr)) {
            dumps <- dumpstr
            dumpstr <- deparse(substitute(dumpstr))
        }
        else {
            dumps <- eval(parse(text = dumpstr))
        }
        df <- df[df$dump %in% dumps, ]
        subtitle <- paste("dumps =", dumpstr)
    }
    else {
        if (is.numeric(dumpstr)) {
            dumps <- dumpstr
            dumpstr <- deparse(substitute(dumpstr))
            bottles <- bottlestr
            bottlestr <- deparse(substitute(bottlestr))
        }
        else {
            dumps <- eval(parse(text = dumpstr))
            bottles <- eval(parse(text = bottlestr))
        }
        df <- df[allmatches(paste(dumps, bottles), paste(df$dump, 
            df$bottle)), ]
        subtitle <- paste("dumps =", dumpstr, ", bottles =", 
            bottlestr)
    }
    if (exclude & !is.null(df$exclude)) 
        df <- df[!df$exclude, ]
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    df$q <- df$q/qfactor
    if (all(is.na(df$ssc) | is.na(df$q))) 
        stop("No matching samples or all specified samples have missing ssc or q")
    df <- df[!is.na(df$ssc), ]
    conf <- intersect(names(df), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in qsscplot:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    if (!(txt %in% names(df))) 
        stop(paste(txt, "is not a valid column in sed object"))
    attach(df)
    on.exit(detach(df))
    if (type == "logxy") 
        log <- "xy"
    else if (type == "logx") 
        log <- "x"
    else log <- ""
    plot(q, ssc, type = "n", xlab = "discharge (m3/sec)", ylab = "ssc", 
        log = log, ...)
    title(paste("Station ", stn, "; ", subtitle, sep = ""), cex.main = 1)
    txtitem <- eval(as.name(txt))
    if (col) {
        dumps <- unique(dump)
        for (i in seq(along = dumps)) {
            d <- dumps[i]
            text(q[dump == d], ssc[dump == d], txtitem[dump == 
                d], cex = textsize, col = i)
        }
    }
    else text(q, ssc, txtitem, cex = textsize)
    if (type == "linear") {
        fit <- lm(ssc ~ q, na.action = na.omit)
        abline(fit)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit"
    }
    else if (type == "logx") {
        fit <- lm(ssc ~ log(q), na.action = na.omit)
        x0 <- range(q)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit to log(x)"
        y0 <- predict(fit, newdata = data.frame(q = x0))
        lines(x0, y0)
    }
    else if (type == "logxy") {
        if (any(q <= 0 | ssc <= 0)) {
            keep <- q > 0 & ssc > 0
            q <- q[keep]
            ssc <- ssc[keep]
        }
        fit <- logline(q, ssc)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "log-log fit"
    }
    else if (type == "sqrt") {
        fit <- lm(sqrt(ssc) ~ sqrt(q), subset = (q >= 0 & ssc >= 
            0))
        x0 <- seq(min(q), max(q), len = 50)
        y0 <- predict(fit, newdata = data.frame(q = x0))
        lines(x0, y0^2)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "sqrt-sqrt fit"
    }
    else if (type == "power") {
        startdata <- df[q != 0 & ssc != 0, ]
        startmodel <- lm(log(ssc) ~ log(q), data = startdata)
        a0 <- exp(coef(startmodel)[1])
        b0 <- coef(startmodel)[2]
        fit <- nls(ssc ~ a * q^b, start = list(a = a0, b = b0))
        x0 <- seq(min(q), max(q), len = 25)
        fit$s <- summary(fit)$sigma
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- "power fit"
        y0 <- predict(fit, newdata = data.frame(q = x0))
        lines(x0, y0)
    }
    else if (type == "loess") {
        if (span == 0) 
            span <- optimize.loess(q, ssc, 0.5, 1, by = 0.05, 
                degree)
        lines(loess.smooth(q, ssc, span = span, degree = degree, 
            family = "gaussian"))
        fit <- loess(ssc ~ q, degree = degree, span = span, family = "gaussian")
        fit$aicc <- aic.loess(fit)
        fit$s <- summary(fit)$s
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- paste("loess(span=", round(span, 2), ",degree=", 
            degree, ")", sep = "")
    }
    else if (type == "pairs") {
        lines(q, ssc)
        label <- "pairwise fit"
    }
    mtext(label, side = 3, line = 0.5, cex = 0.8)
    if (type != "pairs") 
        if (!is.null(fit$aicc)) 
            list(model = fit, r2 = fit$r2, s = fit$s, AICc = fit$aicc)
        else list(model = fit, r2 = fit$r2, s = fit$s)
}, source = c("function(stn, hy, sdate, stime = 0, edate, etime = 2400, dumpstr, bottlestr, exclude = TRUE, type = \"logxy\", col = T, textsize = 0.6, span = 1, degree = 1, txt = \"bottle\", units = \"cfs\", ...)", 
"{", "  #  Revised 2012sep02 Uses \"stn\" attribute of data in plot title", 
"  #     For GUI, permits dumps and bottles to be specified as string expressions", 
"  #  Revised 2011jul11   Now checks for valid values of txt argument", 
"  #     txt=\"dumps\" formerly created invalid scatterplots", 
"  #     Also improved titling of plots with dumps and bottles arguments", 
"  #  Revised 2011feb10   Changed the way exclude works.  The exclude argument", 
"  #     is a logical variable, by default TRUE, governing whether to exclude", 
"  #     samples from analyses according to the column of the same name,", 
"  #     \"exclude\", in the sediment data frame.  This column is set and", 
"  #     modified interactively from the TTS menu.", "  # Arguments:", 
"  # stn = 3-letter station name (e.g. \"nfc\")", "  # hy = 2 digit water year", 
"  # sdate = start date (e.g. 980104)", "  # stime = start military time (e.g. 1830)", 
"  # edate = end date", "  # etime = end military time", "  # dumpstr = expression for vector of dump numbers to include in plot", 
"  # bottlestr = expression for vector of bottle numbers to include in plot", 
"  #    dumpstr can be included without bottlestr: all bottles for those dumps will be plotted", 
"  #    if both dumpstr and bottlestr are specified, the vectors must have same length", 
"  #   dumps and bottles overrides date specification of samples", 
"  # excludes samples whose stgcode matches any value in exclude vector", 
"  # type = \"linear\", \"logx\", \"logxy\", \"sqrt\", \"power\", \"loess\", or \"pairs\"", 
"  # col = whether or not to use color to distinguish dumps", 
"  # textsize = relative size of txt plotting symbols", "  # span = smoothing parameter for loess (0 - 1)", 
"  # degree = degree of loess smooth (1 or 2)", "  # txt = name of text item to use as plotting symbols for samples", 
"  # units = \"cfs\" (ft3/sec) or \"cumecs\" (m3/sec)", "  # ... graphical parameters passed through to plot() ", 
"  nhy <- as.numeric(hy)", "  prevyr <- ifelse(nhy == 0, \"99\", zfill(nhy - 1, 2))", 
"  hy <- zfill(hy, 2)", "  df <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"  # Get station name from \"stn\" attribute since interstorm data get renamed", 
"  station.name <- attr(df,\"stn\")", "  stn <- toupper(stn)", 
"  if(missing(dumpstr) && missing(bottlestr)) {", "    # Use bottles between specified start and end dates", 
"    if(missing(sdate)) sdate <- paste(prevyr, \"0801\", sep = \"\")", 
"    if(missing(edate))", "      edate <- paste(hy, \"0731\", sep = \"\")", 
"    df <- subtime(df, sdate, stime, edate, etime)", "    subtitle <- paste(zfill(sdate,6), \":\", zfill(stime, 4), \" - \", ", 
"                      zfill(edate,6), \":\", zfill(etime, 4),sep=\"\")", 
"  }", "  else if(missing(bottlestr)) {", "    if (is.numeric(dumpstr)) {", 
"      # handles numeric inputs, i.e. when user types the command", 
"      dumps <- dumpstr", "      dumpstr <- deparse(substitute(dumpstr))", 
"    }", "    else {", "      # handles string input, i.e. when command is created from GUI", 
"      dumps <- eval(parse(text=dumpstr))", "    }      ", "    # Use all bottles in specified dumps", 
"    df <- df[df$dump %in% dumps,  ]", "    # dumparg <- format(match.call()$dumps)", 
"    subtitle <- paste(\"dumps =\", dumpstr)        }", "  else {", 
"    if (is.numeric(dumpstr)) {", "      # handles numeric inputs, i.e. when user types the command", 
"      # I assume here that if dumpstr is numeric then so is bottlestr", 
"      dumps <- dumpstr", "      dumpstr <- deparse(substitute(dumpstr))", 
"      bottles <- bottlestr", "      bottlestr <- deparse(substitute(bottlestr))", 
"    }", "    else {", "      # handles string input, i.e. when command is created from GUI", 
"      dumps <- eval(parse(text=dumpstr))", "      bottles <- eval(parse(text=bottlestr))", 
"    }", "    # Use only specified bottles from specified dumps", 
"    df <- df[allmatches(paste(dumps, bottles), paste(df$dump, df$", 
"      bottle)),  ]", "    # dumparg <- format(match.call()$dumps)", 
"    # botarg <- format(match.call()$bottles)", "    subtitle <- paste(\"dumps =\", dumpstr, \", bottles =\", bottlestr)", 
"  }", "  if (exclude & !is.null(df$exclude))", "    df <- df[!df$exclude,  ]", 
"  if(units == \"cfs\")", "    qfactor <- 35.31467", "  else if(units == \"cumecs\")", 
"    qfactor <- 1", "  else stop(\"flow units must be cfs or cumecs\")", 
"  df$q <- df$q/qfactor", "  if(all(is.na(df$ssc) | is.na(df$q)))", 
"    stop(\"No matching samples or all specified samples have missing ssc or q\"", 
"    )", "  df <- df[!is.na(df$ssc),  ]", "  conf <- intersect(names(df),objects(1))", 
"  if (length(conf) > 0) {", "    cat(\"The following objects conflict with object names in qsscplot:\\n\")", 
"    cat(conf,\"\\n\")", "    cat(\"Please remove conflicting objects before proceeding.\\n\")", 
"    return(invisible())", "  } ", "  if (!(txt %in% names(df)))", 
"    stop(paste(txt,\"is not a valid column in sed object\"))", 
"  attach(df)", "  on.exit(detach(df))", "  if(type == \"logxy\")", 
"    log <- \"xy\"", "  else if(type == \"logx\")", "    log <- \"x\"", 
"  else log <- \"\"", "  plot(q, ssc, type = \"n\", xlab = \"discharge (m3/sec)\", ylab = \"ssc\", log = log,", 
"       ...)", "  title(paste(\"Station \", stn, \"; \", subtitle, sep=\"\"), cex.main=1)", 
"  txtitem <- eval(as.name(txt))", "  if(col) {", "    dumps <- unique(dump)", 
"    for(i in seq(along = dumps)) {", "      d <- dumps[i]", 
"      text(q[dump == d], ssc[dump == d], txtitem[dump == d],", 
"           cex = textsize, col = i)", "    }", "  }", "  else text(q, ssc, txtitem, cex = textsize)", 
"  if(type == \"linear\") {", "    fit <- lm(ssc ~ q, na.action = na.omit)", 
"    abline(fit)", "    fit$aicc <- aic.lm(fit)[\"aic.c\"]", 
"    fit$s <- summary(fit)$sigma", "    fit$r2 <- summary(fit)$r.sq", 
"    label <- \"linear fit\"", "  }", "  else if(type == \"logx\") {", 
"    fit <- lm(ssc ~ log(q), na.action = na.omit)", "    x0 <- range(q)", 
"    fit$aicc <- aic.lm(fit)[\"aic.c\"]", "    fit$s <- summary(fit)$sigma", 
"    fit$r2 <- summary(fit)$r.sq", "    label <- \"linear fit to log(x)\"", 
"    y0 <- predict(fit, newdata = data.frame(q = x0))", "    lines(x0, y0)", 
"  }", "  else if(type == \"logxy\") {", "    if (any(q<=0 | ssc<=0)) {", 
"      keep <- q > 0 & ssc > 0", "      q <- q[keep]", "      ssc <- ssc[keep]", 
"    }", "    fit <- logline(q, ssc)", "    fit$s <- summary(fit)$sigma", 
"    fit$r2 <- summary(fit)$r.sq", "    label <- \"log-log fit\"", 
"  }", "  else if(type == \"sqrt\") {", "    fit <- lm(sqrt(ssc) ~ sqrt(q), subset = (q >= 0 & ssc >= 0))", 
"    x0 <- seq(min(q), max(q), len = 50)", "    y0 <- predict(fit, newdata = data.frame(q = x0))", 
"    lines(x0, y0^2)", "    fit$s <- summary(fit)$sigma", "    fit$r2 <- summary(fit)$r.sq", 
"    label <- \"sqrt-sqrt fit\"", "  }", "  else if(type == \"power\") {", 
"    startdata <- df[q != 0 & ssc != 0,  ]", "    startmodel <- lm(log(ssc) ~ log(q), data = startdata)", 
"    a0 <- exp(coef(startmodel)[1])", "    b0 <- coef(startmodel)[2]", 
"    fit <- nls(ssc ~ a * q^b, start = list(a = a0, b = b0))", 
"    x0 <- seq(min(q), max(q), len = 25)", "    fit$s <- summary(fit)$sigma", 
"    fit$r2 <- cor(fitted(fit),ssc)^2", "    label <- \"power fit\"", 
"    y0 <- predict(fit, newdata = data.frame(q = x0))", "    lines(x0, y0)", 
"  }", "  else if(type == \"loess\") {", "    # Uses gaussian family so flowloess can extract RSE", 
"    if(span == 0) span <- optimize.loess(q, ssc, 0.5, 1, by = ", 
"      0.05, degree)", "    lines(loess.smooth(q, ssc, span = span, degree = degree, family", 
"                       = \"gaussian\"))", "    fit <- loess(ssc ~ q, degree = degree, span = span, family = ", 
"      \"gaussian\")", "    fit$aicc <- aic.loess(fit)", "    fit$s <- summary(fit)$s", 
"    fit$r2 <- cor(fitted(fit),ssc)^2", "    label <- paste(\"loess(span=\", round(span,2), \",degree=\", degree, \")\", sep = \"\")", 
"  }", "  else if(type == \"pairs\") {", "    lines(q, ssc)", 
"    label <- \"pairwise fit\"", "  }", "  mtext(label, side = 3, line = 0.5, cex=0.8)", 
"  if(type != \"pairs\")", "    if (!is.null(fit$aicc))", "      list(model=fit,r2=fit$r2,s=fit$s,AICc=fit$aicc)", 
"  else", "    list(model=fit,r2=fit$r2,s=fit$s)", "}"))
qturbplot <-
structure(function (df, sdate, stime, edate, etime, col = T, 
    log = "xy", reg = T, units = "cfs", ...) 
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
}, source = c("function(df, sdate, stime, edate, etime, col = T, log = \"xy\", reg = T, units = ", 
"\t\"cfs\", ...)", "{", "# Old function, not updated to be compatible with turbsscplot & qsscplot", 
"# Scatterplots of turbidity versus discharge from .sed data frame", 
"# df is the .sed data frame", "\tstn <- substring(deparse(substitute(df)), 1, 3)", 
"\tstart <- dates(as.character(sdate), format = \"ymd\", out = \"m/d/y\")", 
"\tstart <- start + mt2msm(stime)/1440", "\tend <- dates(as.character(edate), format = \"ymd\", out = \"m/d/y\")", 
"\tend <- end + mt2msm(etime)/1440", "\tdf <- df[df$chr >= start & df$chr <= end,  ]", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tdf$q <- df$q/qfactor", "\tconf <- intersect(names(df),objects(1))", 
"\tif (length(conf) > 0) {", "\t\tcat(\"The following objects conflict with object names in qturbplot:\\n\")", 
"\t\tcat(conf,\"\\n\")", "\t\tcat(\"Please remove conflicting objects before proceeding.\\n\")", 
"\t\treturn(invisible())", "\t}", "\tattach(df)", "\tplot(q, turb, type = \"n\", xlab = \"discharge\", ylab = \"turb\", log = log,", 
"\t\t...)", "\ttitle(paste(\"Station \", stn, \": \", format(start), \"-\", format(end),", 
"\t\tsep = \"\"), cex = 0.8)", "\tif(col) {", "\t\tdumps <- unique(dump)", 
"\t\tfor(i in seq(along = dumps)) {", "\t\t\td <- dumps[i]", 
"\t\t\ttext(q[dump == d], turb[dump == d], bottle[dump == d],", 
"\t\t\t\tcex = 0.6, col = i)", "\t\t}", "\t}", "\telse text(q, turb, bottle, cex = 0.6)", 
"\tif(reg) {", "\t\tif(log == \"xy\")", "\t\t\tlogline(q, turb)", 
"\t\telse abline(lm(turb ~ q))", "\t}", "\tinvisible(detach(2))", 
"}"))
quecalc <-
structure(function (stg) 
{
    print("Warning from qcalc: QUE rating equation is approximate")
    a1 <- 5.99432
    b1 <- -36.5344
    c1 <- 56.6576
    a2 <- -0.2925
    b2 <- 1.2087
    c2 <- 0
    ifelse(stg > 0.33, a1 + b1 * stg + c1 * stg^2, a2 + b2 * 
        stg + c2 * stg^2)
}, source = c("function(stg)", "{", "\t# QUE discharge rating approximation.", 
"\t# Actual calculation should be 0.96*(SFC - OGI)", "\tprint(\"Warning from qcalc: QUE rating equation is approximate\")", 
"\t# Constants below are based on HY01 thru dump 25", "\ta1 <- 5.99432", 
"\tb1 <- -36.5344", "\tc1 <- 56.6576", "\ta2 <- -0.2925", "\tb2 <- 1.2087", 
"\tc2 <- 0", "\tifelse(stg > 0.33, a1 + b1 * stg + c1 * stg^2, a2 + b2 * stg + c2 * stg^2)", 
"}"))
rawplot.gui <-
structure(function (path = getTTSenv("LOGGERHOME")) 
{
    require(tcltk) || stop("tcltk support is absent")
    options(warn = -1)
    require(chron) || stop("chron support is absent")
    options(warn = 0)
    while (1) {
        pathfile <- select.file(path)
        if (pathfile == "") 
            break
        dump <- read.raw(pathfile)
        vars <- names(dump)
        plot <- function() {
            if (tclvalue(choice) != "none") {
                tflag <- var.check(tclvar)
                if (tflag == T) {
                  schron <- chron(tclvalue(sd), mt2msm(as.numeric(tclvalue(st)))/1440)
                  echron <- chron(tclvalue(ed), mt2msm(as.numeric(tclvalue(et)))/1440)
                  if (tclvalue(choice) == "stage") 
                    dump$left <- dump$stg
                  if (tclvalue(choice) == "turbidity") 
                    dump$left <- dump$turb
                  if (tclvalue(choice) == "stage-turbidity") {
                    dump$left <- dump$stg
                    dump$right <- dump$turb
                  }
                  if (tclvalue(choice) == "rainfall") 
                    dump$left <- dump$rain
                  if (tclvalue(choice) == "water temperature") 
                    dump$left <- dump$wtemp
                  if (tclvalue(choice) == "air temperature") 
                    dump$left <- dump$atemp
                  if (tclvalue(choice) == "water-air temperature") {
                    dump$left <- dump$wtemp
                    dump$right <- dump$atemp
                  }
                  if (tclvalue(choice) == "throughfall") 
                    dump$left <- dump$lc1
                  if (tclvalue(choice) == "wind speed") 
                    dump$left <- dump$wind
                  dump <- as.data.frame(dump)
                  dump <- dump[dump$chron >= schron & dump$chron <= 
                    echron, ]
                  tts.rawplot(dump, tclvar, schron, echron, attr(dump, 
                    "minstg"))
                  tkraise(gui)
                }
            }
            else {
                winDialog(type = c("ok"), "ERROR: You must first choose the type of data you wish to\nplot from the Data Type pull down menu.")
            }
        }
        def.time <- function() {
            if (tclvalue(sd) == "m/d/y") {
                tclvalue(sd) <- format(dates(dump$chron[1]))
                tclvalue(st) <- dump$time[1]
            }
            else {
                check.sd <- try(dates(tclvalue(sd)))
                if (inherits(check.sd, "try-error")) {
                  tclvalue(sd) <- format(dates(dump$chron[1]))
                  tclvalue(st) <- dump$time[1]
                  winDialog(type = c("ok"), "ERROR: Defaulted to dumps start date\ndue to an incorrect date field (mm/dd/yy).")
                }
            }
            if (tclvalue(ed) == "m/d/y") {
                tclvalue(ed) <- format(dates(dump$chron[length(dump$chron)]))
                tclvalue(et) <- dump$time[length(dump$time)]
            }
            else {
                check.ed <- try(dates(tclvalue(ed)))
                if (inherits(check.ed, "try-error")) {
                  tclvalue(ed) <- format(dates(dump$chron[length(dump$chron)]))
                  tclvalue(et) <- dump$time[length(dump$time)]
                  winDialog(type = c("ok"), "ERROR: Defaulted to dumps end date\ndue to an incorrect date field (mm/dd/yy).")
                }
            }
        }
        cmd1 <- function() {
            time <- def.time()
            tclvalue(choice) <- "stage"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(dump$stg, na.rm = T)
            tclvalue(max1) <- max(dump$stg, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd2 <- function() {
            time <- def.time()
            tclvalue(choice) <- "turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(dump$turb, na.rm = T)
            tclvalue(max1) <- max(dump$turb, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd3 <- function() {
            time <- def.time()
            tclvalue(choice) <- "stage-turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(dump$stg, na.rm = T)
            tclvalue(max1) <- max(dump$stg, na.rm = T)
            tclvalue(min2) <- min(dump$turb, na.rm = T)
            tclvalue(max2) <- max(dump$turb, na.rm = T)
        }
        cmd4 <- function() {
            time <- def.time()
            tclvalue(choice) <- "rainfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- 0
            tclvalue(max1) <- sum(dump$rain, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd5 <- function() {
            time <- def.time()
            tclvalue(choice) <- "water temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(dump$wtemp, na.rm = T)
            tclvalue(max1) <- max(dump$wtemp, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd6 <- function() {
            time <- def.time()
            tclvalue(choice) <- "air temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(dump$atemp, na.rm = T)
            tclvalue(max1) <- max(dump$atemp, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd7 <- function() {
            time <- def.time()
            tclvalue(choice) <- "water-air temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(dump$wtemp, na.rm = T)
            tclvalue(max1) <- max(dump$wtemp, na.rm = T)
            tclvalue(min2) <- min(dump$atemp, na.rm = T)
            tclvalue(max2) <- max(dump$atemp, na.rm = T)
        }
        cmd8 <- function() {
            time <- def.time()
            tclvalue(choice) <- "throughfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(dump$lc1, na.rm = T)
            tclvalue(max1) <- max(dump$lc7, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd9 <- function() {
            time <- def.time()
            tclvalue(choice) <- "wind speed"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(dump$wind, na.rm = T)
            tclvalue(max1) <- max(dump$wind, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        gui <- tktoplevel()
        tkwm.title(gui, "TTS RAWPLOT")
        labpath <- tklabel(gui, text = "Path name:")
        pathname <- tclVar(pathfile)
        entpath <- tkentry(gui, textvariable = as.character(pathname), 
            justify = "center", background = "grey", state = "disabled")
        tkpack(labpath, anchor = "w")
        tkpack(entpath, fill = "both", padx = 2)
        tkpack(mb <- tkmenubutton(gui, text = "Data Type", underline = "0", 
            relief = "raised"), fill = "both", pady = 4, padx = 2)
        mb.menu <- tkmenu(mb)
        tkconfigure(mb, menu = paste(tcl("winfo", "parent", mb.menu), 
            ".", tcl("winfo", "name", mb.menu), sep = ""))
        if (any(vars == "stg")) 
            tkadd(mb.menu, "command", label = "stage", command = cmd1)
        if (any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "turbidity", command = cmd2)
        if (any(vars == "stg") & any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "stage-turbidity", 
                command = cmd3)
        if (any(vars == "rain")) 
            tkadd(mb.menu, "command", label = "rainfall", command = cmd4)
        if (any(vars == "wtemp")) 
            tkadd(mb.menu, "command", label = "water temperature", 
                command = cmd5)
        if (any(vars == "atemp")) 
            tkadd(mb.menu, "command", label = "air temperature", 
                command = cmd6)
        if (any(vars == "wtemp") & any(vars == "atemp")) 
            tkadd(mb.menu, "command", label = "water-air temperature", 
                command = cmd7)
        if (any(vars == "lc1") | any(vars == "lc2") | any(vars == 
            "lc3") | any(vars == "lc4") | any(vars == "lc5") | 
            any(vars == "lc6") | any(vars == "lc7")) 
            tkadd(mb.menu, "command", label = "throughfall", 
                command = cmd8)
        if (any(vars == "wind")) 
            tkadd(mb.menu, "command", label = "wind speed", command = cmd9)
        choice <- tclVar("none")
        entchoice <- tkentry(gui, textvariable = as.character(choice), 
            justify = "center", background = "grey", state = "disabled")
        tkpack(entchoice, fill = "both", padx = 2)
        big.frm <- tkframe(gui)
        datl.frm <- tkframe(big.frm)
        datr.frm <- tkframe(big.frm)
        labsd <- tklabel(datl.frm, text = "Start Date")
        sd <- tclVar("m/d/y")
        entsd <- tkentry(datl.frm, textvariable = as.character(sd), 
            width = 10, justify = "center", background = "white")
        tkpack(labsd, entsd)
        labst <- tklabel(datr.frm, text = "Start Time")
        st <- tclVar(0)
        entst <- tkentry(datr.frm, textvariable = as.character(st), 
            width = 10, justify = "center", background = "white")
        tkpack(labst, entst)
        labed <- tklabel(datl.frm, text = "End Date")
        ed <- tclVar("m/d/y")
        ented <- tkentry(datl.frm, textvariable = as.character(ed), 
            width = 10, justify = "center", background = "white")
        tkpack(labed, ented)
        labet <- tklabel(datr.frm, text = "End Time")
        et <- tclVar(2400)
        entet <- tkentry(datr.frm, textvariable = as.character(et), 
            width = 10, justify = "center", background = "white")
        tkpack(labet, entet)
        tkpack(datl.frm, datr.frm, side = "left")
        tkpack(big.frm, pady = 2, padx = 2)
        pleft <- tclVar(0)
        tkpack(tkcheckbutton(gui, text = "Left Axis", variable = as.character(pleft), 
            state = "disabled"), anchor = "w", padx = 1)
        min1.frm <- tkframe(gui)
        labmin1 <- tklabel(min1.frm, text = "axis min", anchor = "w")
        min1 <- tclVar("NA")
        entmin1 <- tkentry(min1.frm, textvariable = as.character(min1), 
            width = 6, justify = "center", background = "white")
        tkpack(entmin1, labmin1, side = "left")
        tkpack(min1.frm, anchor = "w", padx = 2)
        max1.frm <- tkframe(gui)
        labmax1 <- tklabel(max1.frm, text = "axis max", anchor = "w")
        max1 <- tclVar("NA")
        entmax1 <- tkentry(max1.frm, textvariable = as.character(max1), 
            width = 6, justify = "center", background = "white")
        tkpack(entmax1, labmax1, side = "left")
        tkpack(max1.frm, anchor = "w", padx = 2)
        pright <- tclVar(0)
        tkpack(tkcheckbutton(gui, text = "Right Axis", variable = as.character(pright), 
            state = "disabled"), anchor = "w", padx = 1)
        min2.frm <- tkframe(gui)
        labmin2 <- tklabel(min2.frm, text = "axis min")
        min2 <- tclVar("NA")
        entmin2 <- tkentry(min2.frm, textvariable = as.character(min2), 
            width = 6, justify = "center", background = "white")
        tkpack(entmin2, labmin2, side = "left")
        tkpack(min2.frm, anchor = "w", padx = 2)
        max2.frm <- tkframe(gui)
        labmax2 <- tklabel(max2.frm, text = "axis max")
        max2 <- tclVar("NA")
        entmax2 <- tkentry(max2.frm, textvariable = as.character(max2), 
            width = 6, justify = "center", background = "white")
        tkpack(entmax2, labmax2, side = "left")
        tkpack(max2.frm, anchor = "w", padx = 2)
        tclvar <- list(choice = choice, sd = sd, ed = ed, st = st, 
            et = et, min1 = min1, max1 = max1, min2 = min2, max2 = max2, 
            pright = pright, pleft = pleft)
        done <- tclVar(0)
        but.frm <- tkframe(gui)
        plot.but <- tkbutton(but.frm, text = "PLOT", command = plot)
        quit.but <- tkbutton(but.frm, text = "QUIT", command = function() tclvalue(done) <- 1)
        tkpack(plot.but, quit.but, fill = "both")
        tkpack(but.frm, fill = "both", pady = 4, padx = 2)
        tkraise(gui)
        tkbind(gui, "<Destroy>", function() tclvalue(done) <- 2)
        tkwait.variable(as.character(done))
        if (tclvalue(done) == "2") {
            print("GUI aborted")
            return(invisible())
        }
        if (!is.null(dev.list())) {
            dev.off()
        }
        tkdestroy(gui)
    }
}, source = c("function (path=getTTSenv(\"LOGGERHOME\")) ", "{", 
"#", "# RAWPLOT.GUI()  # formerly TTS.GUI (renamed 10/16/2007 JL)", 
"# Plotting routine developed for field observations of raw data.  ", 
"# Program requires both the tcltk and chron library's.", "#", 
"# Program compiled by Jason C. Fisher", "# Version 1.0  (08/31/2000)", 
"# Version 1.1  (09/05/2000) Added file selection during program introduction.", 
"# Version 1.2  (03/22/2001) R.Field  Added interception plotting options.", 
"# Version 1.3  (12/06/2001) J.Lewis  Put file selection in a loop.  ", 
"#                             Made data path an argument.", 
"#                             Renamed dump.data to read.raw, newdata to select.file.", 
"#                             Changed plot title to TTS RAW DATA PLOT.", 
"# Version 1.4  (11/03/2004) J.Lewis  Made the program more robust.", 
"#       Detects long and short lines, gives warning, and tries to continue", 
"#       Pads all records to the same number of variables using read.table(..., fill=T)", 
"#       Gives warning if labels.txt does not match number of variables, tries to continue", 
"#                     if labels.txt is too short, makes up names for the extra variables", 
"#                     if labels.txt is too long, fills missing variables with missing data", 
"#       In the dialog box, minima and maxima no longer show NA when some data are missing", 
"#       Understands a variable \"cumbot\", the number of the last ISCO bottle filled", 
"#       Data may now contain \"cumbot\" instead of the standard TTS variable \"bottle\"", 
"# Version 2.0  (12/15/2004) J.Lewis  Adds minimum stage line to stage-turbidity plots.", 
"#                           Requires minimum stage to be listed in labels.txt as the", 
"#                           fourth variable (after recording interval).", 
"# Version 2.1  (05/25/2005) J.Lewis  Reports error if station ID not found exactly once", 
"#                           in labels.txt file", "# Version 2.2  (07/26/2007) J.Lewis  Changes to read.raw support new CRBasic output", 
"#       formats \"TOACI1\" and \"TOA5\", i.e. with 2-line or 4-line header and formatted dates", 
"# Version 2.3  (10/19/2007) J.Lewis  tkcmd became defunct with R version 2.4.0 ", 
"#                           Changed all occurrences of tkcmd to tcl ", 
"require(tcltk) || stop(\"tcltk support is absent\")", "options(warn=-1)", 
"require(chron) || stop(\"chron support is absent\")", "options(warn=0) ", 
"while (1) {", "    pathfile<-select.file(path)", "    if (pathfile == \"\") break", 
"    ", "    dump<-read.raw(pathfile)", "    vars<-names(dump)", 
"", "    plot <- function() {", "      if(tclvalue(choice)!=\"none\") {", 
"        tflag<-var.check(tclvar)", "        if(tflag==T) {", 
"          schron<-chron(tclvalue(sd),mt2msm(as.numeric(tclvalue(st)))/1440)", 
"          echron<-chron(tclvalue(ed),mt2msm(as.numeric(tclvalue(et)))/1440)", 
"          if(tclvalue(choice)==\"stage\") dump$left<-dump$stg", 
"          if(tclvalue(choice)==\"turbidity\") dump$left<-dump$turb", 
"          if(tclvalue(choice)==\"stage-turbidity\") {", "            dump$left<-dump$stg", 
"            dump$right<-dump$turb", "          }", "          if(tclvalue(choice)==\"rainfall\") dump$left<-dump$rain", 
"          if(tclvalue(choice)==\"water temperature\") dump$left<-dump$wtemp", 
"          if(tclvalue(choice)==\"air temperature\") dump$left<-dump$atemp", 
"          if(tclvalue(choice)==\"water-air temperature\") {", 
"            dump$left<-dump$wtemp", "            dump$right<-dump$atemp", 
"          }", "          if(tclvalue(choice)==\"throughfall\") dump$left<-dump$lc1", 
"          if(tclvalue(choice)==\"wind speed\") dump$left<-dump$wind", 
"          dump<-as.data.frame(dump)", "          dump<-dump[dump$chron>=schron&dump$chron<=echron, ]", 
"          tts.rawplot(dump,tclvar,schron,echron,attr(dump,\"minstg\"))", 
"          tkraise(gui)      ", "        }", "      }", "      else {", 
"        winDialog(type=c(\"ok\"),\"ERROR: You must first choose the type of data you wish to\\nplot from the Data Type pull down menu.\")", 
"      }", "    }", "    def.time<-function() {", "      if(tclvalue(sd)==\"m/d/y\") {", 
"        tclvalue(sd)<-format(dates(dump$chron[1]))", "        tclvalue(st)<-dump$time[1]  ", 
"      }", "      else {", "        check.sd<-try(dates(tclvalue(sd)))", 
"        if(inherits(check.sd,\"try-error\")) {", "          tclvalue(sd)<-format(dates(dump$chron[1]))", 
"          tclvalue(st)<-dump$time[1]  ", "          winDialog(type=c(\"ok\"),\"ERROR: Defaulted to dumps start date\\ndue to an incorrect date field (mm/dd/yy).\")      ", 
"        }", "      }", "      if(tclvalue(ed)==\"m/d/y\") {", 
"        tclvalue(ed)<-format(dates(dump$chron[length(dump$chron)]))", 
"        tclvalue(et)<-dump$time[length(dump$time)]  ", "      }", 
"      else {", "        check.ed<-try(dates(tclvalue(ed)))", 
"        if(inherits(check.ed,\"try-error\")) {", "          tclvalue(ed)<-format(dates(dump$chron[length(dump$chron)]))", 
"          tclvalue(et)<-dump$time[length(dump$time)] ", "          winDialog(type=c(\"ok\"),\"ERROR: Defaulted to dumps end date\\ndue to an incorrect date field (mm/dd/yy).\")", 
"        }       ", "      }", "    }", "#    print<-function() {", 
"#      win.print(width=6,height=5,pointsize=12)", "#      win.print()", 
"#      plot()", "#      dev.off()", "#    }", "    cmd1<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"stage\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(dump$stg,na.rm=T)", "      tclvalue(max1)<-max(dump$stg,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd2<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"turbidity\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<-min(dump$turb,na.rm=T)", 
"      tclvalue(max1)<-max(dump$turb,na.rm=T)", "      tclvalue(min2)<-\"NA\"", 
"      tclvalue(max2)<-\"NA\"", "    }", "    cmd3<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"stage-turbidity\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 1", 
"      tclvalue(min1)<-min(dump$stg,na.rm=T)", "      tclvalue(max1)<-max(dump$stg,na.rm=T)", 
"      tclvalue(min2)<-min(dump$turb,na.rm=T)", "      tclvalue(max2)<-max(dump$turb,na.rm=T)", 
"    }", "    cmd4<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"rainfall\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<- 0", "      tclvalue(max1)<-sum(dump$rain,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd5<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"water temperature\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<-min(dump$wtemp,na.rm=T)", 
"      tclvalue(max1)<-max(dump$wtemp,na.rm=T)", "      tclvalue(min2)<-\"NA\"", 
"      tclvalue(max2)<-\"NA\"", "    }", "    cmd6<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"air temperature\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(dump$atemp,na.rm=T)", "      tclvalue(max1)<-max(dump$atemp,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd7<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"water-air temperature\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 1", "      tclvalue(min1)<-min(dump$wtemp,na.rm=T)", 
"      tclvalue(max1)<-max(dump$wtemp,na.rm=T)", "      tclvalue(min2)<-min(dump$atemp,na.rm=T)", 
"      tclvalue(max2)<-max(dump$atemp,na.rm=T)", "    }", "    cmd8<-function() {", 
"      time<-def.time()", "      tclvalue(choice)<-\"throughfall\"", 
"      tclvalue(pleft) <- 1", "      tclvalue(pright) <- 0", 
"      tclvalue(min1)<-min(dump$lc1,na.rm=T)", "      tclvalue(max1)<-max(dump$lc7,na.rm=T)", 
"      tclvalue(min2)<-\"NA\"", "      tclvalue(max2)<-\"NA\"", 
"    }", "    cmd9<-function() {", "      time<-def.time()", 
"      tclvalue(choice)<-\"wind speed\"", "      tclvalue(pleft) <- 1", 
"      tclvalue(pright) <- 0", "      tclvalue(min1)<-min(dump$wind,na.rm=T)", 
"      tclvalue(max1)<-max(dump$wind,na.rm=T)", "      tclvalue(min2)<-\"NA\"", 
"      tclvalue(max2)<-\"NA\"", "    }", "    gui <- tktoplevel()", 
"    tkwm.title(gui, \"TTS RAWPLOT\")", "    ", "    labpath <- tklabel(gui, text = \"Path name:\")", 
"    pathname <- tclVar(pathfile)", "    entpath <- tkentry(gui, textvariable = as.character(pathname), justify = \"center\", ", 
"        background = \"grey\",state=\"disabled\")", "    tkpack(labpath, anchor = \"w\")", 
"    tkpack(entpath, fill = \"both\", padx = 2)", " ", "    tkpack(mb<-tkmenubutton(gui,text=\"Data Type\",underline=\"0\",relief=\"raised\"),", 
"      fill=\"both\",pady=4,padx=2)", "    mb.menu<-tkmenu(mb)", 
"    tkconfigure(mb,menu=paste(tcl(\"winfo\",\"parent\",mb.menu),\".\",tcl(\"winfo\",\"name\",mb.menu),sep=\"\"))", 
"    if(any(vars==\"stg\"))", "      tkadd(mb.menu,\"command\",label=\"stage\",command=cmd1)", 
"    if(any(vars==\"turb\"))", "      tkadd(mb.menu,\"command\",label=\"turbidity\",command=cmd2)", 
"    if(any(vars==\"stg\")&any(vars==\"turb\"))", "      tkadd(mb.menu,\"command\",label=\"stage-turbidity\",command=cmd3)", 
"    if(any(vars==\"rain\"))", "      tkadd(mb.menu,\"command\",label=\"rainfall\",command=cmd4)", 
"    if(any(vars==\"wtemp\"))", "      tkadd(mb.menu,\"command\",label=\"water temperature\",command=cmd5)", 
"    if(any(vars==\"atemp\"))", "      tkadd(mb.menu,\"command\",label=\"air temperature\",command=cmd6)", 
"    if(any(vars==\"wtemp\")&any(vars==\"atemp\"))", "      tkadd(mb.menu,\"command\",label=\"water-air temperature\",command=cmd7)", 
"    if(any(vars==\"lc1\")|any(vars==\"lc2\")|any(vars==\"lc3\")|any(vars==\"lc4\")|any(vars==\"lc5\")|any(vars==\"lc6\")|any(vars==\"lc7\"))", 
"      tkadd(mb.menu,\"command\",label=\"throughfall\",command=cmd8)", 
"    if(any(vars==\"wind\"))", "      tkadd(mb.menu,\"command\",label=\"wind speed\",command=cmd9)", 
"    choice <- tclVar(\"none\")", "    entchoice<-tkentry(gui,textvariable=as.character(choice),justify=\"center\",background=\"grey\",state=\"disabled\")", 
"    tkpack(entchoice,fill=\"both\",padx=2)", "", "    big.frm <- tkframe(gui)", 
"    datl.frm <- tkframe(big.frm)", "    datr.frm <- tkframe(big.frm)", 
"    labsd <- tklabel(datl.frm, text = \"Start Date\")", "    sd <- tclVar(\"m/d/y\")", 
"    entsd <- tkentry(datl.frm, textvariable = as.character(sd), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labsd, entsd)", 
"", "    labst <- tklabel(datr.frm, text = \"Start Time\")", 
"    st <- tclVar(0)", "    entst <- tkentry(datr.frm, textvariable = as.character(st), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labst, entst)", 
"", "    labed <- tklabel(datl.frm, text = \"End Date\")", "    ed <- tclVar(\"m/d/y\")", 
"    ented <- tkentry(datl.frm, textvariable = as.character(ed), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labed, ented)", 
"", "    labet <- tklabel(datr.frm, text = \"End Time\")", "    et <- tclVar(2400) ", 
"    entet <- tkentry(datr.frm, textvariable = as.character(et), width = 10, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(labet, entet)", 
"    ", "    tkpack(datl.frm, datr.frm, side = \"left\")", "    tkpack(big.frm, pady = 2,padx=2)", 
"    ", "    pleft <- tclVar(0)", "    tkpack(tkcheckbutton(gui, text = \"Left Axis\", variable = as.character(pleft),state=\"disabled\"), ", 
"        anchor = \"w\", padx = 1)", "    ", "    min1.frm <- tkframe(gui)", 
"    labmin1 <- tklabel(min1.frm, text = \"axis min\", anchor = \"w\")", 
"    min1 <- tclVar(\"NA\")", "    entmin1 <- tkentry(min1.frm, textvariable = as.character(min1), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmin1, labmin1, side = \"left\")", 
"    tkpack(min1.frm, anchor = \"w\", padx = 2)", " ", "    ", 
"    max1.frm <- tkframe(gui)", "    labmax1 <- tklabel(max1.frm, text = \"axis max\", anchor = \"w\")", 
"    max1 <- tclVar(\"NA\")", "    entmax1 <- tkentry(max1.frm, textvariable = as.character(max1), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmax1, labmax1, side = \"left\")", 
"    tkpack(max1.frm, anchor = \"w\", padx = 2)", "     ", "    pright <- tclVar(0)", 
"    tkpack(tkcheckbutton(gui, text = \"Right Axis\", variable = as.character(pright),state=\"disabled\"), ", 
"        anchor = \"w\", padx = 1)", "    ", "    min2.frm <- tkframe(gui)", 
"    labmin2 <- tklabel(min2.frm, text = \"axis min\")", "    min2 <- tclVar(\"NA\")", 
"    entmin2 <- tkentry(min2.frm, textvariable = as.character(min2), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmin2, labmin2, side = \"left\")", 
"    tkpack(min2.frm, anchor = \"w\", padx = 2)", "    ", "    max2.frm <- tkframe(gui)", 
"    labmax2 <- tklabel(max2.frm, text = \"axis max\")", "    max2 <- tclVar(\"NA\")", 
"    entmax2 <- tkentry(max2.frm, textvariable = as.character(max2), width = 6, ", 
"        justify = \"center\", background = \"white\")", "    tkpack(entmax2, labmax2, side = \"left\")", 
"    tkpack(max2.frm, anchor = \"w\", padx = 2)", "    ", "    tclvar <- list(choice=choice,sd=sd,ed=ed,st=st,et=et,min1=min1,max1=max1,min2=min2,max2=max2,pright=pright,pleft=pleft)", 
"", "    done <- tclVar(0)", "    but.frm <- tkframe(gui)", "    plot.but <- tkbutton(but.frm, text = \"PLOT\", command = plot)", 
"#    print.but<-tkbutton(but.frm,text=\"PRINT\",command=print)", 
"    quit.but <- tkbutton(but.frm, text = \"QUIT\", command = function() tclvalue(done) <- 1)", 
"    tkpack(plot.but,quit.but, fill = \"both\")", "    tkpack(but.frm, fill = \"both\", pady = 4, padx = 2)", 
"", "    tkraise(gui)", "    tkbind(gui, \"<Destroy>\", function() tclvalue(done) <- 2)", 
"    tkwait.variable(as.character(done))", "    if (tclvalue(done) == \"2\") { ", 
"        print(\"GUI aborted\")", "        return(invisible())", 
"    }", "    if(!is.null(dev.list())){", "      dev.off()", 
"    }", "    tkdestroy(gui)", "}", "}"))
read.allstorms <-
function(stn, hy, match = T, loc = paste(Sys.getenv("WATER"),"\\Sediment\\SuspendedSediment\\MethodsTesting_Analyses\\SuspendedSedLoads\\LoadEstimation\\hy", hy, sep = "")) 
{
    hy <- zfill(hy, 2)
    if (match) 
        filename <- paste("storms", hy, ".alt", sep = "")
    else filename <- paste("storms", hy, ".csv", sep = "")
    file <- paste(loc, filename, sep = "/")
    options(show.error.messages = F, warn = -1)
    dat <- try(read.table(file, sep = ",", header = T))
    options(show.error.messages = T, warn = 0)
    if (class(dat) == "try-error") 
        if (match) {
            cat("No alt file found: reading csv file\n")
            return(read.allstorms(stn, hy, match = F))
        }
        else stop(paste("Cannot find or read", file))
    cat(paste("Storm dates read from file ", filename, "\n", 
        sep = "'"))
    names(dat) <- c("stn", "number", "sdate", "stime", "edate", 
        "etime")
    schron <- chron(ymd2date(dat$sdate)) + mt2msm(dat$stime)/1440
    echron <- chron(ymd2date(dat$edate)) + mt2msm(dat$etime)/1440
    newdat <- data.frame(stn = dat$stn, number = dat$number, 
        schron, echron)
    newdat$stn <- as.character(newdat$stn)
    newdat[toupper(newdat$stn) == toupper(stn), c("number", "schron", 
        "echron")]
}
read.campbell <-
structure(function (file, path = getTTSenv("LOGGERHOME")) 
{
    data <- read.table(paste(path, file, sep = "\\"), sep = ",")[, 
        2:10]
    names(data) <- c("year", "day", "time", "dump", "bottle", 
        "code1", "code2", "stg", "turb")
    data$chr <- campbell.date(data$year, data$day, data$time)
    nc <- dim(data)[[2]]
    data[, c(nc, 5:(nc - 1))]
}, source = c("function(file,path=getTTSenv(\"LOGGERHOME\"))", 
"{", "\t# Reads a Campbell TTS data file and creates a data frame", 
"\tdata <- read.table(paste(path,file,sep=\"\\\\\"), sep = \",\")[, 2:10]", 
"\tnames(data) <- c(\"year\", \"day\", \"time\", \"dump\", \"bottle\", \"code1\",", 
"\t\t\"code2\", \"stg\", \"turb\")", "\tdata$chr <- campbell.date(data$year, data$day, data$time)", 
"\tnc <- dim(data)[[2]]", "\tdata[, c(nc, 5:(nc - 1))]", "}"))
read.crbasic <-
structure(function (pathname) 
{
    basicnames <- c("TIMESTAMP", "TMSTAMP", "RECORD", "RECNBR", 
        "dumpCount", "bottle", "cumbot", "threshCode", "sampleCode", 
        "stage", "medianTurb", "waterTemp", "airTemp", "rain")
    Rnames <- c("timestamp", "timestamp", "recnum", "recnum", 
        "dump", "bottle", "cumbot", "code1", "code2", "stg", 
        "turb", "wtemp", "atemp", "rain")
    names(Rnames) <- basicnames
    firstline <- scan(pathname, what = "", sep = ",", nlines = 1)
    stn <- substring(firstline[2], 0, 3)
    headerlen <- switch(firstline[1], TOACI1 = 2, TOA5 = 4)
    headernames <- scan(pathname, skip = 1, nlines = 1, what = "", 
        sep = ",")
    message <- "Warning:"
    if (!all(headernames %in% basicnames)) {
        unfound <- headernames[!(headernames %in% basicnames)]
        message <- paste(message, "\nUnrecognized variable names found in header: ", 
            unfound)
    }
    varnames <- Rnames[headernames]
    varcounts <- count.fields(pathname, skip = headerlen, sep = ",")
    count.table <- table(varcounts)
    nvars <- length(varnames)
    if (length(count.table) > 1) {
        which.short <- which(varcounts < nvars)
        if (length(which.short) > 0) {
            message <- paste(message, "\nThe following lines are incomplete:", 
                paste(which.short + headerlen, collapse = ", "))
        }
        which.long <- which(varcounts > nvars)
        if (length(which.long) > 0) {
            message <- paste(message, "\nThe following lines have extra data:", 
                paste(which.long + headerlen, collapse = ", "))
        }
    }
    else if (nvars != as.numeric(names(count.table))) {
        message <- paste(message, "\nThe number of fields does not match the variables listed in the header line.")
    }
    minstg <- NA
    station.info <- try(read.table("labels.txt", sep = ",", fill = T, 
        header = F))
    if (inherits(station.info, "try-error")) {
        message <- paste(message, "\nUnable to find labels.txt file.")
    }
    else {
        stationlabels <- as.character(station.info[, 2])
        matches <- (tolower(stationlabels) == tolower(stn))
        if (sum(matches) == 0) 
            message <- paste(message, "\n", stn, " not found in labels.txt file.")
        if (sum(matches) > 1) 
            message <- paste(message, "\n", stn, " found multiple times in labels.txt file.")
        if (sum(matches) == 1) {
            labels <- drop(as.matrix(station.info[matches, ]))
            labels <- labels[labels != ""]
            labels <- tolower(labels)
            minstg <- labels[4]
            labels <- labels[5:length(labels)]
            if (length(labels) != length(varnames) || any(labels != 
                varnames)) 
                message <- paste(message, "\nVariables in header do not match labels.txt.")
        }
    }
    if (message != "Warning:") 
        winDialog(type = c("ok"), message)
    dump <- read.table(pathname, skip = headerlen, sep = ",", 
        col.names = varnames, fill = T)
    dump$chron <- crbasic.date(dump$timestamp)
    dump$time <- paste(substring(dump$timestamp, 12, 13), substring(dump$timestamp, 
        15, 16), sep = "")
    attr(dump, "stn") <- stn
    attr(dump, "minstg") <- as.numeric(minstg)
    if (!("bottle" %in% varnames) && ("cumbot" %in% varnames)) {
        bot.dif <- c(0, diff(dump$cumbot))
        dump$bottle <- dump$cumbot
        dump$bottle[bot.dif == 0] <- 0
    }
    dump[!is.na(dump$chron), ]
}, source = c("function (pathname) ", "{", "    # Read CRBasic formatted file", 
"    # Get the variable names from the header", "    # Oct 29, 2007: Made matching with info in labels.txt case-insensitive", 
"    basicnames <- c(\"TIMESTAMP\",\"TMSTAMP\",\"RECORD\",\"RECNBR\",\"dumpCount\",\"bottle\",\"cumbot\",\"threshCode\",\"sampleCode\",", 
"                  \"stage\",\"medianTurb\", \"waterTemp\",\"airTemp\",\"rain\")", 
"    Rnames <- c(\"timestamp\",\"timestamp\",\"recnum\",\"recnum\",\"dump\",\"bottle\",\"cumbot\",\"code1\",\"code2\",", 
"                  \"stg\",\"turb\",\"wtemp\",\"atemp\",\"rain\")", 
"    names(Rnames) <- basicnames", "    firstline <- scan(pathname,what=\"\",sep=\",\",nlines=1)", 
"    stn <- substring(firstline[2],0,3)", "    headerlen <- switch(firstline[1],", 
"         TOACI1 = 2,", "         TOA5 = 4)", "    headernames <- scan(pathname,skip=1,nlines=1,what=\"\",sep=\",\")", 
"    message <- \"Warning:\"", "    if (!all(headernames %in% basicnames)) {", 
"       unfound <- headernames[!(headernames %in% basicnames)]", 
"       message <- paste(message,\"\\nUnrecognized variable names found in header: \",unfound)", 
"    }", "    varnames <- Rnames[headernames]", "    varcounts<-count.fields(pathname,skip=headerlen,sep=\",\")", 
"    count.table <- table(varcounts)", "    nvars <- length(varnames)", 
"    if (length(count.table) > 1) {", "        which.short <- which(varcounts < nvars)", 
"        if (length(which.short) > 0) {", "                message <- paste(message,\"\\nThe following lines are incomplete:\", ", 
"                                 paste(which.short+headerlen,collapse=\", \"))", 
"        }", "        which.long <- which(varcounts > nvars)", 
"        if (length(which.long) > 0) {", "                message <- paste(message,\"\\nThe following lines have extra data:\", ", 
"                                 paste(which.long+headerlen,collapse=\", \"))", 
"        }", "    }", "    else if (nvars != as.numeric(names(count.table))) {", 
"        message <- paste(message,\"\\nThe number of fields does not match the variables listed in the header line.\")", 
"    }", "    # Attempt to assign minimum stage from labels.txt file", 
"    minstg <- NA", "    station.info <- try(read.table(\"labels.txt\",sep=\",\",fill=T,header=F))", 
"    if (inherits(station.info,\"try-error\")) {", "        message <- paste(message,\"\\nUnable to find labels.txt file.\")", 
"    }", "    else {", "        stationlabels <- as.character(station.info[,2])", 
"        matches <- (tolower(stationlabels) == tolower(stn))", 
"        # Identify ID problems in labels.txt file", "        if (sum(matches) == 0) message <- paste(message,\"\\n\",stn,\" not found in labels.txt file.\")", 
"        if (sum(matches) > 1) message <- paste(message,\"\\n\",stn,\" found multiple times in labels.txt file.\")", 
"        if (sum(matches) == 1) {", "           labels <- drop(as.matrix(station.info[matches, ]))", 
"           labels <- labels[labels != \"\"]", "           labels <- tolower(labels)", 
"           minstg <- labels[4]", "           labels <- labels[5:length(labels)]", 
"           if (length(labels) != length(varnames) || any(labels!=varnames)) ", 
"              message <- paste(message,\"\\nVariables in header do not match labels.txt.\")", 
"        }", "    }", "    if (message != \"Warning:\")", "        winDialog(type = c(\"ok\"), message)            ", 
"    dump<-read.table(pathname,skip=headerlen,sep=\",\",col.names=varnames,fill=T)    ", 
"    dump$chron<-crbasic.date(dump$timestamp)", "    dump$time <- paste(substring(dump$timestamp,12,13),substring(dump$timestamp,15,16),sep=\"\")", 
"    attr(dump,\"stn\") <- stn", "    attr(dump,\"minstg\") <- as.numeric(minstg)", 
"    # For stations that record only the highest (cumulative) bottle number", 
"    if (!(\"bottle\" %in% varnames) && (\"cumbot\" %in% varnames)) {", 
"        bot.dif <- c(0,diff(dump$cumbot))", "        dump$bottle <- dump$cumbot", 
"        dump$bottle[bot.dif == 0] <- 0", "    }     ", "    dump[!is.na(dump$chron), ]", 
"", "}"))
read.excel <-
structure(function (header = TRUE, ...) 
{
    read.table("clipboard", sep = "\t", header = header, ...)
}, source = c("function(header=TRUE, ...) {", "  read.table(\"clipboard\", sep=\"\\t\", header=header, ...)", 
"}"))
read.flo <-
structure(function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
    abs.path <- paste(ttshome, stn, file, sep = "/")
    columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "stg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    if (!file.exists(abs.path)) {
        print(paste(abs.path, "does not exist"))
        return(NULL)
    }
    print(paste("Reading file", abs.path))
    data <- try(read.table(abs.path, strip.white = T, sep = ",")[, 
        1:length(columns)])
    if (inherits(data, "try-error")) 
        return(data)
    names(data) <- columns
    data$chr <- make.chr(data$year, data$mo, data$dy, data$time)
    find.factors(data[, c("chr", "dump", "bottle", "stg", "stgcode", 
        "q", "turb", "turbcode")])
    data <- data[, c("chr", "dump", "bottle", "codes", "stg", 
        "stgcode", "q", "turb", "turbcode")]
    attr(data, "stn") <- stn
    data
}, source = c("function(stn, hy, ttshome = getTTSenv(\"TTSHOME\"))", 
"{", "# Revised Sep 2, 2012 added \"stn\" attribute to output data", 
"# Ported from Splus to R: June 8, 2005 (JL)", "# Reads new format flo files and discards any extra columns", 
"# No header line is permitted", "# Identifies character data in critical columns as errors.", 
"        file <- paste(stn, zfill(hy, 2), \".flo\", sep = \"\")", 
"        abs.path <- paste(ttshome, stn, file, sep=\"/\")", "        columns <- c(\"year\", \"mo\", \"dy\", \"time\", \"dump\", \"bottle\", \"codes\",", 
"                \"rawstg\", \"stg\", \"stgcode\", \"q\", \"rawturb\", \"turb\", \"turbcode\")", 
"        if (!file.exists(abs.path)) {", "            print(paste(abs.path,\"does not exist\"))", 
"            return(NULL)", "        }", "        print(paste(\"Reading file\",abs.path))", 
"        data <- try(read.table(abs.path, strip.white=T, sep = \",\")[, 1:length(columns)])", 
"        if (inherits(data,\"try-error\")) return(data)", "        names(data) <- columns", 
"        data$chr <- make.chr(data$year, data$mo, data$dy, data$time)", 
"        find.factors(data[, c(\"chr\",\"dump\",\"bottle\",\"stg\",\"stgcode\",\"q\",\"turb\",\"turbcode\")])", 
"        data <- data[, c(\"chr\",\"dump\",\"bottle\",\"codes\",\"stg\",\"stgcode\",\"q\",\"turb\",\"turbcode\")]", 
"        attr(data,\"stn\") <- stn    # Preserves station identity even if renamed later", 
"        data", "}"))
read.flo.vr <-
structure(function (file, path = ".") 
{
    full.name <- paste(path, file, sep = "\\")
    print(full.name)
    data <- read.table(full.name, sep = ",", skip = 1, fill = T, 
        as.is = 1:2)
    data <- data[, 1:4]
    names(data) <- c("date", "time", "stg", "q")
    data$chr <- chron(data$date, paste(data$time, "00", sep = ":"))
    find.factors(data[, c("stg", "q")])
    data[, c("chr", "stg", "q")]
}, source = c("function(file,path=\".\") {", "# Created Aug 30, 2004 (JL)", 
"# Reads a CSV file with one header line and whose first 4 columns are:", 
"#        date, time, level, discharge", "# e.g.   12/1/2003,0:05,0.005,0.0004", 
"# The header line and any extra columns are ignored.", "# Identifies character data in critical columns as errors.", 
"", "        full.name <- paste(path, file, sep=\"\\\\\")", "        print(full.name)", 
"        data <- read.table(full.name, sep = \",\", skip=1, fill=T,as.is=1:2)", 
"        data <- data[,1:4]", "        names(data) <- c(\"date\", \"time\", \"stg\", \"q\")", 
"\tdata$chr <- chron(data$date,paste(data$time,\"00\",sep=\":\"))", 
"\tfind.factors(data[, c(\"stg\",\"q\")])", "\tdata[,c(\"chr\",\"stg\",\"q\")]\t", 
"}"))
read.flofile <-
structure(function (flopath, extravars) 
{
    basic.columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "stg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    varnames <- c(basic.columns, extravars)
    nbasicvars <- length(basic.columns)
    nextravars <- length(extravars)
    nvars <- nbasicvars + nextravars
    varcounts <- count.fields(flopath, sep = ",")
    count.table <- table(varcounts)
    counts <- as.numeric(names(count.table))
    ncols <- max(counts)
    if (length(count.table) > 1) {
        change <- min(which(varcounts != varcounts[1]))
        message <- paste("\nThe number of fields in the flo file changed at line", 
            change)
        if (ncols == nvars) {
            message <- paste(message, "\nShort lines will be padded with blank fields.")
        }
        winDialog(type = c("ok"), paste("Warning:", message))
    }
    if (ncols <= nvars) {
        data <- read.csv(flopath, strip.white = T, fill = T, 
            col.names = varnames[1:ncols])
    }
    else {
        data <- read.csv(flopath, strip.white = T, fill = T)[, 
            1:nvars]
        names(data) <- varnames
    }
    data$chron <- make.chr(data$year, data$mo, data$dy, data$time)
    if (nextravars == 0) {
        if (ncols > nvars) {
            message <- paste("\nUnidentified extra variables will be ignored.")
            winDialog(type = c("ok"), paste("Warning:", message))
        }
        return(data[, c("chron", "bottle", "stg", "q", "turb")])
    }
    else if (ncols == nvars) {
        return(data[, c("chron", "bottle", "stg", "q", "turb", 
            extravars)])
    }
    else {
        message <- "\nThe number of extra fields is inconsistent with labels.txt."
        message <- paste(message, "\nPlease correct labels.txt and try again.")
        winDialog(type = c("ok"), paste("Aborting:", message))
        stop(message)
    }
}, source = c("function (flopath,extravars) {", "# Construct list of variable names", 
"        basic.columns <- c(\"year\", \"mo\", \"dy\", \"time\", \"dump\", \"bottle\", \"codes\",", 
"                \"rawstg\", \"stg\", \"stgcode\", \"q\", \"rawturb\", \"turb\", \"turbcode\")", 
"        varnames <- c(basic.columns, extravars)", "        nbasicvars <- length(basic.columns)", 
"        nextravars <- length(extravars)", "        nvars <- nbasicvars + nextravars", 
"", "# Report if number of fields in the data changes", "        varcounts <- count.fields(flopath,sep=\",\")", 
"        count.table <- table(varcounts)", "        counts <- as.numeric(names(count.table))", 
"        ncols <- max(counts)", "        if (length(count.table) > 1) {", 
"            change <- min(which(varcounts != varcounts[1]))", 
"            message <- paste(\"\\nThe number of fields in the flo file changed at line\",change)", 
"            if (ncols == nvars) {", "                message<- paste(message,\"\\nShort lines will be padded with blank fields.\") ", 
"            }", "            winDialog(type = c(\"ok\"), paste(\"Warning:\",message))", 
"        }", "", "# Read the data and return the variables needed for plotting", 
"        if (ncols <= nvars) {", "             data <- read.csv(flopath, strip.white=T, fill=T, col.names=varnames[1:ncols])", 
"        }", "        else {", "             data <- read.csv(flopath, strip.white=T, fill=T)[,1:nvars]", 
"             names(data) <- varnames", "        }", "        data$chron <- make.chr(data$year, data$mo, data$dy, data$time)", 
"        if (nextravars == 0) {", "#            Extra variables, if any, are not identified in labels.txt,", 
"#            or there may not be a labels.txt, so return only the basic variables", 
"             if (ncols > nvars) {", "                  message <- paste(\"\\nUnidentified extra variables will be ignored.\")", 
"                  winDialog(type = c(\"ok\"), paste(\"Warning:\",message))", 
"             }", "             return(data[, c(\"chron\",\"bottle\",\"stg\",\"q\",\"turb\")])", 
"        }", "        else if (ncols == nvars) {", "             return(data[, c(\"chron\",\"bottle\",\"stg\",\"q\",\"turb\",extravars)])", 
"        }", "        else {", "             message <- \"\\nThe number of extra fields is inconsistent with labels.txt.\"", 
"             message <- paste(message,\"\\nPlease correct labels.txt and try again.\")", 
"             winDialog(type=c(\"ok\"), paste(\"Aborting:\",message))", 
"             stop(message)", "        }", "}"))
read.flostages <-
structure(function () 
{
}, source = c("function () ", "{", "}"))
read.lab <-
structure(function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".isc", sep = "")
    rawdir <- paste("raw", zfill(hy, 2), sep = "")
    full.name <- paste(ttshome, stn, rawdir, file, sep = "/")
    if (!file.exists(full.name)) {
        print(paste(full.name, "does not exist"))
        return(NULL)
    }
    print(paste("Reading file", full.name))
    data <- try(read.table(full.name, strip.white = T, sep = ","))
    if (inherits(data, "try-error")) 
        return(data)
    ncols <- dim(data)[2]
    if (ncols == 3) {
        names(data) <- c("dump", "bottle", "ssc")
        data$labcodes <- rep(0, dim(data)[1])
    }
    else if (ncols >= 4) {
        data <- data[, 1:4]
        names(data) <- c("dump", "bottle", "ssc", "labcodes")
    }
    else {
        stop("Need at least 3 columns: dump, bottle, and ssc")
    }
    find.factors(data)
    data
}, source = c("function(stn, hy, ttshome = getTTSenv(\"TTSHOME\"))", 
"{", "# Revised Oct 26, 2007: no longer requires labcodes", "# Revised June 8, 2005", 
"# Reads the .isc file from the ttshome directory", "# The .isc file has no header and the first 3 columns are:", 
"#    dump, bottle, ssc", "# Optional: 4th column can contain labcodes", 
"#    If no labcodes are found, they will be created and initialized to 0", 
"# Identifies character data in critical columns as errors", 
"        file <- paste(stn, zfill(hy, 2), \".isc\", sep = \"\")", 
"        rawdir <- paste(\"raw\",zfill(hy, 2), sep=\"\")", "        full.name <- paste(ttshome, stn, rawdir, file, sep=\"/\")", 
"        if (!file.exists(full.name)) {", "            print(paste(full.name,\"does not exist\"))", 
"            return(NULL)", "        }", "        print(paste(\"Reading file\",full.name))", 
"        data <- try(read.table(full.name, strip.white=T, sep = \",\"))", 
"        if (inherits(data,\"try-error\")) return(data)", "        ncols <- dim(data)[2]", 
"        if (ncols == 3) {", "           names(data) <- c(\"dump\",\"bottle\",\"ssc\")", 
"           data$labcodes <- rep(0,dim(data)[1])", "        }", 
"        else if (ncols >= 4) {", "#          Discard columns after 4th", 
"           data <- data[,1:4]", "           names(data) <- c(\"dump\",\"bottle\",\"ssc\",\"labcodes\")", 
"        }", "        else {", "           stop(\"Need at least 3 columns: dump, bottle, and ssc\")", 
"        }  ", "        find.factors(data)", "        data", 
"}"))
read.lab.vr <-
structure(function (file, path = ".") 
{
    full.name <- paste(path, file, sep = "\\")
    print(full.name)
    data <- read.table(full.name, sep = ",", skip = 1, fill = T, 
        as.is = 1:2)
    data <- data[, 1:5]
    names(data) <- c("date", "time", "dump", "bottle", "ssc")
    data <- data[data$time != "NA", ]
    data <- data[!is.na(data$ssc), ]
    data$chr <- chron(data$date, paste(data$time, "00", sep = ":"))
    find.factors(data[, c("dump", "bottle", "ssc")])
    data[, c("chr", "dump", "bottle", "ssc")]
}, source = c("function(file,path=\".\") {", "# Created Aug 30, 2004 (JL)", 
"# Reads a CSV file with one header line and whose first 5 columns are:", 
"# date,time,dump,Bottle #,SSC", "# 11/18/2003,9:51,NA,1,NA", 
"# 12/6/2003,14:25,6,1,460", "# The header line and any extra columns are ignored.", 
"# Removes any lines with missing SSC", "", "        full.name <- paste(path, file, sep=\"\\\\\")", 
"        print(full.name)", "        data <- read.table(full.name, sep = \",\", skip=1, fill=T,as.is=1:2)", 
"        data <- data[,1:5]", "        names(data) <- c(\"date\", \"time\", \"dump\", \"bottle\", \"ssc\")", 
"\tdata <- data[data$time != \"NA\", ]", "\tdata <- data[!is.na(data$ssc), ]", 
"\tdata$chr <- chron(data$date,paste(data$time,\"00\",sep=\":\"))", 
"\tfind.factors(data[,c(\"dump\",\"bottle\",\"ssc\")])", "\tdata[,c(\"chr\",\"dump\",\"bottle\",\"ssc\")]\t", 
"}"))
read.mixed <-
structure(function (pathname) 
{
    library(chron)
    array.id <- zfill(scan(pathname, what = "", sep = ",", n = 1), 
        3)
    station.info <- scan("labels.txt", what = "")
    matches <- station.info[substring(station.info, 1, 3) == 
        array.id]
    if (length(matches) == 0) 
        stop("Array ID not found in labels.txt")
    if (length(matches) > 1) 
        stop("Array ID found multiple times in labels.txt")
    varnames <- unlist(strsplit(matches, ","))
    station <- varnames[2]
    minstg <- as.numeric(varnames[4])
    varnames <- varnames[5:length(varnames)]
    varcounts <- count.fields(pathname, sep = ",")
    count.table <- table(varcounts)
    count.sorted <- rev(sort(count.table))
    nvars <- as.numeric(names(count.sorted[1]))
    message <- "Warning:"
    if (length(count.table) > 1) {
        which.short <- which(varcounts < nvars)
        if (length(which.short) > 0) 
            message <- paste(message, "\nThe following lines are incomplete:", 
                paste(which.short, collapse = ", "))
        which.long <- which(varcounts > nvars)
        if (length(which.long) > 0) 
            message <- paste(message, "\nThe following lines have extra data:", 
                paste(which.long, collapse = ", "))
    }
    if (nvars != length(varnames)) {
        message <- paste(message, "\nThe number of fields does not agree with labels.txt.")
        while (nvars > length(varnames)) {
            newname <- paste("V", length(varnames) + 1, sep = "")
            varnames <- c(varnames, newname)
        }
    }
    if (message != "Warning:") 
        winDialog(type = c("ok"), message)
    dump <- read.table(pathname, sep = ",", col.names = varnames, 
        fill = T)
    dump$chron <- campbell.date(dump$year, dump$date, dump$time)
    attr(dump, "stn") <- station
    attr(dump, "minstg") <- minstg
    if (!("bottle" %in% varnames) && ("cumbot" %in% varnames)) {
        bot.dif <- c(0, diff(dump$cumbot))
        dump$bottle <- dump$cumbot
        dump$bottle[bot.dif == 0] <- 0
    }
    dump[!is.na(dump$chron), ]
}, source = c("function (pathname) ", "{", "  library(chron)", 
"  array.id <- zfill(scan(pathname,what=\"\",sep=\",\",n=1),3)    ", 
"  station.info <- scan(\"labels.txt\",what=\"\")", "  matches <- station.info[substring(station.info,1,3)==array.id]", 
"  if (length(matches) == 0) stop(\"Array ID not found in labels.txt\")", 
"  if (length(matches) > 1) stop(\"Array ID found multiple times in labels.txt\")", 
"  varnames <- unlist(strsplit(matches,\",\"))", "  station <- varnames[2]", 
"  minstg <- as.numeric(varnames[4])", "  varnames <- varnames[5:length(varnames)]", 
"  varcounts <- count.fields(pathname,sep=\",\")", "  count.table <- table(varcounts)", 
"  count.sorted <- rev(sort(count.table))", "  nvars <- as.numeric(names(count.sorted[1]))", 
"  message <- \"Warning:\"", "  if (length(count.table) > 1) {", 
"        which.short <- which(varcounts < nvars)", "        if (length(which.short) > 0)", 
"\t\tmessage <- paste(message,\"\\nThe following lines are incomplete:\", paste(which.short,collapse=\", \"))", 
"        which.long <- which(varcounts > nvars)", "        if (length(which.long) > 0)", 
"        \tmessage <- paste(message,\"\\nThe following lines have extra data:\", paste(which.long,collapse=\", \"))", 
"  }", "  if (nvars != length(varnames)) {", "\tmessage <- paste(message,\"\\nThe number of fields does not agree with labels.txt.\")", 
"        while (nvars > length(varnames)) {", "                newname <- paste(\"V\",length(varnames)+1,sep=\"\")", 
"                varnames <- c(varnames,newname)", "        }", 
"  }", "  if (message != \"Warning:\")", "\twinDialog(type = c(\"ok\"), message)            ", 
"  dump<-read.table(pathname,sep=\",\",col.names=varnames,fill=T)", 
"  dump$chron<-campbell.date(dump$year,dump$date,dump$time)", 
"  attr(dump,\"stn\") <- station", "  attr(dump,\"minstg\") <- minstg", 
"", "  # For stations that record only the highest (cumulative) bottle number", 
"  if (!(\"bottle\" %in% varnames) && (\"cumbot\" %in% varnames)) {", 
"        bot.dif <- c(0,diff(dump$cumbot))", "        dump$bottle <- dump$cumbot", 
"        dump$bottle[bot.dif == 0] <- 0", "  }     ", "  ", "  dump[!is.na(dump$chron), ]", 
"}"))
read.or <-
structure(function (stn, hy, choose.staff = F, ttshome = getTTSenv("TTSHOME")) 
{
    orfile <- paste(stn, zfill(hy, 2), ".or", sep = "")
    orpath <- paste(ttshome, stn, orfile, sep = "/")
    staffs <- staffnames(stn)
    staff2 <- gsub(" ", "", staffs)
    n <- length(staffs)
    if (choose.staff && n > 1) {
        default.staff <- staff2[pickone(staffs[-n], "staff plate")]
    }
    else {
        default.staff <- staff2[n]
        if (choose.staff) 
            cat(staffs[n], "is the only staff plate\n")
    }
    or <- read.csv(orpath, col.names = c("year", "mo", "dy", 
        "time", "dump", staff2))
    or$chr <- make.chr(or$year, or$mo, or$dy, or$time)
    flodat <- read.stages(stn, hy)
    or$rawstg <- interp.chron(flodat, or$chr, "rawstg")
    or$corstg <- interp.chron(flodat, or$chr, "corstg")
    or$staff <- or[, default.staff]
    or[, c("chr", "dump", "staff", "rawstg", "corstg")]
}, source = c("function(stn,hy,choose.staff=F,ttshome=getTTSenv(\"TTSHOME\")) {", 
"# Reads the OR file and joins both stages from the FLO file", 
"# Assumes there is just one staff plate reading", "# Returns a data frame with staff reading, raw stage, and corrected stage", 
"# If choose.staff=T, user chooses a staff plate from stationinfo.txt", 
"# Otherwise, the default staff plate from stationinfo.txt is used", 
"  orfile <- paste(stn,zfill(hy,2),\".or\",sep=\"\")", "  orpath <- paste(ttshome,stn,orfile,sep=\"/\")", 
"  staffs <- staffnames(stn)", "  # remove spaces from staff plate names", 
"  staff2 <- gsub(\" \",\"\",staffs)", "  n <- length(staffs)", 
"  if (choose.staff && n > 1) {", "     default.staff <- staff2[pickone(staffs[-n],\"staff plate\")]", 
"  }", "  else {", "     default.staff <- staff2[n]", "     if (choose.staff) cat(staffs[n],\"is the only staff plate\\n\")", 
"  }", "  or <- read.csv(orpath, col.names = c(\"year\",\"mo\",\"dy\",\"time\",\"dump\",staff2))", 
"  or$chr <- make.chr(or$year,or$mo,or$dy,or$time)", "  flodat <- read.stages(stn,hy)", 
"# Interpolate the stages from the FLO data", "  or$rawstg <- interp.chron(flodat,or$chr,\"rawstg\")", 
"  or$corstg <- interp.chron(flodat,or$chr,\"corstg\")", "  or$staff <- or[,default.staff]", 
"  or[,c(\"chr\",\"dump\",\"staff\",\"rawstg\",\"corstg\")]", 
"}"))
read.raw <-
structure(function (filepath) 
{
    filetype <- scan(filepath, what = "", n = 1)
    if (filetype %in% c("TOACI1", "TOA5")) 
        data <- read.crbasic(filepath)
    else data <- read.mixed(filepath)
}, source = c("function(filepath) {", "   # Read first word of file to determine file type, then call appropriate read function", 
"   # Default action is to read a mixed array compatible file", 
"   filetype <- scan(filepath,what=\"\",n=1)", "   if (filetype %in% c(\"TOACI1\",\"TOA5\"))", 
"        data <- read.crbasic(filepath)", "   else", "        data <- read.mixed(filepath)", 
"}"))
read.sand <-
structure(function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".sand", sep = "")
    rawdir <- paste("raw", zfill(hy, 2), sep = "")
    full.name <- paste(ttshome, stn, rawdir, file, sep = "/")
    print(full.name)
    data <- read.table(full.name, strip.white = T, sep = ",")
    ncols <- dim(data)[2]
    if (ncols == 4) {
        names(data) <- c("dump", "bottle", "sand", "totssc")
        data$labcodes <- rep(0, dim(data)[1])
    }
    else if (ncols >= 5) {
        data <- data[, 1:5]
        names(data) <- c("dump", "bottle", "sand", "totssc", 
            "labcodes")
    }
    else {
        stop("Need at least 4 columns: dump, bottle, sand, and totssc")
    }
    find.factors(data)
    data
}, source = c("function(stn, hy, ttshome = getTTSenv(\"TTSHOME\"))", 
"{", "# Created from read.lab, 07Mar2010", "#    read.sand reads ftr06.sand and similarly", 
"#    named files in which the first 3 columns are:", "#    dump, bottle, sand, ssc", 
"", "# Revised Oct 26, 2007: no longer requires labcodes", "# Revised June 8, 2005", 
"# Reads the .isc file from the ttshome directory", "# The .isc file has no header and the first 4 columns are:", 
"#    dump, bottle, sand, totssc", "# Optional: 5th column can contain labcodes", 
"#    If no labcodes are found, they will be created and initialized to 0", 
"# Identifies character data in critical columns as errors", 
"        file <- paste(stn, zfill(hy, 2), \".sand\", sep = \"\")", 
"        rawdir <- paste(\"raw\",zfill(hy, 2), sep=\"\")", "        full.name <- paste(ttshome, stn, rawdir, file, sep=\"/\")", 
"        print(full.name)", "        data <- read.table(full.name, strip.white=T, sep = \",\")", 
"        ncols <- dim(data)[2]", "        if (ncols == 4) {", 
"           names(data) <- c(\"dump\",\"bottle\",\"sand\",\"totssc\")", 
"           data$labcodes <- rep(0,dim(data)[1])", "        }", 
"        else if (ncols >= 5) {", "#          Discard columns after 5th", 
"           data <- data[,1:5]", "           names(data) <- c(\"dump\",\"bottle\",\"sand\",\"totssc\",\"labcodes\")", 
"        }", "        else {", "           stop(\"Need at least 4 columns: dump, bottle, sand, and totssc\")", 
"        }  ", "        find.factors(data)", "        data", 
"}"))
read.ssc <-
structure(function (stn, hy, path = paste(getTTSenv("TTSHOME"), 
    "tenminssc", sep = "/")) 
{
    fname <- paste(stn, zfill(hy, 2), "ssc.txt", sep = "")
    pathname <- paste(path, fname, sep = "/")
    data <- read.table(pathname, header = T, as.is = T)
    data$chr <- chron(data$DATE, paste(data$TIME, "00", sep = ":"))
    data
}, source = c("function (stn, hy, path = paste(getTTSenv(\"TTSHOME\"), \"tenminssc\", sep = \"/\")) ", 
"{", "# Read the 10-minute SSC file", "   fname <- paste(stn,zfill(hy,2),\"ssc.txt\",sep=\"\")", 
"   pathname <- paste(path,fname,sep=\"/\")", "   data <- read.table(pathname,header=T,as.is=T)", 
"   data$chr <- chron(data$DATE,paste(data$TIME,\"00\",sep=\":\"))", 
"   data", "}"))
read.stages <-
structure(function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
{
    file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
    abs.path <- paste(ttshome, stn, file, sep = "\\")
    print(abs.path)
    columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "corstg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    data <- read.table(abs.path, strip.white = T, sep = ",")[, 
        1:length(columns)]
    names(data) <- columns
    data$chr <- make.chr(data$year, data$mo, data$dy, data$time)
    find.factors(data[, c("chr", "dump", "rawstg", "corstg")])
    data[, c("chr", "dump", "rawstg", "corstg")]
}, source = c("function(stn, hy, ttshome =getTTSenv(\"TTSHOME\"))", 
"{", "# Adapted from read.flo to save only stages: Aug 18, 2008 (JL)", 
"# Reads new format flo files and discards any extra columns", 
"# No header line is permitted", "# Identifies character data in critical columns as errors.", 
"        file <- paste(stn, zfill(hy, 2), \".flo\", sep = \"\")", 
"        abs.path <- paste(ttshome, stn, file, sep=\"\\\\\")", 
"        print(abs.path)", "        columns <- c(\"year\", \"mo\", \"dy\", \"time\", \"dump\", \"bottle\", \"codes\",", 
"                \"rawstg\", \"corstg\", \"stgcode\", \"q\", \"rawturb\", \"turb\", \"turbcode\")", 
"        data <- read.table(abs.path, strip.white=T, sep = \",\")[, 1:length(columns)]", 
"        names(data) <- columns", "        data$chr <- make.chr(data$year, data$mo, data$dy, data$time)", 
"        find.factors(data[, c(\"chr\",\"dump\",\"rawstg\",\"corstg\")])", 
"        data[, c(\"chr\",\"dump\",\"rawstg\",\"corstg\")]", 
"}"))
read.storms <-
structure(function (stn, hy, alt = T) 
{
    hy <- zfill(hy, 2)
    if (alt) 
        filename <- paste(stn, hy, "storms.alt", sep = "")
    else filename <- paste(stn, hy, "storms.csv", sep = "")
    loc <- paste("sedloadestwork\\hy", hy, sep = "")
    file <- paste(loc, filename, sep = "\\")
    options(show.error.messages = F)
    dat <- try(read.table(file, sep = ","))
    options(show.error.messages = T)
    if (class(dat) == "try-error") {
        if (alt) 
            return(read.storms(stn, hy, alt = F))
        else stop(paste("Cannot find or read", file))
    }
    cat(paste("Storm dates read from file ", filename, "\n", 
        sep = "'"))
    names(dat) <- c("number", "sdate", "stime", "edate", "etime")
    schron <- ymd2date(dat$sdate) + mt2msm(dat$stime)/1440
    echron <- ymd2date(dat$edate) + mt2msm(dat$etime)/1440
    data.frame(number = dat$number, schron, echron)
}, source = c("function(stn, hy, alt=T)", "{", "# Reads a text file containing storm numbers and start/end times", 
"# file must be named stnhystorms.csv, e.g. nfc05storms.csv", 
"# file must be located in sedloadestwork\\hynn, e.g. sedloadestwork\\hy05", 
"# Returns a data frame", "\thy <- zfill(hy, 2)", "\tif (alt)", 
"\t\tfilename <- paste(stn,hy,\"storms.alt\",sep=\"\")", "\telse", 
"\t\tfilename <- paste(stn,hy,\"storms.csv\",sep=\"\")", "\tloc <- paste(\"sedloadestwork\\\\hy\",hy,sep=\"\")", 
"\tfile <- paste(loc,filename,sep=\"\\\\\")", "\toptions(show.error.messages=F)", 
"\tdat <- try(read.table(file, sep = \",\"))", "\toptions(show.error.messages=T)", 
"\tif (class(dat) == \"try-error\") {", "\t\tif (alt) ", "\t\t\treturn(read.storms(stn,hy,alt=F))", 
"\t\telse", "\t\t\tstop(paste(\"Cannot find or read\",file))", 
"\t}", "\tcat(paste(\"Storm dates read from file \",filename,\"\\n\",sep=\"'\"))", 
"\tnames(dat) <- c(\"number\", \"sdate\", \"stime\", \"edate\", \"etime\")", 
"\tschron <- ymd2date(dat$sdate) + mt2msm(dat$stime)/1440", "\techron <- ymd2date(dat$edate) + mt2msm(dat$etime)/1440", 
"\tdata.frame(number = dat$number, schron, echron)", "}"))
readAndMerge.gui <-
structure(function () 
{
    readMerge <- function(panel) {
        with(panel, {
            stn <- tolower(pars[1])
            hy4 <- pars[2]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- substring(hy4, 3, 4)
            setTTSenv(c("STN", "HY"), c(stn, hy4))
            ttshome <- getTTSenv("TTSHOME")
            cmd <- paste("read.flo(\"", stn, "\",", hy2, ")\n", 
                "read.lab(\"", stn, "\",", hy2, ")\n", "merge.flo(\"", 
                stn, "\",", hy2, ")\n", "mismatches(\"", stn, 
                "\",", hy2, ")\n", sep = "")
            saveCommand(stn, hy2, save = savecmd, cmd = cmd)
            flodata <- read.flo(stn, hy2, ttshome)
            if (is.data.frame(flodata)) {
                objname <- paste(stn, hy2, ".flo", sep = "")
                assign(objname, flodata, envir = .GlobalEnv)
                print(paste("Data imported to object", objname))
            }
            labdata <- read.lab(stn, hy2, ttshome)
            if (is.data.frame(labdata)) {
                objname <- paste(stn, hy2, ".lab", sep = "")
                assign(objname, labdata, envir = .GlobalEnv)
                print(paste("Data imported to object", objname))
            }
            seddata <- merge.flo(stn, hy2)
            objname <- paste(stn, hy2, ".sed", sep = "")
            assign(objname, seddata, envir = .GlobalEnv)
            print(paste("Flo and lab data merged to object", 
                objname))
            print(mismatches(stn, hy2))
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control("Read and Merge Data")
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save command to file", 
        action = nothing)
    res1 <- rp.button(panel, action = readMerge, title = "OK", 
        pos = "left", quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates GUI using rpanel package to read and merge TTS (flo and lab) data", 
"  # Creates lab, flo, and sed data frames; and identifies mismatches after merging ", 
"  readMerge <- function(panel) {", "    with(panel, {", "      stn <- tolower(pars[1])", 
"      hy4 <- pars[2]", "      if (nchar(hy4) < 3) {", "        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- substring(hy4,3,4)", "      # Save selected station and year in TTS environment", 
"      setTTSenv(c(\"STN\",\"HY\"),c(stn,hy4))", "      # Get TTSHOME environment variable", 
"      ttshome <- getTTSenv(\"TTSHOME\")", "      # Create commands for display and saving to command file", 
"      cmd <- paste('read.flo(\"',stn,'\",',hy2,')\\n',", "                   'read.lab(\"',stn,'\",',hy2,')\\n',", 
"                   'merge.flo(\"',stn,'\",',hy2,')\\n',", "                   'mismatches(\"',stn,'\",',hy2,')\\n',sep=\"\")", 
"      saveCommand(stn,hy2,save=savecmd,cmd=cmd)", "", "      # Read the flo file", 
"      flodata <- read.flo(stn,hy2,ttshome)", "      if (is.data.frame(flodata)) {", 
"        objname <- paste(stn,hy2,\".flo\",sep=\"\")", "        assign(objname,flodata,envir=.GlobalEnv)", 
"        print(paste(\"Data imported to object\",objname))", 
"      }", "      # Read the ISCO file", "      labdata <- read.lab(stn,hy2,ttshome)", 
"      if (is.data.frame(labdata)) {", "        objname <- paste(stn,hy2,\".lab\",sep=\"\")", 
"        assign(objname,labdata,envir=.GlobalEnv)", "        print(paste(\"Data imported to object\",objname))", 
"      }", "      # Merge to sed object", "      seddata <- merge.flo(stn,hy2)", 
"      objname <- paste(stn,hy2,\".sed\",sep=\"\")", "      assign(objname,seddata,envir=.GlobalEnv)", 
"      print(paste(\"Flo and lab data merged to object\",objname))", 
"      # Display mismatches ", "      print(mismatches(stn,hy2))", 
"    })", "    panel", "  }", "  nothing <- function(panel) panel", 
"  panel <- rp.control(\"Read and Merge Data\")", "  env <- getTTSenv()", 
"  init.stn <- getTTSenv(\"STN\")", "  init.hy <- getTTSenv(\"HY\")", 
"  my.textentry(panel, pars, action = nothing, labels=c(\"Station\",\"Water year\"), ", 
"               title=\"Select station and year\",initval = c(init.stn, init.hy))", 
"  rp.checkbox(panel, savecmd, initval = TRUE, ", "              label = \"Save command to file\",action=nothing)", 
"  res1 <- rp.button(panel, action = readMerge, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
redraw <-
structure(function (panel) 
{
    rp.tkrreplot(panel, tkrp)
    panel
}, source = c("function(panel) {", "      rp.tkrreplot(panel, tkrp)", 
"      panel", "      }"))
regmat <-
structure(function (formula, xpro = 0.78, ypro = 0.92, ...) 
{
    splom(formula, pscales = 0, panel = function(x, y) {
        panel.xyplot(x, y)
        panel.lmline(x, y)
        fit <- lm(y ~ x)
        r2 <- round(summary(fit)$r.squared, 3)
        s <- round(summary(fit)$sigma, 3)
        expr1 <- substitute(paste(r^2, "=", r2), list(r2 = r2))
        expr2 <- substitute(paste("s =", s), list(s = s))
        cpl <- current.panel.limits()
        x1 <- xpro * cpl$xlim[1] + (1 - xpro) * cpl$xlim[2]
        y1 <- ypro * cpl$ylim[2] + (1 - ypro) * cpl$ylim[1]
        y2 <- (ypro - 0.1) * cpl$ylim[2] + (1.1 - ypro) * cpl$ylim[1]
        panel.text(x1, y1, expr1, cex = 0.75)
        panel.text(x1, y2, expr2, cex = 0.75)
    }, ...)
}, source = c("function(formula,xpro=0.78,ypro=0.92, ...) {", 
"   splom(formula, pscales=0, panel = function(x,y) {", "     panel.xyplot(x,y)", 
"     panel.lmline(x,y)", "     fit <- lm(y~x)", "     r2 <- round(summary(fit)$r.squared,3)", 
"     s <- round(summary(fit)$sigma,3)", "     expr1 <- substitute(paste(r^2,\"=\",r2),list(r2=r2))", 
"     expr2 <- substitute(paste(\"s =\",s),list(s=s))", "     cpl <- current.panel.limits()", 
"     x1 <- xpro*cpl$xlim[1] + (1-xpro)*cpl$xlim[2]", "     y1 <- ypro*cpl$ylim[2] + (1-ypro)*cpl$ylim[1]", 
"     y2 <- (ypro-0.10)*cpl$ylim[2] + (1.10-ypro)*cpl$ylim[1]", 
"     panel.text(x1,y1,expr1,cex=0.75)", "     panel.text(x1,y2,expr2,cex=0.75)", 
"  }, ...)", "}"))
removettsMenu <-
structure(function () 
winMenuDel("TTS"), source = "function() winMenuDel(\"TTS\")")
reselData.gui <-
structure(function () 
{
    reselectSamples <- function(panel) {
        with(panel, {
            stn <- tolower(pars[1])
            hy4 <- pars[2]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- substring(hy4, 3, 4)
            setTTSenv(c("STN", "HY", "INTERSTORM"), c(stn, hy4, 
                interstorm))
            ttshome <- getTTSenv("TTSHOME")
            sta <- check.interstorm(stn, hy2, interstorm, surrogate, 
                checkflo = F)
            objname <- paste(sta, hy2, ".sed", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                cat("ERROR: ", objname, "not found\n")
            seddata <- eval(as.name(objname))
            subset <- checkDumpDate(seddata, panel)
            if (surrogate == "turbidity") 
                xvar <- "turb"
            else xvar <- "q"
            if (is.null(subset)) 
                seddata <- reselect(seddata, xvar, "ssc", objname)
            else seddata <- reselect(seddata, xvar, "ssc", objname, 
                subset)
            assign(objname, seddata, envir = .GlobalEnv)
            cmd <- paste(objname, "$exclude <- logical(dim(", 
                objname, ")[1])\n", objname, "$exclude[c(", paste(which(seddata$exclude), 
                  collapse = ","), ")] <- T", sep = "")
            saveCommand(stn, hy2, save = savecmd, cmd = cmd)
        })
        panel
    }
    checkDumpDate <- function(data, panel) {
        with(panel, {
            if (dump.date == "dump") {
                sdump <- as.numeric(start.end[1])
                edump <- as.numeric(start.end[2])
                if (sdump > edump) 
                  stop("End dump cannot precede start dump")
                if (sdump > max(data$dump)) 
                  stop("Start dump exceeds data maximum")
                if (edump < min(data$dump)) 
                  stop("End dump is less than data minimum")
                setTTSenv(c("SDUMP", "EDUMP"), c(sdump, edump))
                subset <- which(data$dump >= sdump & data$dump <= 
                  edump)
                if (length(subset) > 0) 
                  return(subset)
                else return(NULL)
            }
            else if (dump.date == "date") {
                schr <- parseDate(start.end[1])
                echr <- parseDate(start.end[2])
                if (schr > echr) 
                  stop("End time cannot precede start time")
                if (schr > max(data$chr)) 
                  stop("Start time exceeds data maximum")
                if (echr < min(data$chr)) 
                  stop("End time exceeds data minimum")
                subset <- which(data$chr >= schr & data$chr <= 
                  echr)
                sdate <- substring(drop.parens(format(schr)), 
                  1, 14)
                edate <- substring(drop.parens(format(echr)), 
                  1, 14)
                setTTSenv(c("SDATE", "EDATE"), c(sdate, edate))
                if (length(subset) > 0) 
                  return(subset)
                else return(NULL)
            }
            else return(NULL)
        })
    }
    nothing <- function(panel) panel
    panel <- rp.control("Identify samples for exclusion")
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    init.sdate <- getTTSenv("SDATE")
    init.edate <- getTTSenv("EDATE")
    if (is.na(init.sdate)) 
        init.sdate <- "mm/dd/yy hh:mm:ss"
    if (is.na(init.edate)) 
        init.edate <- "mm/dd/yy hh:mm:ss"
    init.interstorm <- getTTSenv("INTERSTORM")
    rp.radiogroup(panel, surrogate, c("turbidity", "flow"), title = "Sediment surrogate", 
        action = nothing)
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy), pos = "top")
    rp.radiogroup(panel, dump.date, c("date", "dump", "neither"), 
        c("date", "dump", "neither"), title = "Mode for limiting samples to be displayed")
    my.textentry(panel, start.end, action = nothing, labels = c("Start", 
        "End"), title = "Specify limits according to selected mode", 
        initval = c(init.sdate, init.edate))
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save command to file", 
        action = nothing)
    rp.checkbox(panel, interstorm, initval = init.interstorm, 
        label = "Interstorm data only", action = nothing)
    res1 <- rp.button(panel, action = reselectSamples, title = "OK", 
        pos = "left", quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to flag sed data for omission from subsequent analyses", 
"  # Adds exclude column to data frame and sets value to T for points selected by user", 
"  # Most other functions will exclude such points if their argument list also has exclude = T", 
"  reselectSamples <- function(panel) {", "    with(panel, {", 
"      stn <- tolower(pars[1])", "      hy4 <- pars[2]", "      if (nchar(hy4) < 3) {", 
"        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- substring(hy4,3,4)", "      # Save selected station and year in TTS environment", 
"      setTTSenv(c(\"STN\",\"HY\",\"INTERSTORM\"),c(stn,hy4,interstorm))", 
"      # Get TTSHOME environment variable", "      ttshome <- getTTSenv(\"TTSHOME\")", 
"      sta <- check.interstorm(stn,hy2,interstorm,surrogate,checkflo=F)", 
"      objname <- paste(sta,hy2,\".sed\",sep=\"\")", "      if (!exists(objname,env=.GlobalEnv)) ", 
"        cat(\"ERROR: \",objname,\"not found\\n\")", "      seddata <- eval(as.name(objname))", 
"      subset <- checkDumpDate(seddata,panel)", "      if (surrogate ==\"turbidity\")", 
"        xvar <- \"turb\"", "      else", "        xvar <- \"q\"", 
"      if (is.null(subset))", "        seddata <- reselect(seddata,xvar,\"ssc\",objname)", 
"      else", "        seddata <- reselect(seddata,xvar,\"ssc\",objname,subset)", 
"      assign(objname,seddata,envir=.GlobalEnv)", "      cmd <- paste(objname,\"$exclude <- logical(dim(\",objname,\")[1])\\n\",", 
"                   objname,\"$exclude[c(\",paste(which(seddata$exclude),collapse=\",\"),\")] <- T\",sep=\"\")", 
"      saveCommand(stn,hy2,save=savecmd,cmd=cmd)", "    })", 
"    panel", "  }", "  ", "  checkDumpDate <- function(data,panel) {", 
"    with(panel, {", "      if (dump.date == \"dump\") {", "        sdump <- as.numeric(start.end[1])", 
"        edump <- as.numeric(start.end[2])", "        if (sdump > edump) stop(\"End dump cannot precede start dump\")", 
"        if (sdump > max(data$dump)) stop(\"Start dump exceeds data maximum\")", 
"        if (edump < min(data$dump)) stop(\"End dump is less than data minimum\")", 
"        setTTSenv(c(\"SDUMP\",\"EDUMP\"),c(sdump,edump))", "        subset <- which(data$dump >= sdump & data$dump <= edump)", 
"        if (length(subset) > 0) return(subset)", "        else return(NULL)", 
"      }", "      else if (dump.date == \"date\") {", "        schr <- parseDate(start.end[1])", 
"        echr <- parseDate(start.end[2])", "        if (schr > echr) stop(\"End time cannot precede start time\")", 
"        if (schr > max(data$chr)) stop(\"Start time exceeds data maximum\")", 
"        if (echr < min(data$chr)) stop(\"End time exceeds data minimum\")", 
"        subset <- which(data$chr >= schr & data$chr <= echr)", 
"        sdate <- substring(drop.parens(format(schr)),1,14)", 
"        edate <- substring(drop.parens(format(echr)),1,14)", 
"        setTTSenv(c(\"SDATE\",\"EDATE\"),c(sdate,edate))", "        if (length(subset) > 0) return(subset)", 
"        else return(NULL)", "      }", "      else return(NULL)", 
"    })", "  }", "  ", "  nothing <- function(panel) panel", 
"  panel <- rp.control(\"Identify samples for exclusion\")", 
"  env <- getTTSenv()", "  init.stn <- getTTSenv(\"STN\")", "  init.hy <- getTTSenv(\"HY\")", 
"  init.sdate <- getTTSenv(\"SDATE\")", "  init.edate <- getTTSenv(\"EDATE\")", 
"  if (is.na(init.sdate)) init.sdate <- \"mm/dd/yy hh:mm:ss\"", 
"  if (is.na(init.edate)) init.edate <- \"mm/dd/yy hh:mm:ss\"", 
"  init.interstorm <- getTTSenv(\"INTERSTORM\")", "  ", "  rp.radiogroup(panel, surrogate, c(\"turbidity\",\"flow\"),title=\"Sediment surrogate\",action=nothing)", 
"  my.textentry(panel, pars, action = nothing, labels=c(\"Station\",\"Water year\"), ", 
"               title=\"Select station and year\",initval = c(init.stn, init.hy),", 
"               pos=\"top\")", "  rp.radiogroup(panel,dump.date,c(\"date\",\"dump\",\"neither\"),c(\"date\",\"dump\",\"neither\"),", 
"                title=\"Mode for limiting samples to be displayed\")", 
"  my.textentry(panel,start.end, action = nothing, labels=c(\"Start\",\"End\"),", 
"               title=\"Specify limits according to selected mode\",initval= c(init.sdate,init.edate))", 
"  rp.checkbox(panel, savecmd, initval = TRUE, ", "                label = \"Save command to file\",action=nothing)", 
"  rp.checkbox(panel, interstorm, initval = init.interstorm, ", 
"              label = \"Interstorm data only\",action=nothing)", 
"  res1 <- rp.button(panel, action = reselectSamples, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
reselData.simple <-
structure(function (type) 
{
    reselectSamples <- function(panel) {
        with(panel, {
            stn <- tolower(pars[1])
            hy4 <- pars[2]
            hy2 <- substring(hy4, 3, 4)
            setTTSenv(c("STN", "HY"), c(stn, hy4))
            ttshome <- getTTSenv("TTSHOME")
            objname <- paste(stn, hy2, ".sed", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                stop(paste(objname, "not found"))
            seddata <- eval(as.name(objname))
            seddata <- reselect(seddata, "turb", "ssc", type, 
                objname)
            assign(objname, seddata, envir = .GlobalEnv)
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control(paste(type, "samples"), type = type)
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    res1 <- rp.button(panel, action = reselectSamples, title = "OK", 
        pos = "left", quit = TRUE)
    rp.button(panel, action = nothing, title = "Cancel", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function(type) {", "   reselectSamples <- function(panel) {", 
"      with(panel, {", "         stn <- tolower(pars[1])", "         hy4 <- pars[2]", 
"         hy2 <- substring(hy4,3,4)", "         # Save selected station and year in TTS environment", 
"         setTTSenv(c(\"STN\",\"HY\"),c(stn,hy4))", "         # Get TTSHOME environment variable", 
"         ttshome <- getTTSenv(\"TTSHOME\")", "         objname <- paste(stn,hy2,\".sed\",sep=\"\")", 
"         if (!exists(objname,env=.GlobalEnv)) stop(paste(objname,\"not found\"))", 
"         seddata <- eval(as.name(objname))", "         seddata <- reselect(seddata,\"turb\",\"ssc\",type,objname)", 
"         assign(objname,seddata,envir=.GlobalEnv)", "      })", 
"   panel", "   }", "   nothing <- function(panel) panel", "   panel <- rp.control(paste(type,\"samples\"),type=type)", 
"   env <- getTTSenv()", "   init.stn <- getTTSenv(\"STN\")", 
"   init.hy <- getTTSenv(\"HY\")", "   my.textentry(panel, pars, action = nothing, labels=c(\"Station\",\"Water year\"), ", 
"       title=\"Select station and year\",initval = c(init.stn, init.hy))", 
"   res1 <- rp.button(panel, action = reselectSamples, title=\"OK\", pos=\"left\",quit=TRUE)", 
"   rp.button(panel, action = nothing, title=\"Cancel\", pos=\"right\", quit=TRUE)", 
"   rp.do(panel,nothing)", "}"))
reselect <-
structure(function (data, xvar, yvar, dataname, subset) 
{
    if (!(xvar %in% names(data))) 
        stop(paste(xvar, "is not a column in", dataname))
    if (!(yvar %in% names(data))) 
        stop(paste(yvar, "is not a column in", dataname))
    cat("Click points for inclusion/exclusion, use middle mouse button to quit\n")
    if (!("exclude" %in% names(data))) 
        data$exclude <- logical(dim(data)[1])
    if (missing(subset)) 
        subdata <- data
    else subdata <- data[subset, ]
    repeat {
        plot(subdata[, xvar], subdata[, yvar], pch = 1, xlab = xvar, 
            ylab = yvar, main = dataname)
        points(subdata[subdata$exclude, xvar], subdata[subdata$exclude, 
            yvar], pch = 13, col = 2)
        k <- identify(subdata[, xvar], subdata[, yvar], row.names(subdata), 
            n = 1)
        if (length(k) != 0) 
            subdata$exclude[k] <- !subdata$exclude[k]
        else break
    }
    data$exclude[subset] <- subdata$exclude
    if (sum(data$exclude) > 0) {
        cat("The following items in", dataname, "are now tagged for exclusion:\n")
        print(data[data$exclude, 1:5])
    }
    else cat("No items in", dataname, "are now tagged for exclusion\n")
    data
}, source = c("function(data, xvar, yvar, dataname, subset) {", 
"#  Adds or modifies the \"exclude\" column in sediment dataset", 
"#  Changes TRUE to FALSE or FALSE to TRUE for data points that are clicked on", 
"   if (!(xvar %in% names(data))) stop(paste(xvar,\"is not a column in\", dataname))", 
"   if (!(yvar %in% names(data))) stop(paste(yvar,\"is not a column in\", dataname))", 
"   cat(\"Click points for inclusion/exclusion, use middle mouse button to quit\\n\")", 
"   if (!(\"exclude\" %in% names(data)))", "      data$exclude <- logical(dim(data)[1])", 
"   if (missing(subset))", "      subdata <- data", "   else subdata <- data[subset, ]", 
"   repeat {", "      plot(subdata[,xvar], subdata[,yvar], pch=1, xlab=xvar, ylab=yvar, main=dataname)", 
"      points(subdata[subdata$exclude,xvar], subdata[subdata$exclude,yvar], pch=13, col=2)", 
"      k <- identify(subdata[,xvar], subdata[,yvar], row.names(subdata), n=1)", 
"      if (length(k) !=0) subdata$exclude[k] <- !subdata$exclude[k]", 
"      else break", "   }", "   data$exclude[subset] <- subdata$exclude", 
"   if (sum(data$exclude) > 0) {", "      cat(\"The following items in\",dataname,\"are now tagged for exclusion:\\n\")", 
"      print(data[data$exclude,1:5])", "   }", "   else", "      cat(\"No items in\",dataname,\"are now tagged for exclusion\\n\")", 
"   data", "}"))
reselect.simple <-
structure(function (data, xvar, yvar, type = "Omit", dataname) 
{
    if (!(xvar %in% names(data))) 
        stop(paste(xvar, "is not a column in", dataname))
    if (!(yvar %in% names(data))) 
        stop(paste(yvar, "is not a column in", dataname))
    if (!("exclude" %in% names(data))) 
        data$exclude <- logical(dim(data)[1])
    repeat {
        plot(data[, xvar], data[, yvar], pch = 1, xlab = xvar, 
            ylab = yvar, main = dataname)
        points(data[data$exclude, xvar], data[data$exclude, yvar], 
            pch = 13, col = 2)
        k <- identify(data[, xvar], data[, yvar], n = 1)
        if (length(k) != 0) 
            data$exclude[k] <- ifelse(type == "Omit", TRUE, FALSE)
        else break
    }
    if (sum(data$exclude) > 0) {
        cat("The following items in", dataname, "are now tagged for exclusion:\n")
        print(data[data$exclude, 1:5])
    }
    else cat("No items in", dataname, "are now tagged for exclusion\n")
    data
}, source = c("function(data, xvar, yvar, type=\"Omit\", dataname) {", 
"   if (!(xvar %in% names(data))) stop(paste(xvar,\"is not a column in\", dataname))", 
"   if (!(yvar %in% names(data))) stop(paste(yvar,\"is not a column in\", dataname))", 
"   if (!(\"exclude\" %in% names(data)))", "      data$exclude <- logical(dim(data)[1])", 
"   repeat {", "      plot(data[,xvar], data[,yvar], pch=1, xlab=xvar, ylab=yvar, main=dataname)", 
"      points(data[data$exclude,xvar], data[data$exclude,yvar], pch=13, col=2)", 
"      k <- identify(data[,xvar], data[,yvar], n=1)", "      if (length(k) !=0)  data$exclude[k] <- ifelse(type==\"Omit\",TRUE,FALSE)", 
"      else break", "   }", "   if (sum(data$exclude) > 0) {", 
"      cat(\"The following items in\",dataname,\"are now tagged for exclusion:\\n\")", 
"      print(data[data$exclude,1:5])", "   }", "   else", "      cat(\"No items in\",dataname,\"are now tagged for exclusion\\n\")", 
"   data", "}"))
rmse <-
structure(function (x, y) 
sqrt(mean((y - x)^2)), source = "function(x,y) sqrt(mean((y-x)^2))")
round2tenmin <-
structure(function (x, dir = "nearest") 
{
    interval <- 144 * as.numeric(x)
    if (dir == "nearest") {
        t <- round(interval)
    }
    else if (dir == "down") {
        t <- floor(interval)
    }
    else if (dir == "up") {
        t <- ceiling(interval)
    }
    else stop("dir must be 'nearest', 'up', or 'down'")
    chron(t/144)
}, source = c("function(x,dir=\"nearest\") {", "  # Rounds a chron object to 10-minute interval", 
"  # Rounding options are nearest, down, or up", "  interval <- 144*as.numeric(x)", 
"  if (dir==\"nearest\") {", "    t <- round(interval)", "  }", 
"  else if (dir==\"down\") {", "    t <- floor(interval)", "  }", 
"  else if (dir==\"up\") {", "    t <- ceiling(interval)", "  }", 
"  else stop(\"dir must be 'nearest', 'up', or 'down'\")", "  chron(t/144)", 
"}"))
saveCommand <-
structure(function (stn, hy2, funcname, arglist, result, savetofile, 
    cmd = "") 
{
    if (savetofile) {
        home <- getTTSenv("TTSHOME")
        stndir <- paste(home, stn, sep = "/")
        if (!file.exists(stndir)) {
            dir.create(cmddir)
            cat("Created", stndir, "\n")
        }
        cmddir <- paste(stndir, "R commands", sep = "/")
        if (!file.exists(cmddir)) {
            dir.create(cmddir)
            cat("Created", cmddir, "\n")
        }
        if (file.info(cmddir)$isdir) {
            cmdfile <- paste(stn, zfill(hy2, 2), "cmd.R", sep = "")
            filepath <- paste(cmddir, cmdfile, sep = "/")
            writecmd(file = filepath, funcname, arglist, result, 
                cmd)
        }
        else {
            cat("Error: we need 'R commands' to be a directory (it is currently a file)\n")
            writecmd(file = "", funcname, arglist, result, cmd)
        }
    }
    else {
        writecmd(file = "", funcname, arglist, result, cmd)
    }
}, source = c("function(stn,hy2,funcname,arglist,result,savetofile,cmd=\"\") {", 
"  if (savetofile) {", "    # Check for existence of command directory and create if needed", 
"    home <- getTTSenv(\"TTSHOME\")", "    stndir <- paste(home,stn,sep=\"/\")", 
"    if (!file.exists(stndir)) {", "      dir.create(cmddir)", 
"      cat(\"Created\",stndir,\"\\n\")", "    }", "    cmddir <- paste(stndir,\"R commands\",sep=\"/\")", 
"    if (!file.exists(cmddir)) {", "      dir.create(cmddir)", 
"      cat(\"Created\",cmddir,\"\\n\")", "    }", "    if (file.info(cmddir)$isdir) { ", 
"      # Write the command to the command file", "      cmdfile <- paste(stn,zfill(hy2,2),\"cmd.R\",sep=\"\")", 
"      filepath <- paste(cmddir,cmdfile,sep=\"/\")", "      writecmd(file=filepath,funcname,arglist,result,cmd)", 
"    }", "    else {", "      cat(\"Error: we need 'R commands' to be a directory (it is currently a file)\\n\")", 
"      writecmd(file=\"\",funcname,arglist,result,cmd)  # Just echo command to screen", 
"    }", "  }", "  else {", "    writecmd(file=\"\",funcname,arglist,result,cmd)  # Just echo command to screen", 
"  }", "}"))
scatterPlot.gui <-
structure(function () 
{
    scatPlot <- function(panel) {
        with(panel, {
            stn <- tolower(pars["stn"])
            hy4 <- pars["hy4"]
            startdt <- pars["startdt"]
            lastdt <- pars["lastdt"]
            dumpexpr <- as.vector(pars["dumpexpr"])
            botexpr <- as.vector(pars["botexpr"])
            result <- pars["result"]
            startgrid <- par("mfrow")
            mygrid <- as.numeric(unlist(strsplit(layout, "x")))
            if (any(mygrid != startgrid)) {
                par(mfrow = mygrid)
            }
            interstorm <- checkvars["interstorm"]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- as.vector(substring(hy4, 3, 4))
            setTTSenv(c("STN", "HY", "SDATE", "EDATE", "DUMPS", 
                "BOTTLES", "INTERSTORM"), c(stn, hy4, startdt, 
                lastdt, dumpexpr, botexpr, interstorm))
            arglist <- list(txt = plotsym, type = type, exclude = checkvars["exclude"])
            sta <- check.interstorm(stn, hy2, interstorm, surrogate, 
                checkflo = F)
            if (type == "loess") {
                span <- as.numeric(loesspars["span"])
                degree <- as.numeric(loesspars["degree"])
                arglist <- c(arglist, span = span, degree = degree)
            }
            if (subsetby == "date/time") {
                sdate <- paste(substring(startdt, 7, 8), substring(startdt, 
                  1, 2), substring(startdt, 4, 5), sep = "")
                stime <- paste(substring(startdt, 10, 11), substring(startdt, 
                  13, 14), sep = "")
                edate <- paste(substring(lastdt, 7, 8), substring(lastdt, 
                  1, 2), substring(lastdt, 4, 5), sep = "")
                etime <- paste(substring(lastdt, 10, 11), substring(lastdt, 
                  13, 14), sep = "")
                arglist <- c(sta, hy2, sdate, stime, edate, etime, 
                  arglist)
            }
            else if (subsetby == "dump/bottle") {
                dumps = eval(parse(text = dumpexpr))
                bots = eval(parse(text = botexpr))
                if (botexpr == "") 
                  arglist <- c(sta, hy2, dumpstr = dumpexpr, 
                    arglist)
                else arglist <- c(sta, hy2, dumpstr = dumpexpr, 
                  bottlestr = botexpr, arglist)
            }
            else {
                arglist <- c(sta, hy2, arglist)
            }
            if (surrogate == "turbidity") 
                funcname <- "turbsscplot"
            else funcname <- "qsscplot"
            plotfunc <- get(funcname, envir = .GlobalEnv)
            res <- do.call("plotfunc", arglist)
            if (result != "") {
                assign(result, res, envir = .GlobalEnv)
                cat("Result saved in workspace as", result, "\n")
            }
            saveCommand(stn, hy2, funcname, arglist, result, 
                checkvars["savecmd"])
        })
        panel
    }
    reset <- function(panel, oldgrid) {
        par(mfrow = panel[["oldgrid"]])
        panel
    }
    nothing <- function(panel) invisible(panel)
    env <- getTTSenv()
    init.stn <- env["STN", ]
    init.hy <- env["HY", ]
    init.sdate <- env["SDATE", ]
    init.edate <- env["EDATE", ]
    dumpexpr <- env["DUMPS", ]
    botexpr <- env["BOTTLES", ]
    oldgrid <- par("mfrow")
    init.interstorm <- env["INTERSTORM", ]
    if (is.na(init.interstorm)) 
        init.interstorm <- F
    pars <- c(init.stn, init.hy, init.sdate, init.edate, dumpexpr, 
        botexpr, "")
    loesspars <- c(1, 1)
    panel <- rp.control("Scatterplot", oldgrid = oldgrid)
    rp.radiogroup(panel, surrogate, c("turbidity", "flow"), title = "Sediment surrogate", 
        action = nothing)
    rp.listbox(panel, layout, vals = c("1x1", "1x2", "2x2", "2x3"), 
        initval = "1x1", rows = 4, title = "Plot layout", action = nothing)
    my.textentry(panel, pars, labels = c("Station", "Water year", 
        "First sample date/time (m/d/y h:m)", "Last sample date/time (m/d/y h:m)", 
        "Dumps (R expression)", "Corresponding bottles (R expression)", 
        "Output object name (optional)"), names = c("stn", "hy4", 
        "startdt", "lastdt", "dumpexpr", "botexpr", "result"), 
        title = "Enter values", initval = pars)
    rp.radiogroup(panel, subsetby, c("date/time", "dump/bottle", 
        "use entire year"), title = "Criterion for selecting samples", 
        action = nothing, initval = "date/time")
    rp.radiogroup(panel, plotsym, c("bottle", "dump"), title = "Plotting symbol", 
        initval = "bottle", action = nothing)
    rp.radiogroup(panel, type, c("linear", "logx", "logxy", "power", 
        "loess", "pairs"), title = "Model to fit", initval = "linear", 
        action = nothing)
    my.textentry(panel, loesspars, labels = c("span", "degree"), 
        names = c("span", "degree"), title = "Loess parameters", 
        initval = loesspars)
    rp.checkbox(panel, checkvars, initval = c(T, init.interstorm, 
        T), title = "Other options", labels = c("Exclude previously flagged points (those with exclude=T)", 
        "Interstorm data only", "Save command to file"), names = c("exclude", 
        "interstorm", "savecmd"), action = nothing)
    rp.button(panel, action = scatPlot, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = reset, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run turbsscplot() and qsscplo()", 
"  scatPlot <- function(panel) {", "    with(panel, {", "      stn <- tolower(pars[\"stn\"])", 
"      hy4 <- pars[\"hy4\"]", "      startdt <- pars[\"startdt\"]", 
"      lastdt <- pars[\"lastdt\"]", "      dumpexpr <- as.vector(pars[\"dumpexpr\"])  # necessary when arglist is assembled later", 
"      botexpr <- as.vector(pars[\"botexpr\"])    # necessary when arglist is assembled later", 
"      result <- pars[\"result\"]", "      startgrid <- par(\"mfrow\")", 
"      mygrid <- as.numeric(unlist(strsplit(layout,\"x\")))", 
"      if (any(mygrid != startgrid)) {", "        par(mfrow=mygrid)", 
"      }", "      interstorm <- checkvars[\"interstorm\"]", "      ", 
"      if (nchar(hy4) < 3) {", "        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- as.vector(substring(hy4,3,4))  # as.vector required to get rid of the name \"hy4\"", 
"      # Save selected station and year in TTS environment", 
"      setTTSenv(c(\"STN\",\"HY\",\"SDATE\",\"EDATE\",\"DUMPS\",\"BOTTLES\",\"INTERSTORM\"),", 
"                c(stn,hy4,startdt,lastdt,dumpexpr,botexpr,interstorm))", 
"      ", "      arglist <- list(txt=plotsym,type=type, exclude=checkvars[\"exclude\"])", 
"      sta <- check.interstorm(stn, hy2, interstorm, surrogate,checkflo=F)", 
"      if (type==\"loess\") {", "        span <- as.numeric(loesspars[\"span\"])", 
"        degree <- as.numeric(loesspars[\"degree\"])", "        arglist <- c(arglist,span=span,degree=degree)", 
"      } ", "      if (subsetby == \"date/time\") {", "        sdate <- paste(substring(startdt,7,8),substring(startdt,1,2),substring(startdt,4,5),sep=\"\")", 
"        stime <- paste(substring(startdt,10,11),substring(startdt,13,14),sep=\"\")", 
"        edate <- paste(substring(lastdt,7,8),substring(lastdt,1,2),substring(lastdt,4,5),sep=\"\")", 
"        etime <- paste(substring(lastdt,10,11),substring(lastdt,13,14),sep=\"\")", 
"        arglist <- c(sta,hy2,sdate,stime,edate,etime,arglist)", 
"      }", "      else if(subsetby==\"dump/bottle\") {", "        dumps = eval(parse(text=dumpexpr))", 
"        bots = eval(parse(text=botexpr))", "        if (botexpr == \"\")", 
"          arglist <- c(sta,hy2,dumpstr=dumpexpr,arglist)", "        else ", 
"          arglist <- c(sta,hy2,dumpstr=dumpexpr,bottlestr=botexpr,arglist)", 
"      }", "      else {", "        arglist <- c(sta,hy2,arglist)", 
"      }", "      ", "      if (surrogate == \"turbidity\")", 
"        funcname <- \"turbsscplot\"", "      else ", "        funcname <- \"qsscplot\"", 
"      plotfunc <- get(funcname,envir=.GlobalEnv)", "      res <- do.call(\"plotfunc\",arglist)", 
"      if (result!=\"\") {", "        assign(result,res,envir=.GlobalEnv)", 
"        cat(\"Result saved in workspace as\",result,\"\\n\")", 
"      }", "      saveCommand(stn,hy2,funcname,arglist,result,checkvars[\"savecmd\"])", 
"    })", "    panel", "  }", "  ", "  reset <- function(panel,oldgrid) {", 
"    par(mfrow=panel[[\"oldgrid\"]])", "    panel", "  }", "  ", 
"  nothing <- function(panel) invisible(panel)", "  env <- getTTSenv()", 
"  init.stn <- env[\"STN\",]", "  init.hy <- env[\"HY\",]", "  init.sdate <- env[\"SDATE\",]", 
"  init.edate <- env[\"EDATE\",]", "  dumpexpr <- env[\"DUMPS\",]", 
"  botexpr <- env[\"BOTTLES\",]", "  oldgrid <- par(\"mfrow\")", 
"  init.interstorm <- env[\"INTERSTORM\",]", "  if (is.na(init.interstorm))", 
"    init.interstorm <- F", "  ", "  pars <- c(init.stn,init.hy,init.sdate,init.edate,dumpexpr,botexpr,\"\")", 
"  loesspars <- c(1,1)", "  panel <- rp.control(\"Scatterplot\",oldgrid=oldgrid)", 
"", "  rp.radiogroup(panel, surrogate, c(\"turbidity\",\"flow\"),title=\"Sediment surrogate\",action=nothing)", 
"  rp.listbox(panel, layout, vals=c(\"1x1\",\"1x2\",\"2x2\",\"2x3\"),initval=\"1x1\", rows=4,title=\"Plot layout\",action=nothing)", 
"  my.textentry(panel,pars,labels=c(\"Station\",\"Water year\",\"First sample date/time (m/d/y h:m)\",\"Last sample date/time (m/d/y h:m)\",", 
"                                   \"Dumps (R expression)\",\"Corresponding bottles (R expression)\",\"Output object name (optional)\"),", 
"               names=c(\"stn\",\"hy4\",\"startdt\",\"lastdt\",\"dumpexpr\",\"botexpr\",\"result\"),title=\"Enter values\",initval=pars)", 
"  rp.radiogroup(panel, subsetby, c(\"date/time\",\"dump/bottle\",\"use entire year\"),title=\"Criterion for selecting samples\",action=nothing,initval=\"date/time\")", 
"  rp.radiogroup(panel, plotsym, c(\"bottle\",\"dump\"),title=\"Plotting symbol\",initval=\"bottle\",action=nothing)  ", 
"  rp.radiogroup(panel, type, c(\"linear\", \"logx\", \"logxy\", \"power\", \"loess\", \"pairs\"), title=\"Model to fit\", initval=\"linear\",action=nothing)", 
"  my.textentry(panel,loesspars,labels=c(\"span\",\"degree\"),names=c(\"span\",\"degree\"),title=\"Loess parameters\",initval=loesspars)", 
"  rp.checkbox(panel, checkvars, initval = c(T,init.interstorm,T), title=\"Other options\",", 
"              labels=c(\"Exclude previously flagged points (those with exclude=T)\",\"Interstorm data only\",\"Save command to file\"),", 
"              names=c(\"exclude\",\"interstorm\",\"savecmd\"),action=nothing)", 
"  rp.button(panel, action = scatPlot, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = reset, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
scientific <-
structure(function (x, mandigits, expdigits) 
{
    x <- formatC(x, format = "E", digits = mandigits)
    splitnum <- strsplit(x, "E")
    mantissa <- unlist(lapply(splitnum, function(x) x[1]))
    exp <- unlist(lapply(splitnum, function(x) x[2]))
    abs.exp <- substring(exp, 2, nchar(exp))
    sign.exp <- substring(exp, 1, 1)
    new.exp <- padstring(abs.exp, expdigits, "0", right = F, 
        chop = T)
    new.exp <- paste(sign.exp, new.exp, sep = "")
    paste(mantissa, new.exp, sep = "E")
}, source = c("function (x, mandigits, expdigits) ", "{", "# Formats a number in scientific notation", 
"# mandigits = number of mantissa digits to right of decimal", 
"# expdigits = number of numeric digits in exponent", "# Note: width of output is one greater for negative numbers", 
"# This function compensates for the fact that the built-in", 
"# function, formatC, ignores width when using format=\"E\" .", 
"# The actual width produced by formatC is platform-dependent", 
"    x <- formatC(x, format=\"E\", digits=mandigits)", "    splitnum <- strsplit(x,\"E\")", 
"    mantissa <- unlist(lapply(splitnum, function(x) x[1]))", 
"    exp <- unlist(lapply(splitnum, function(x) x[2]))", "    abs.exp <- substring(exp,2,nchar(exp))", 
"    sign.exp <- substring(exp,1,1)", "    new.exp <- padstring(abs.exp,expdigits,\"0\",right=F,chop=T)", 
"    new.exp <- paste(sign.exp,new.exp,sep=\"\")", "    paste(mantissa,new.exp,sep=\"E\")", 
"}"))
searchfuncs <-
structure(function (string, names) 
{
    if (missing(names)) 
        names <- listfuncs()
    funcstrings <- sapply(names, function(x) paste(deparse(eval(as.name(x))), 
        collapse = " "))
    names[grep(string, funcstrings)]
}, source = c("function(string, names) {", "# Search for string inside a set of functions specified by names", 
"# Return the names of the functions that contain the string", 
"    if (missing(names)) names <- listfuncs()", "    funcstrings <- sapply(names, function(x) paste(deparse(eval(as.name(x))),collapse=\" \"))", 
"    names[grep(string,funcstrings)]", "}"))
select.file <-
structure(function (pathname = getTTSenv("LOGGERHOME")) 
{
    cat("\nWelcome to TTS RAWPLOT...\n")
    if (file.exists(pathname)) {
        files <- list.files(path = pathname, pattern = "[dD][aA][tT]$", 
            all.files = FALSE, full.names = FALSE)
        if (length(files) == 0) 
            stop(paste("No data files found in", pathname))
    }
    else if (pathname == "") {
        stop("Please set the data logger Home Dir from the TTS menu\n")
    }
    else {
        stop(paste("Data repository", pathname, "not found\n", 
            "Please reset the data logger Home Dir from the TTS menu\n"))
    }
    cat(paste("\nThe following files, found within the ", pathname, 
        " directory,\nare available for plotting:\n\n", sep = ""))
    for (i in seq(along = files)) {
        cat(paste(files[i], "->", i, "\n", sep = " "))
    }
    answer <- readline("\nFile selection? (<CR> = Quit): ")
    if (answer == "") 
        return("")
    file <- paste(pathname, files[as.integer(answer)], sep = "/")
    cat("\n")
    file
}, source = c("function (pathname = getTTSenv(\"LOGGERHOME\")) ", 
"{", "    cat(\"\\nWelcome to TTS RAWPLOT...\\n\")", "    if (file.exists(pathname)) {", 
"        files <- list.files(path = pathname, pattern = \"[dD][aA][tT]$\", ", 
"            all.files = FALSE, full.names = FALSE)", "        if (length(files) == 0) stop(paste(\"No data files found in\",pathname))", 
"    }", "    else if (pathname == \"\") {", "        stop(\"Please set the data logger Home Dir from the TTS menu\\n\")", 
"    }", "    else {", "        stop(paste(\"Data repository\",pathname,\"not found\\n\",", 
"        \"Please reset the data logger Home Dir from the TTS menu\\n\"))", 
"    }", "    cat(paste(\"\\nThe following files, found within the \", pathname, ", 
"        \" directory,\\nare available for plotting:\\n\\n\", sep = \"\"))", 
"    for (i in seq(along=files)) {", "        cat(paste(files[i], \"->\", i, \"\\n\", sep = \" \"))", 
"    }", "    answer <- readline(\"\\nFile selection? (<CR> = Quit): \")", 
"    if (answer == \"\") return(\"\")", "    file <- paste(pathname,files[as.integer(answer)],sep=\"/\")", 
"    cat(\"\\n\")", "    file", "}"))
select.flofile <-
structure(function (stn, allfiles) 
{
    regex <- paste(stn, "[0-9][0-9][.]flo", sep = "")
    flofiles <- grep(regex, allfiles, value = T, ignore = T)
    hy <- as.numeric(substring(flofiles, 4, 5))
    flofiles <- flofiles[order(yy2year(hy))]
    cat("\nChoose a FLO file:\n\n")
    n <- length(flofiles)
    if (n == 0) {
        print("No flo files found in station directory")
        return()
    }
    for (i in 1:n) {
        cat(paste(flofiles[i], "->", i, "\n", sep = " "))
    }
    while (1) {
        answer <- readline("\nEnter file number: <CR> = Quit) ")
        if (answer == "") 
            return()
        answer <- as.integer(answer)
        if (!is.na(answer) && answer %in% 1:n) 
            break
    }
    cat("\n")
    flofiles[answer]
}, source = c("function (stn,allfiles) ", "{", "#   Let user select from available flo files for given station", 
"    regex <- paste(stn,\"[0-9][0-9][.]flo\",sep=\"\")", "    flofiles <- grep(regex,allfiles,value=T,ignore=T)", 
"    hy <- as.numeric(substring(flofiles,4,5))", "    flofiles <- flofiles[order(yy2year(hy))]", 
"    cat(\"\\nChoose a FLO file:\\n\\n\")", "    n <- length(flofiles)", 
"    if (n == 0) {", "        print(\"No flo files found in station directory\")", 
"        return()", "    }", "    for (i in 1:n) {", "        cat(paste(flofiles[i], \"->\", i, \"\\n\", sep = \" \"))", 
"    }", "    while (1) {", "        answer <- readline(\"\\nEnter file number: <CR> = Quit) \")", 
"        if (answer == \"\") return()", "        answer <- as.integer(answer)", 
"        if (!is.na(answer) && answer %in% 1:n) break", "    }", 
"    cat(\"\\n\")", "    flofiles[answer]", "}"))
select.station <-
structure(function (path) 
{
    if (!file.exists(path)) 
        stop(paste(path, "not found.\n", "To change the location of station directories do: fix(floplot.gui)\n", 
            "The default TTS home location is specified at line 1 of floplot.gui."))
    folders <- list.files(path)
    stationdirs <- folders[nchar(folders) == 3]
    n <- length(stationdirs)
    if (n == 0) 
        stop("No station directories found\n")
    cat("Station directories are:")
    for (i in 1:n) {
        if ((i%%16) == 1) 
            cat("\n")
        cat(stationdirs[i], " ")
    }
    while (1) {
        cat("\n")
        stn <- readline("Enter station name (<CR> = Quit): ")
        stn <- tolower(stn)
        if (stn == "") 
            return(NULL)
        if (stn %in% tolower(stationdirs)) 
            break
        else cat("Station directory not found under ", path, 
            "\n")
    }
    extravars <- character(0)
    minstg <- NA
    if (!file.exists("labels.txt")) {
        cat("No labels.txt found in ", getwd(), "\n")
        cat("You will not be able to plot temperature or rainfall\n")
    }
    else {
        labels <- scan("labels.txt", what = "")
        data <- strsplit(labels, ",")
        stations <- tolower(sapply(data, function(x) x[2]))
        if (stn %in% stations) {
            names(data) <- stations
            selected.data <- data[[stn]]
            array.id <- as.numeric(selected.data[1])
            if (array.id == 0) 
                extravars <- selected.data[-(1:12)]
            else extravars <- selected.data[-(1:14)]
            minstg <- as.numeric(selected.data[4])
        }
    }
    cat("\n")
    list(stn = stn, extravars = extravars, minstg = minstg)
}, source = c("function (path) ", "{", "#   Ask user which station to plot", 
"#   Return station and, from labels.txt, extra variable names and minstg", 
"#   First get a list of station directories", "    if (!file.exists(path)) stop(paste(path,\"not found.\\n\",", 
"       \"To change the location of station directories do: fix(floplot.gui)\\n\",", 
"       \"The default TTS home location is specified at line 1 of floplot.gui.\"))", 
"    folders <- list.files(path)", "    stationdirs <- folders[nchar(folders)==3]", 
"    n <- length(stationdirs)", "    if (n==0) stop(\"No station directories found\\n\")", 
"    cat(\"Station directories are:\")", "    for (i in 1:n) {", 
"        if ((i %% 16) == 1) cat(\"\\n\")", "        cat(stationdirs[i],\" \")", 
"    }", "#   Prompt for station name until valid station given", 
"    while (1) {", "       cat(\"\\n\")", "       stn <- readline(\"Enter station name (<CR> = Quit): \")", 
"       stn <- tolower(stn)", "       if (stn == \"\") return(NULL)", 
"       if (stn %in% tolower(stationdirs))", "          break", 
"       else", "          cat(\"Station directory not found under \", path,\"\\n\")", 
"    }", "    extravars <- character(0)", "    minstg <- NA", 
"    if (!file.exists(\"labels.txt\")) {", "        cat(\"No labels.txt found in \", getwd(),\"\\n\")", 
"        cat(\"You will not be able to plot temperature or rainfall\\n\")", 
"    }", "    else {", "#      Search labels.txt for selected station", 
"       labels <- scan(\"labels.txt\",what=\"\")", "       data <- strsplit(labels,\",\")", 
"       stations <- tolower(sapply(data,function(x) x[2]))", 
"       if (stn %in% stations) {", "           # Get extra variable names and minimum stage         ", 
"           # Assumes the following standard TTS variable names will be listed", 
"           # in labels.txt after array, stn, units, and minstg:", 
"           # Array ID = 000: timestamp,recnum,dump,bottle,code1,code2,stg,turb", 
"           # Array ID > 000: array,year,date,time,dump,bottle,code1,code2,stg,turb ", 
"           names(data) <- stations", "           selected.data <- data[[stn]]", 
"           array.id <- as.numeric(selected.data[1])", "           if (array.id == 0)", 
"               extravars <- selected.data[-(1:12)]", "           else", 
"               extravars <- selected.data[-(1:14)]", "           minstg <- as.numeric(selected.data[4])", 
"       }", "    }", "    cat(\"\\n\")", "    list(stn=stn,extravars=extravars,minstg=minstg)", 
"}"))
selStnYear <-
structure(function () 
{
    initializeDialog(title = gettextRcmdr("Select station and year"))
    onOK <- function() {
        stn <<- tclvalue(stnValue)
        hy <<- tclvalue(hyValue)
        closeDialog()
        doItAndPrint(paste("hy: set to", hy, "\nstn: set to", 
            stn))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    stnFrame <- tkframe(top)
    stnValue <- tclVar(ifelse(exists("stn", env = .GlobalEnv), 
        stn, ""))
    stnField <- tkentry(stnFrame, width = "6", textvariable = stnValue)
    hyFrame <- tkframe(top)
    hyValue <- tclVar(ifelse(exists("hy", env = .GlobalEnv), 
        hy, ""))
    hyField <- tkentry(hyFrame, width = "6", textvariable = hyValue)
    tkgrid(tklabel(stnFrame, text = gettextRcmdr("Station Name"), 
        fg = "blue"))
    tkgrid(stnField, sticky = "w")
    tkgrid(tklabel(hyFrame, text = gettextRcmdr("Hydro Year"), 
        fg = "blue"))
    tkgrid(hyField, sticky = "w")
    tkgrid(stnFrame, hyFrame, sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix(rows = 2, columns = 2)
    invisible(list(stn = stn, hy = hy))
}, source = c("function() {", "   initializeDialog(title=gettextRcmdr(\"Select station and year\"))", 
"   onOK <- function() {", "       stn <<- tclvalue(stnValue)", 
"       hy <<- tclvalue(hyValue)", "       closeDialog()", "       doItAndPrint(paste(\"hy: set to\",hy,\"\\nstn: set to\",stn))", 
"       tkfocus(CommanderWindow())", "   }", "   OKCancelHelp()", 
"   stnFrame <- tkframe(top)", "   stnValue <- tclVar(ifelse(exists(\"stn\", env=.GlobalEnv),stn,\"\"))", 
"   stnField <- tkentry(stnFrame, width=\"6\", textvariable=stnValue)", 
"   hyFrame <- tkframe(top)", "   hyValue <- tclVar(ifelse(exists(\"hy\", env=.GlobalEnv),hy,\"\"))", 
"   hyField <- tkentry(hyFrame, width=\"6\", textvariable=hyValue)", 
"   tkgrid(tklabel(stnFrame,text=gettextRcmdr(\"Station Name\"), fg=\"blue\"))", 
"   tkgrid(stnField, sticky=\"w\")", "   tkgrid(tklabel(hyFrame,text=gettextRcmdr(\"Hydro Year\"), fg=\"blue\"))", 
"   tkgrid(hyField, sticky=\"w\")", "   tkgrid(stnFrame,hyFrame, sticky=\"nw\")", 
"   tkgrid(buttonsFrame, columnspan=2, sticky=\"w\")", "   dialogSuffix(rows=2, columns=2)", 
"   invisible(list(stn=stn, hy=hy))", "}"))
setLoggerHome <-
structure(function (initial = getTTSenv("LOGGERHOME")) 
{
    library(tcltk)
    title <- "Select the location to which data logger files are uploaded in the field"
    tcldir <- tkchooseDirectory(title = title, initialdir = initial, 
        mustexist = T)
    newhome <- tclvalue(tcldir)
    if (newhome == "" || (!is.na(initial) && newhome == initial)) {
        cat("Data Logger Home not changed\n")
        return(invisible())
    }
    setTTSenv("LOGGERHOME", newhome)
    cat("Data Logger Home set to", newhome, "\n")
}, source = c("function(initial=getTTSenv(\"LOGGERHOME\")) {", 
"# Allows the user to set the Data Logger Home directory", "# Value is saved in \"TTSenvironment.txt\" file in working directory", 
"    library(tcltk)", "    title <- \"Select the location to which data logger files are uploaded in the field\"", 
"    tcldir <- tkchooseDirectory(title=title, initialdir=initial, mustexist=T)", 
"    newhome <- tclvalue(tcldir)", "    if (newhome == \"\" || (!is.na(initial) && newhome == initial)) {", 
"        cat(\"Data Logger Home not changed\\n\")", "        return(invisible())", 
"    }", "    setTTSenv(\"LOGGERHOME\",newhome)", "    cat(\"Data Logger Home set to\",newhome,\"\\n\")", 
"}"))
setTTSenv <-
structure(function (name, value, env) 
{
    if (missing(env)) 
        env <- getTTSenv()
    env[name, "value"] <- value
    write.table(env, "TTSenvironment.txt", sep = "=", col.names = F, 
        quote = F)
}, source = c("function (name,value,env) ", "{", "   if(missing(env))", 
"      env <- getTTSenv()", "   env[name,\"value\"] <- value", 
"   write.table(env,\"TTSenvironment.txt\",sep=\"=\",col.names=F,quote=F)", 
"}"))
setTTSHome <-
structure(function (initial = getTTSenv("TTSHOME")) 
{
    library(tcltk)
    title <- "Select the TTS Home Directory (i.e. where the station folders reside)"
    tcldir <- tkchooseDirectory(title = title, initialdir = initial, 
        mustexist = T)
    newhome <- tclvalue(tcldir)
    if (newhome == "" || (!is.na(initial) && newhome == initial)) {
        cat("TTS Home not changed\n")
        return(invisible())
    }
    setTTSenv("TTSHOME", newhome)
    cat("TTS Home set to", newhome, "\n")
}, source = c("function(initial=getTTSenv(\"TTSHOME\")) {", "# Allows the user to set the TTS Home directory for appended data plots", 
"    library(tcltk)", "    title <- \"Select the TTS Home Directory (i.e. where the station folders reside)\"", 
"    tcldir <- tkchooseDirectory(title=title, initialdir=initial, mustexist=T)", 
"    newhome <- tclvalue(tcldir)", "    if (newhome == \"\" || (!is.na(initial) && newhome == initial)) {", 
"        cat(\"TTS Home not changed\\n\")", "        return(invisible())", 
"    }", "    setTTSenv(\"TTSHOME\",newhome)", "    cat(\"TTS Home set to\",newhome,\"\\n\")", 
"}"))
sev <-
structure(function (ssc, dur, a, b, c) 
{
    result <- a + b * log(dur) + c * log(ssc)
    result[dur == 0] <- 0
    result
}, source = c("function(ssc, dur, a, b, c) {", "# Calculate Newcombe and Jensen's Severity of ill effects (SEV) index", 
"    result <- a + b*log(dur) + c*log(ssc)", "    result[dur == 0] <- 0", 
"    result", "}"))
sev.mat <-
structure(function (matrix, a, b, c) 
{
    sevmat <- matrix
    ssc <- as.numeric(dimnames(matrix)[[2]])
    for (i in 1:length(ssc)) {
        sevmat[, i] <- sev(ssc[i], matrix[, i], a, b, c)
    }
    sevmat
}, source = c("function(matrix, a, b, c) {", "# Apply severity calculation to a matrix of durations", 
"# in which the column names are the SSC levels", "     sevmat <- matrix", 
"     ssc <- as.numeric(dimnames(matrix)[[2]])", "     for (i in 1:length(ssc)) {", 
"         sevmat[,i] <- sev(ssc[i],matrix[,i],a,b,c)", "     }", 
"     sevmat", "}"))
ssc.mycdf <-
structure(function (x, title, months, log = "x", zero = 0.1, 
    leap = F, interval = 10, grid = "days", yvar = "SSC", ...) 
{
    if (any(is.na(x))) 
        stop("Missing values not allowed")
    if (log == "x") {
        if (any(x <= 0)) {
            if (any(x < 0)) 
                cat(sum(x < 0), "negative values replaced by", 
                  zero, "\n")
            if (any(x == 0)) 
                cat(sum(x == 0), "zero values replaced by", zero, 
                  "\n")
            x[x <= 0] <- zero
        }
    }
    par(mar = c(5, 4, 4, 4) + 0.1, mgp = c(2.5, 0.75, 0))
    y1lab <- paste("Proportion exceeding given", yvar)
    y2lab <- paste("Days exceeding given", yvar)
    xsort <- sort(x)
    nx <- length(x)
    daysperyear <- sumdays(months, leap)
    px <- nx/daysperyear/(1440/interval)
    if (px > 1) 
        stop("Do not run with more than one year of data")
    y <- (1 - (1:nx)/nx) * px
    if (yvar == "SSC") 
        xlab <- "SSC (mg/L)"
    else if (yvar == "turbidity") 
        xlab <- "turbidity (NTU)"
    else xlab <- yvar
    plot(xsort, y, xlab = xlab, ylab = y1lab, log = log, ...)
    title(title)
    y2range <- daysperyear * par()$usr[3:4]
    y2ticlab <- pretty(y2range)
    y2tic <- y2ticlab/daysperyear
    axis(4, at = y2tic, label = y2ticlab, las = 2)
    mtext(side = 4, outer = T, at = 0.5, text = y2lab, line = -1.75)
    if (grid == "days") {
        axis(1, tck = 1, fg = "gray")
        axis(4, tck = 1, fg = "gray", at = y2tic, label = F)
    }
    else if (grid == "pro") {
        axis(1, tck = 1, fg = "gray")
        axis(2, tck = 1, fg = "gray")
    }
    box()
}, source = c("function(x,title,months,log=\"x\",zero=0.1,leap=F,interval=10,grid=\"days\",yvar=\"SSC\",...) {", 
"# Plot the annual CDF of SSC at a station (may be used for other variables besides SSC)", 
"# Results are normalized for a year consisting only of specified months", 
"# x is the variable to be analyzed", "# title is the title of the plot", 
"# months is a vector specifying which months to analyze", "# if log=\"x\" plot CDF of log(x), replacing any zero or negative values with", 
"#    the value given by 'zero' argument", "# leap is logical variable indicating leap year (T) or not (F)", 
"# interval is the length of the time interval between values of the x vector", 
"# grid places grid according to left axis (grid=\"pro\") or right axis (grid=\"days\")", 
"# yvar is the name of the variable analyzed, to be used in labeling  axes", 
"   if (any(is.na(x))) stop(\"Missing values not allowed\")", 
"   if (log==\"x\") {", "      if (any(x <= 0)) {", "         if (any(x < 0))", 
"             cat(sum(x < 0),\"negative values replaced by\",zero,\"\\n\")", 
"         if (any(x == 0))", "             cat(sum(x == 0),\"zero values replaced by\",zero,\"\\n\")", 
"         x[x <= 0] <- zero", "      }", "   }", "   par(mar=c(5,4,4,4) + 0.1, mgp=c(2.5,0.75,0))", 
"   y1lab <- paste(\"Proportion exceeding given\",yvar)", "   y2lab <- paste(\"Days exceeding given\",yvar)", 
"   xsort <- sort(x)", "   nx <- length(x)", "   daysperyear <- sumdays(months,leap)", 
"   px <- nx/daysperyear/(1440/interval)", "   if (px > 1)", 
"      stop(\"Do not run with more than one year of data\")", 
"   y <- (1-(1:nx)/nx) * px", "   if (yvar == \"SSC\")", "      xlab <- \"SSC (mg/L)\"", 
"   else if (yvar == \"turbidity\")", "      xlab <- \"turbidity (NTU)\"", 
"   else", "      xlab <- yvar", "   plot(xsort, y, xlab=xlab, ylab=y1lab, log=log, ...)", 
"   title(title)", "   y2range <- daysperyear * par()$usr[3:4]", 
"   y2ticlab <- pretty(y2range)", "   y2tic <- y2ticlab/daysperyear", 
"   axis(4,at=y2tic,label=y2ticlab,las=2)", "   mtext(side=4,outer=T,at=0.5,text=y2lab,line=-1.75)", 
"   if (grid == \"days\") {", "      axis(1,tck=1,fg=\"gray\")", 
"      axis(4,tck=1,fg=\"gray\",at=y2tic,label=F)", "   }", "   else if (grid == \"pro\") {", 
"      axis(1,tck=1,fg=\"gray\")", "      axis(2,tck=1,fg=\"gray\")", 
"   }", "", "", "   box()", "}"))
ssc.mycdfs <-
structure(function (xlist, title, months, log = "x", zero = 0.1, 
    leap = F, interval = 10, grid = "days", yvar = "SSC", ...) 
{
    if (!is.list(xlist)) 
        ssc.mycdf(xlist, title, log, zero, leap, interval, ...)
    y1lab <- paste("Proportion exceeding given", yvar)
    y2lab <- paste("Days exceeding given", yvar)
    par(mar = c(5, 4, 4, 4) + 0.1, mgp = c(2.5, 0.75, 0))
    nxx <- sapply(xlist, length)
    daysperyear <- sumdays(months, leap)
    pxx <- nxx/daysperyear/(1440/interval)
    if (any(pxx > 1)) 
        stop("Do not run with more than one year of data")
    xmin <- min(sapply(xlist, min))
    xmax <- max(sapply(xlist, max))
    if (log == "x") 
        xmin <- max(zero, xmin)
    if (yvar == "SSC") 
        xlab <- "SSC (mg/L)"
    else if (yvar == "turbidity") 
        xlab <- "turbidity (NTU)"
    else xlab <- yvar
    plot(c(xmin, xmax), c(0, max(pxx)), type = "n", xlab = xlab, 
        ylab = y1lab, log = log, ...)
    title(title)
    y2range <- daysperyear * par()$usr[3:4]
    y2ticlab <- pretty(y2range)
    y2tic <- y2ticlab/daysperyear
    axis(4, at = y2tic, label = y2ticlab, las = 2)
    mtext(side = 4, outer = T, at = 0.5, text = y2lab, line = -1.75)
    if (grid == "days") {
        axis(1, tck = 1, fg = "gray")
        axis(4, tck = 1, fg = "gray", at = y2tic, label = F)
    }
    else if (grid == "pro") {
        axis(1, tck = 1, fg = "gray")
        axis(2, tck = 1, fg = "gray")
    }
    box()
    xnames <- names(xlist)
    for (i in 1:length(xlist)) {
        x <- xlist[[i]]
        nx <- nxx[i]
        px <- pxx[i]
        xname <- xnames[i]
        if (log == "x") {
            if (any(x <= 0)) {
                if (any(x < 0)) 
                  cat(sum(x < 0), "negative values of", xname, 
                    "replaced by", zero, "\n")
                if (any(x == 0)) 
                  cat(sum(x == 0), "zero values of", xname, "replaced by", 
                    zero, "\n")
                x[x <= 0] <- zero
            }
        }
        xsort <- sort(x)
        y <- (1 - (1:nx)/nx) * px
        lines(xsort, y, col = i)
    }
    legend(locator(1), xnames, lty = 1, col = 1:length(xnames))
}, source = c("function(xlist,title,months,log=\"x\",zero=0.1,leap=F,interval=10,grid=\"days\",yvar=\"SSC\",...) {", 
"# Plot the annual CDF of SSC (or other variable) at one or more stations for a given year", 
"# x will be a list if several stations are to be plotted", "# names of components will appear in legend", 
"# If less than a year of data, assume anthing not measured has lower SSC", 
"# if log=\"x\" plot CDF of log(x), replacing any zero or negative values with", 
"#    the value given by 'zero' argument", "# Arguments, excpept for the first, are identical to those of ssc.mycdf", 
"   if (!is.list(xlist))", "      ssc.mycdf(xlist,title,log,zero,leap,interval,...)", 
"   y1lab <- paste(\"Proportion exceeding given\",yvar)", "   y2lab <- paste(\"Days exceeding given\",yvar)", 
"   par(mar=c(5,4,4,4) + 0.1, mgp=c(2.5,0.75,0))", "   nxx <- sapply(xlist,length)", 
"   daysperyear <- sumdays(months,leap)", "   pxx <- nxx/daysperyear/(1440/interval)", 
"   if (any(pxx > 1))", "      stop(\"Do not run with more than one year of data\")", 
"   xmin <- min(sapply(xlist,min))", "   xmax <- max(sapply(xlist,max))", 
"   if (log==\"x\")", "      xmin <- max(zero,xmin)", "   if (yvar == \"SSC\")", 
"      xlab <- \"SSC (mg/L)\"", "   else if (yvar == \"turbidity\")", 
"      xlab <- \"turbidity (NTU)\"", "   else", "      xlab <- yvar", 
"   plot(c(xmin,xmax), c(0,max(pxx)), type=\"n\", xlab=xlab, ylab=y1lab, log=log, ...)", 
"   title(title)   ", "   y2range <- daysperyear * par()$usr[3:4]", 
"   y2ticlab <- pretty(y2range)", "   y2tic <- y2ticlab/daysperyear", 
"   axis(4,at=y2tic,label=y2ticlab,las=2)", "   mtext(side=4,outer=T,at=0.5,text=y2lab,line=-1.75)", 
"   if (grid == \"days\") {", "      axis(1,tck=1,fg=\"gray\")", 
"      axis(4,tck=1,fg=\"gray\",at=y2tic,label=F)", "   }", "   else if (grid == \"pro\") {", 
"      axis(1,tck=1,fg=\"gray\")", "      axis(2,tck=1,fg=\"gray\")", 
"   }", "   box()", "   xnames <- names(xlist)", "   for (i in 1:length(xlist)) {", 
"      x <- xlist[[i]]", "      nx <- nxx[i]", "      px <- pxx[i]", 
"      xname <- xnames[i]", "      if (log==\"x\") {", "         if (any(x <= 0)) {", 
"            if (any(x < 0))", "                cat(sum(x < 0),\"negative values of\",xname,\"replaced by\",zero,\"\\n\")", 
"            if (any(x == 0))", "                cat(sum(x == 0),\"zero values of\",xname,\"replaced by\",zero,\"\\n\")", 
"            x[x <= 0] <- zero", "         }", "      }", "      xsort <- sort(x)", 
"      y <- (1-(1:nx)/nx) * px", "      lines(xsort,y,col=i)", 
"   }", "   legend(locator(1),xnames,lty=1,col=1:length(xnames))", 
"}"))
staffnames <-
structure(function (stn) 
{
    ttshome <- getTTSenv("TTSHOME")
    filepath <- paste(ttshome, "stationinfo.txt", sep = "/")
    if (!file.exists(filepath)) {
        cat("stationinfo.txt not found in", ttshome, "\n")
        cat("Assuming just one staff plate exists\n")
        return("staff")
    }
    info <- scan(filepath, what = list(stn = "", area = "", units = "", 
        or1 = "", or2 = "", or3 = "", or4 = "", or5 = ""), sep = ",", 
        fill = T, na.strings = "")
    if (!(stn %in% info$stn)) {
        cat(stn, "not found in stationinfo.txt\n")
        cat("Assuming just one staff plate exists\n")
        return("staff")
    }
    info.df <- as.data.frame(info)
    info <- info.df[info.df$stn == stn, ]
    info <- info[!is.na(info)]
    info[-(1:3)]
}, source = c("function(stn) {", "# Returns the list of staff plate names from stationinfo.txt", 
"ttshome <- getTTSenv(\"TTSHOME\")", "filepath <- paste(ttshome,\"stationinfo.txt\",sep=\"/\")", 
"if (!file.exists(filepath)) {", "   cat(\"stationinfo.txt not found in\",ttshome,\"\\n\")", 
"   cat(\"Assuming just one staff plate exists\\n\")", "   return(\"staff\")", 
"}", "info <- scan(filepath,what=list(stn=\"\",area=\"\",units=\"\",or1=\"\",or2=\"\",or3=\"\",or4=\"\",or5=\"\"),sep=\",\",fill=T,na.strings=\"\")", 
"if (!(stn %in% info$stn)) {", "   cat(stn,\"not found in stationinfo.txt\\n\")", 
"   cat(\"Assuming just one staff plate exists\\n\")", "   return(\"staff\")", 
"}", "info.df <- as.data.frame(info)", "info <- info.df[info.df$stn == stn, ]", 
"info <- info[!is.na(info)]", "info[-(1:3)]", "}"))
statmode <-
structure(function (x) 
{
    xtab <- table(x)
    xmax <- which(xtab == max(xtab))
    xmode <- names(xmax)
    if (is.numeric(x)) 
        as.numeric(xmode)
    else xmode
}, source = c("function(x) {", "# Calculates the statistical mode (most common value)", 
"  xtab <- table(x)", "  xmax <- which(xtab == max(xtab))", "  xmode <- names(xmax)", 
"  if (is.numeric(x))", "     as.numeric(xmode)", "  else", "     xmode", 
"}"))
stgcalc <-
structure(function (q0, lower, upper, stn, hy) 
{
    qcalc2 <- function(stg, stn, hy, q) {
        qcalc(stn, stg, hy) - q
    }
    options(show.error.messages = FALSE)
    result <- try(uniroot(f = qcalc2, interval = c(lower, upper), 
        tol = 2e-04, stn = stn, hy = hy, q = q0)$root)
    options(show.error.messages = TRUE)
    if (inherits(result, "try-error")) 
        return(NA)
    else return(result)
}, source = c("function(q0, lower, upper, stn, hy)", "{", "\t# Numerical solution to get stage from discharge q0", 
"\t# Need to supply lower and upper limits to search and 3-letter station", 
"\tqcalc2 <- function(stg, stn, hy, q)", "\t{", "\t\tqcalc(stn, stg, hy) - q", 
"\t}", "\toptions(show.error.messages = FALSE)", "\tresult <- try(", 
"\tuniroot(f = qcalc2, interval = c(lower,upper), tol = 0.0002, stn = stn,", 
"\t\thy = hy, q = q0)$root", "\t)", "\toptions(show.error.messages = TRUE)", 
"\tif (inherits(result,\"try-error\")) ", "\t\treturn(NA)", "\telse", 
"\t\treturn(result)", "}"))
subtime <-
structure(function (frame, sdate, stime, edate, etime) 
{
    if (missing(sdate)) {
        schron <- min(frame$chr)
    }
    else {
        if (missing(stime)) 
            stime <- 0
        schron <- make.chron(sdate, stime)
    }
    if (missing(edate)) {
        echron <- max(frame$chr)
    }
    else {
        if (missing(etime)) 
            etime <- 2400
        echron <- make.chron(edate, etime)
    }
    keepers <- (frame$chr >= schron & frame$chr <= echron)
    if (sum(keepers) == 0) 
        print("No records found between specified dates")
    frame[keepers, ]
}, source = c("function(frame, sdate, stime, edate, etime)", 
"{", "\t#  Extracts records from a data frame between specified starting ", 
"\t#  and ending dates and times.  Frame must contain a variable of ", 
"\t#  class \"chron\", and it must be named \"chr\".", "\tif(missing(sdate)) {", 
"\t\tschron <- min(frame$chr)", "\t}", "\telse {", "\t\tif(missing(stime))", 
"\t\t\tstime <- 0", "\t\tschron <- make.chron(sdate, stime)", 
"\t}", "\tif(missing(edate)) {", "\t\techron <- max(frame$chr)", 
"\t}", "\telse {", "\t\tif(missing(etime))", "\t\t\tetime <- 2400", 
"\t\techron <- make.chron(edate, etime)", "\t}", "\tkeepers <- (frame$chr >= schron & frame$chr <= echron)", 
"\tif (sum(keepers) == 0) print(\"No records found between specified dates\")", 
"\tframe[keepers,  ]", "}"))
subtime2 <-
structure(function (frame, sdate, stime, edate, etime) 
{
    sdate <- dates(as.character(sdate), format = "ymd")
    stime <- mt2msm(stime)
    edate <- dates(as.character(edate), format = "ymd")
    etime <- mt2msm(etime)
    frame$msm <- mt2msm(frame$time)
    keep1 <- frame$date > sdate | (frame$date == sdate & frame$msm >= 
        stime)
    keep2 <- frame$date < edate | (frame$date == edate & frame$msm <= 
        etime)
    if (sum(keep1) == 0 && sum(keep2) == 0) 
        print("No records found between specified dates")
    frame[keep1 & keep2, ]
}, source = c("function(frame, sdate, stime, edate, etime)", 
"{", "\t#  Extracts records from a data frame between specified starting ", 
"\t#  and ending dates and times.  Frame must contain a dates object", 
"\t#  named \"date\" and a minutes-since-midnight variable named \"msm\"", 
"\tsdate <- dates(as.character(sdate), format = \"ymd\")", "\tstime <- mt2msm(stime)", 
"\tedate <- dates(as.character(edate), format = \"ymd\")", "\tetime <- mt2msm(etime)", 
"\tframe$msm <- mt2msm(frame$time)", "\tkeep1 <- frame$date > sdate | (frame$date == sdate & frame$msm >= stime", 
"\t\t)", "\tkeep2 <- frame$date < edate | (frame$date == edate & frame$msm <= etime", 
"\t\t)", "\tif (sum(keep1)==0 && sum(keep2)==0) print(\"No records found between specified dates\")", 
"\tframe[keep1 & keep2,  ]", "}"))
sumdays <-
structure(function (months, leap) 
{
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if (leap) 
        days[2] <- 29
    sum(days[months])
}, source = c("function(months,leap) {", "# Calculate the number of days in specified months", 
"# leap is logical variable indicating leap year (T) or not (F)", 
"   days <- c(31,28,31,30,31,30,31,31,30,31,30,31)", "   if (leap) days[2] <- 29", 
"   sum(days[months])", "}"))
total <-
structure(function (...) 
{
    arglist <- as.list(match.call()[-1])
    names <- unlist(arglist)
    vsum <- 0
    ysum <- 0
    mtx <- NULL
    type <- NULL
    meth <- NULL
    start <- NULL
    end <- NULL
    times <- NULL
    for (objname in names) {
        obj <- eval(as.name(objname))
        start <- c(start, obj$chr[1])
    }
    names <- names[order(start)]
    start <- sort(start)
    for (objname in names) {
        obj <- eval(as.name(objname))
        vsum <- vsum + (obj$cv/100 * obj$yhat)^2
        ysum <- ysum + obj$yhat
        mtx <- rbind(mtx, unlist(obj[c("yhat", "n", "r2", "s", 
            "cv")]))
        if (!is.null(obj$bias)) {
            type.bias <- paste(obj$type, obj$bias, sep = ":")
            type <- c(type, type.bias)
        }
        else type <- c(type, obj$type)
        meth <- c(meth, obj$meth)
        end <- c(end, last.val(obj$chr))
        times <- c(times, obj$chr)
    }
    dimnames(mtx)[[1]] <- names
    df <- data.frame(round(mtx, 3))
    df$type <- type
    surrogates <- c("turb", "flow", "time")
    df$surr <- surrogates[meth]
    start <- format(chron(start))
    end <- format(chron(end))
    df$start <- substring(start, 2, 15)
    df$end <- substring(end, 2, 15)
    if (length(table(diff(round(1440 * times)))) > 1) 
        cat("Warning: Objects are non-sequential: check for gaps and overlaps\n")
    df <- df[1:dim(df)[1], c("start", "end", "type", "surr", 
        "yhat", "n", "r2", "s", "cv")]
    print(df)
    cat("Total load =", round(ysum), "kg")
    if (!is.na(vsum)) 
        cat(",  CV =", round((100 * sqrt(vsum))/ysum, 2), "%\n")
    else cat("\n")
    invisible(df)
}, source = c("function(...)", "{", "\t# Replaces sum.tot, tts.tot, and msc.tot: June 29, 2005  JL", 
"\t# Extracts summary stats from a set of modeling objects.", 
"\t# The modeling objects are supplied as arguments to this function", 
"\t# e.g. sum.tot(obj1, obj2, obj3)", "\targlist <- as.list(match.call()[-1])", 
"\tnames <- unlist(arglist)", "\tvsum <- 0", "\tysum <- 0", "\tmtx <- NULL", 
"\ttype <- NULL", "\tmeth <- NULL", "\tstart <- NULL", "\tend <- NULL", 
"\ttimes <- NULL", "\t# Put objects in order", "\tfor(objname in names) {", 
"\t\tobj <- eval(as.name(objname))", "\t\tstart <- c(start, obj$chr[1])", 
"\t}", "\tnames <- names[order(start)]", "\tstart <- sort(start)", 
"\t# Bind information from the modeling objects together", "\tfor(objname in names) {", 
"\t\tobj <- eval(as.name(objname))", "\t\tvsum <- vsum + (obj$cv/100 * obj$yhat)^2", 
"\t\tysum <- ysum + obj$yhat", "\t\tmtx <- rbind(mtx, unlist(obj[c(\"yhat\",\"n\",\"r2\",\"s\",\"cv\")]))", 
"\t\tif (!is.null(obj$bias)) {", "\t\t\ttype.bias <- paste(obj$type,obj$bias,sep=\":\")", 
"\t\t\ttype <- c(type, type.bias)", "\t\t}", "\t\telse", "\t\t\ttype <- c(type, obj$type)", 
"\t\tmeth <- c(meth, obj$meth)", "\t\tend <- c(end, last.val(obj$chr))", 
"\t\ttimes <- c(times, obj$chr)", "\t}", "\tdimnames(mtx)[[1]] <- names", 
"\tdf <- data.frame(round(mtx, 3))", "\tdf$type <- type", "\tsurrogates <- c(\"turb\", \"flow\", \"time\")", 
"\tdf$surr <- surrogates[meth]", "\tstart <- format(chron(start))", 
"\tend <- format(chron(end))", "\tdf$start <- substring(start, 2, 15)", 
"\tdf$end <- substring(end, 2, 15)", "\t# Verify that all sequential time differences are equal to the nearest minute   ", 
"\tif(length(table(diff(round(1440 * times)))) > 1) cat(", "\t\t\t\"Warning: Objects are non-sequential: check for gaps and overlaps\\n\"", 
"\t\t\t)", "\t# Reorder the columns for display", "\tdf <- df[1:dim(df)[1], c(\"start\", \"end\", \"type\", \"surr\", \"yhat\", \"n\",", 
"\t\t\"r2\", \"s\", \"cv\")]", "\tprint(df)", "\t# Report summary stats", 
"\tcat(\"Total load =\", round(ysum), \"kg\")", "\tif(!is.na(vsum))", 
"\t\tcat(\",  CV =\", round((100 * sqrt(vsum))/ysum, 2), \"%\\n\")", 
"\telse cat(\"\\n\")", "\t# Invisibly return data frame (since it was already displayed)", 
"\tinvisible(df)", "}"))
total.gui <-
structure(function () 
{
    total.it <- function(panel) {
        with(panel, {
            objkeep <- objnames[obj]
            args <- paste(objkeep, collapse = ",")
            cmd <- paste("total(", args, ")", sep = "")
            eval(parse(text = cmd))
            stn <- env["STN", ]
            hy <- env["HY", ]
            hy2 <- substring(hy, 3, 4)
            saveCommand(stn, hy2, save = savecmd, cmd = cmd)
        })
        panel
    }
    nothing <- function(panel) panel
    env <- getTTSenv()
    objnames <- model.objects()
    panel <- rp.control("Stats and totals", objnames = objnames)
    rp.checkbox(panel, obj, labels = objnames, title = "Select objects to include", 
        action = nothing)
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save command to file", 
        action = nothing)
    rp.button(panel, action = total.it, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run total()", 
"  # Displays stats and total sediment load for model objects", 
"  total.it <- function(panel) {", "    with(panel, {", "      objkeep <- objnames[obj]", 
"      args <- paste(objkeep,collapse=\",\")", "      cmd <- paste(\"total(\",args,\")\",sep=\"\")", 
"      eval(parse(text=cmd))", "      stn <- env[\"STN\",]", 
"      hy <- env[\"HY\",]", "      hy2 <- substring(hy,3,4)", 
"      saveCommand(stn,hy2,save=savecmd,cmd=cmd)", "    })", 
"    panel", "  }", "  nothing <- function(panel) panel", "  env <- getTTSenv()", 
"  objnames <- model.objects()", "  ", "  panel <- rp.control(\"Stats and totals\",objnames=objnames)", 
"  rp.checkbox(panel, obj, labels=objnames, title=\"Select objects to include\", action=nothing)", 
"  rp.checkbox(panel, savecmd, initval = TRUE, label = \"Save command to file\",action=nothing)", 
"  rp.button(panel, action = total.it, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
tts.gui <-
structure(function () 
{
    while (1) {
        choice <- tolower(readline("\nPlot raw or appended files? Enter 'r' or 'a': "))
        if (choice %in% c("r", "a")) 
            break
    }
    if (choice == "r") 
        rawplot.gui()
    else floplot.gui()
}, source = c("function () ", "{", "# New front end 10/16/2007, call this TTS Data Plot Rev 4.0", 
"# Calls rawplot.gui or floplot.gui, depending on user's choice", 
"   while (1) {", "      choice <- tolower(readline(\"\\nPlot raw or appended files? Enter 'r' or 'a': \"))", 
"      if (choice %in% c(\"r\",\"a\")) break", "}", "   if (choice == \"r\")", 
"       rawplot.gui()", "   else", "       floplot.gui()", "}"
))
tts.rawplot <-
structure(function (dump, tclvar, schron, echron, minstg) 
{
    attach(tclvar, warn = F)
    on.exit(detach(tclvar))
    msgflag <- 0
    if (is.null(dev.list())) 
        win.graph(width = 10, height = 7, pointsize = 12)
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
}, source = c("function (dump,tclvar,schron,echron,minstg) ", 
"{", "  #modified 11/25/2002 J.Lewis   Removed interception plotting options", 
"  #  Changed color schemes.  Turbidity and air temperature plot in \"red\"", 
"  #modified 03/23/2001 R.Field   Added interception plotting options.", 
"  attach(tclvar,warn=F)", "  on.exit(detach(tclvar))", "  msgflag<-0", 
"  if (is.null(dev.list()))", "    win.graph(width = 10, height = 7, pointsize = 12)", 
"  if(tclvalue(pright)==1) par(mar = c(5, 4, 4, 4) + 0.1)", "  else par(mar = c(5, 4, 4, 2) + 0.1)", 
"  if(tclvalue(pright)==1) {", "    if(tclvalue(choice)==\"stage-turbidity\") {", 
"      ltitle<-\"stage\"", "      rtitle<-\"turbidity\"", "    }", 
"    else if(tclvalue(choice)==\"discharge-turbidity\") {", "      ltitle<-\"discharge\"", 
"      rtitle<-\"turbidity\"", "    }", "    else if(tclvalue(choice)==\"discharge-rainfall\") {", 
"      ltitle<-\"discharge\"", "      rtitle<-\"cumulative rainfall\"", 
"    }", "    else if(tclvalue(choice)==\"discharge-water temperature\") {", 
"      ltitle<-\"discharge\"", "      rtitle<-\"water temperature\"", 
"    }", "    else {", "      ltitle<-\"water temperature\"", 
"      rtitle<-\"air temperature\"", "    }", "    msgflag <- 1", 
"  }", "  else {", "    ltitle<-tclvalue(choice)", "    msgflag <- 0", 
"  }", "  midpoint <- mean(c(schron,echron))", "  maintitle<-paste(\"TTS DATA PLOT\\n\",\"Station \",toupper(attr(dump,\"stn\")),\": HY \",hydro.year(midpoint),sep=\"\")  ", 
"  ", "  leftcol <- ifelse(tclvalue(choice) %in% c(\"turbidity\",\"air temperature\"),\"red\",\"blue\")", 
"  rightcol <- \"red\"", "  if(tclvalue(choice)==\"rainfall\") {", 
"    missing.rain <- is.na(dump$left)", "    if (sum(missing.rain) > 0) {", 
"       dump$left[missing.rain] <- 0", "       cs <- cumsum(dump$left)", 
"       cs[missing.rain] <- NA", "    }", "    else ", "       cs <- cumsum(dump$left)", 
"    plot.default(x=dump$chron, y=cs,axes=F,type=\"l\",col=leftcol,", 
"    ylim=c(0,sum(dump$left)),main=maintitle,ylab=\"cumulative rainfall\",xlab=\"\")", 
"    text.default(x=dump$chron[length(dump$chron)],sum(dump$left),", 
"      labels=round(sum(dump$left),2),offset=0.5,cex=0.7,col=leftcol)", 
"  }", "  else {    ", "    plot.default(x=dump$chron, y=dump$left,axes=F,type=\"n\",", 
"    ylim=c(as.numeric(tclvalue(min1)),as.numeric(tclvalue(max1))),", 
"    main=maintitle,ylab=ltitle,xlab=\"\") ", "    diffs<-diff(dump$chron)   ", 
"#   Gaps are more than 1.5 times minimum interval, but at least 1% of a day (14.4 min)", 
"    startgaps<-seq(along=diffs)[diffs>1.5*min(diffs) & diffs > 0.01]", 
"    starts<-1+c(0,startgaps)", "    ends<-c(startgaps,length(dump$chron))", 
"    for(i in seq(along=starts)) {", "      series<-starts[i]:ends[i]", 
"      x<-dump$chron[series]", "      y<-dump$left[series]", 
"      lines(x=x,y=y,col=leftcol,err=-1)", "    } ", "  }", " ", 
"  stagechosen <- length(grep(\"stage\",tclvalue(choice)))", 
"  if (stagechosen && !is.na(minstg)) {", "        segments(min(as.numeric(x)),minstg,max(as.numeric(x)),minstg,col=\"blue\",lty=2)", 
"        mtext(c(\"min\",\"stg\"),side=2,at=minstg+c(0.65,-0.25)*strheight(\"minstg\"),line=-0.25,las=1,adj=0,cex=0.7,col=\"blue\")", 
"  }", "    ", "  axis(2,las=2,cex=0.75)", "  minortics(2)", 
"  if(tclvalue(pright)==1) {", "    par(new=T)", "    if(tclvalue(choice)==\"discharge-rainfall\") {      ", 
"       missing.rain <- is.na(dump$right)", "       if (sum(missing.rain) > 0) {", 
"          dump$right[missing.rain] <- 0 ", "          cs <- cumsum(dump$right)         ", 
"          cs[missing.rain] <- NA", "       }", "       else", 
"          cs <- cumsum(dump$right)", "       plot.default(x=dump$chron, y=cs,axes=F,type=\"l\",col=rightcol,", 
"       ylim=c(0,sum(dump$right)),main=maintitle,ylab=\"\",xlab=\"\")", 
"       text.default(x=dump$chron[length(dump$chron)],sum(dump$right),", 
"          labels=round(sum(dump$right),2),offset=0.5,cex=0.7,col=rightcol)", 
"    }", "    else {       ", "       plot.default(x=dump$chron, y=dump$right,axes=F,type=\"n\",", 
"          ylim=c(as.numeric(tclvalue(min2)),as.numeric(tclvalue(max2))),", 
"          ylab=\"\",xlab=\"\")         ", "       for(i in seq(along=starts)) {", 
"          series<-starts[i]:ends[i]", "          x<-dump$chron[series]", 
"          y<-dump$right[series]", "          lines(x=x,y=y,col=rightcol,err=-1)", 
"       }", "    }", "    axis(4,las=2,cex=0.75,tck=-0.015)", 
"    minortics(4)", "    mtext(rtitle,side=4,line=2.75)", "  }", 
"  ", "  turbchosen <- length(grep(\"turbidity\",tclvalue(choice)))", 
"  if(turbchosen) {", "    if(!all(dump$bottle==0,na.rm=T)) {", 
"      xpnts<-dump$chron[dump$bottle>=1]", "      ypnts<-dump$turb[dump$bottle>=1]", 
"      botnum<-dump$bottle[dump$bottle>=1]", "      points(xpnts,ypnts,cex=0.7,err=-1)", 
"      xrange <- par()$usr[2] - par()$usr[1]", "      text(xpnts+0.006*xrange,ypnts,botnum,cex=0.7,adj=0,col=\"black\")", 
"    }", "  }", "  ", "  axis.time(dump$chron)", "  mtext(paste(\"START = First logged point on or after\", format(schron)),side=1,adj=0,line=3,cex=0.5)", 
"  mtext(paste(\"END = Last logged point on or before\", format(echron)),side=1,adj=0,line=3.7,cex=0.5)", 
"  mystamp()", "  box()  ", "", "  if(msgflag==1 || (turbchosen & any(dump$bottle > 0, na.rm=T)))", 
"    winDialog(type = c(\"ok\"), \"Click on the plot to place legend and redisplay dialog\")", 
"  if(turbchosen) {", "    if(all(dump$bottle==0,na.rm=T)) {   ", 
"      if(tclvalue(pright)==1) {", "        location <- locator(1)", 
"        xleg <- location$x", "        yleg <- location$y     ", 
"        legend(x=xleg,y=yleg,legend=c(ltitle,rtitle),col=c(leftcol,rightcol),", 
"          lty = c(1, 1),pch=c(-1,-1),cex = 0.7)", "      }", 
"    }", "    else {  ", "      location <- locator(1)", "      xleg <- location$x", 
"      yleg <- location$y              ", "      if(tclvalue(pright)!=1)   ", 
"        legend(x=xleg,y=yleg,legend=c(ltitle,\"bottle\"),col=c(leftcol,\"black\"),lty = c(1,-1),", 
"          pch=c(-1,1),cex = 0.7)", "      else", "        legend(x=xleg,y=yleg,legend=c(ltitle,rtitle,\"bottle\"),col=c(leftcol,rightcol,\"black\"),", 
"          lty = c(1, 1,-1),pch=c(-1,-1,1),cex = 0.7) ", "    }                 ", 
"  }", "  else {", "    if(tclvalue(pright)==1) {  ", "      location <- locator(1) ", 
"      xleg <- location$x", "      yleg <- location$y", "      legend(x=xleg,y=yleg,legend=c(ltitle,rtitle),col=c(leftcol,rightcol),lty = c(1, 1),cex = 0.7)", 
"    }  ", "  }", "}"))
tts.ssc <-
structure(function (...) 
{
    arglist <- as.list(match.call()[-1])
    chr <- chron(NULL)
    ssc <- numeric(0)
    meth <- numeric(0)
    yhat <- 0
    for (name in arglist) {
        obj <- eval(name)
        ssc <- append(ssc, obj$predssc)
        chr <- append(chr, obj$chr)
        meth <- append(meth, rep(obj$meth, length(obj$chr)))
        yhat <- yhat + obj$yhat
    }
    ssc[ssc < 0] <- 0
    cat("yhat:", round(yhat), "\n")
    dat <- data.frame(chr, ssc, meth)
    dat <- dat[order(chr), ]
    row.names(dat) <- 1:dim(dat)[1]
    dat
}, source = c("function(...)", "{", "\t# Saves an object containing date and time, concentration, and ", 
"\t#    sediment surrogate (turbidity = 1, discharge = 2, time = 3)", 
"\t# New version Jun 29, 2005 works with new modeling functions.", 
"\t# No longer needs to distinguish object types since new objects", 
"\t# always contain a \"meth\" component.", "\t# e.g. tts.ssc(obj1,obj2,obj3) works fine", 
"\t# Estimation methods are turbidity = 1, discharge = 2, time = 3", 
"\targlist <- as.list(match.call()[-1])", "\tchr <- chron(NULL)", 
"\tssc <- numeric(0)", "\tmeth <- numeric(0)", "\tyhat <- 0", 
"\tfor(name in arglist) {", "\t\tobj <- eval(name)", "\t\tssc <- append(ssc, obj$predssc)", 
"\t\tchr <- append(chr, obj$chr)", "\t\tmeth <- append(meth, rep(obj$meth, length(obj$chr)))", 
"\t\tyhat <- yhat + obj$yhat", "\t}", "\tssc[ssc < 0] <- 0", 
"\tcat(\"yhat:\", round(yhat), \"\\n\")", "\tdat <- data.frame(chr, ssc, meth)", 
"\tdat <- dat[order(chr),  ]", "\trow.names(dat) <- 1:dim(dat)[1]", 
"\tdat", "}"))
tts.ssc.gui <-
structure(function () 
{
    save.it <- function(panel) {
        with(panel, {
            stn <- env["STN", ]
            hy <- env["HY", ]
            hy2 <- substring(hy, 3, 4)
            if (substring(hy, 3, 3) == "0") 
                hy2 <- substring(hy, 4, 4)
            else hy2 <- substring(hy, 3, 4)
            event <- zfill(seqno, 2)
            setTTSenv("INTERSTORM", interstorm)
            if (interstorm && !(event %in% c("00", "99"))) 
                rp.messagebox("For interstorm data enter:\n  00 if surrogate is flow\n  99 if surrogate is turbidity", 
                  title = "Interstorm warning")
            result <- paste(stn, hy2, event, ".ssc", sep = "")
            objkeep <- objnames[obj]
            args <- paste(objkeep, collapse = ",")
            cmd <- paste("tts.ssc(", args, ")", sep = "")
            res <- eval(parse(text = cmd))
            if (seqno != "") {
                cmd <- paste(result, " <- ", cmd, sep = "")
                assign(result, res, envir = .GlobalEnv)
                cat("Result saved in workspace as", result, "\n")
            }
            else cat("To save results, you must enter name of output object\n")
            saveCommand(stn, hy2, save = savecmd, cmd = cmd)
        })
        panel
    }
    nothing <- function(panel) panel
    env <- getTTSenv()
    init.interstorm <- env["INTERSTORM", ]
    objnames <- model.objects()
    panel <- rp.control("Save SSC time series", func = total, 
        objnames = objnames)
    rp.checkbox(panel, obj, labels = objnames, title = "Select objects to include", 
        action = nothing)
    my.textentry(panel, seqno, label = "Storm sequence number (0-99)", 
        action = nothing, initval = "")
    rp.checkbox(panel, interstorm, initval = init.interstorm, 
        label = "Interstorm data only", action = nothing)
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save command to file", 
        action = nothing)
    rp.button(panel, action = save.it, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run tts.ssc()", 
"  # For a set of model objects, saves a time series data frame", 
"  #    containing date, time, SSC, and surrogate method", "  save.it <- function(panel) {", 
"    with(panel, {", "      stn <- env[\"STN\",]", "      hy <- env[\"HY\",]", 
"      hy2 <- substring(hy,3,4)", "      if (substring(hy,3,3)==\"0\") ", 
"        hy2 <- substring(hy,4,4)", "      else", "        hy2 <- substring(hy,3,4)", 
"      event <- zfill(seqno,2)", "      setTTSenv(\"INTERSTORM\",interstorm)", 
"      if (interstorm && !(event %in% c(\"00\",\"99\")))", "        rp.messagebox(\"For interstorm data enter:\\n  00 if surrogate is flow\\n  99 if surrogate is turbidity\",", 
"                        title=\"Interstorm warning\")", "      result <- paste(stn,hy2,event,\".ssc\",sep=\"\")", 
"      ", "      objkeep <- objnames[obj]", "      args <- paste(objkeep,collapse=\",\")", 
"      cmd <- paste(\"tts.ssc(\",args,\")\",sep=\"\")     ", 
"      res <- eval(parse(text=cmd))", "      if (seqno!=\"\") {", 
"        cmd <- paste(result,\" <- \",cmd,sep=\"\")", "        assign(result,res,envir=.GlobalEnv)", 
"        cat(\"Result saved in workspace as\",result,\"\\n\")", 
"      }", "      else cat(\"To save results, you must enter name of output object\\n\")", 
"      saveCommand(stn,hy2,save=savecmd,cmd=cmd)", "    })", 
"    panel", "  }", "  nothing <- function(panel) panel", "  env <- getTTSenv()", 
"  init.interstorm <- env[\"INTERSTORM\",]", "  objnames <- model.objects()", 
"  ", "  panel <- rp.control(\"Save SSC time series\",func=total,objnames=objnames)", 
"  rp.checkbox(panel, obj, labels=objnames, title=\"Select objects to include\", action=nothing)", 
"  my.textentry(panel, seqno, label=\"Storm sequence number (0-99)\", action=nothing, initval=\"\")", 
"  rp.checkbox(panel, interstorm, initval = init.interstorm, ", 
"              label = \"Interstorm data only\",action=nothing)", 
"  rp.checkbox(panel, savecmd, initval = TRUE, label = \"Save command to file\",action=nothing)", 
"  rp.button(panel, action = save.it, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
tts.stats <-
structure(function (sam, pop, interval = 10, var = T, units = "cfs") 
{
    k <- 0.06 * interval
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    x1 <- cbind(rep(1, n), sam$turb)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% sam$ssc
    poppredssc <- b[1] + b[2] * pop$turb
    good <- poppredssc >= 0
    yhat <- sum(k * popq[good] * poppredssc[good])
    sampredssc <- b[1] + b[2] * sam$turb
    resid <- sampredssc - sam$ssc
    r <- cor(sam$ssc, sampredssc)
    s2 <- sum(resid^2)/(n - 2)
    N <- length(popq)
    if (var) {
        x2 <- cbind(k * popq, k * popq * pop$turb)
        V <- x2 %*% invxx %*% t(x2)
        sumV <- sum(V)
        estvar <- s2 * sumV
        cv <- (100 * sqrt(estvar))/yhat
    }
    else cv <- NULL
    list(yhat = yhat, N = N, n = n, negs = sum(!good), coef = b, 
        r2 = r^2, s = sqrt(s2), cv = cv)
}, source = c("function(sam, pop, interval = 10, var = T, units = \"cfs\")", 
"{", "\t#  Computes a sediment load and sundry statistics.", 
"\t#  ssc is estimated using linear regression on turbidity.", 
"\t#  Arguments: ", "\t#    sam- data frame with columns named turb and ssc:", 
"\t#         sample values of turbidity and ssc (mg/L)", "\t#    pop- data frame with columns named turb and q:", 
"\t#         population values of turbidity and discharge (cfs)", 
"\t#    interval- time interval in pop data frame (minutes)", 
"\t#    var- if T then CV is calculated, if F then not", "\t#    units- \"cfs\" or \"cumecs\"", 
"\t#  Return value is a list with the following components:", 
"\t#    yhat = estimated load in kg", "\t#    N = Number of intervals estimated", 
"\t#    n = regression sample size", "\t#    negs = number of negative ssc predictions converted to 0", 
"\t#    coef = regression coefficients (intercept, slope)", "\t#    r2 = regression r-squared", 
"\t#    s = residual standard error for the regression", "\t#    cv = standard error of the estimated load as a percentage", 
"\tk <- 0.059999999999999998 * interval", "\t# Convert from cfs to m3/sec", 
"\tif(units == \"cfs\") qfactor <- 35.31467 else if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpopq <- pop$q/qfactor", "\tn <- dim(sam)[1]", "\tx1 <- cbind(rep(1, n), sam$turb)", 
"\txx <- t(x1) %*% x1", "\tinvxx <- solve(xx)", "\ttmp <- invxx %*% t(x1)", 
"\tb <- tmp %*% sam$ssc", "\tpoppredssc <- b[1] + b[2] * pop$turb", 
"\tgood <- poppredssc >= 0", "\tyhat <- sum(k * popq[good] * poppredssc[good])", 
"\tsampredssc <- b[1] + b[2] * sam$turb", "\tresid <- sampredssc - sam$ssc", 
"\tr <- cor(sam$ssc, sampredssc)", "\ts2 <- sum(resid^2)/(n - 2)", 
"\tN <- length(popq)", "\tif(var) {", "\t\tx2 <- cbind(k * popq, k * popq * pop$turb)", 
"\t\tV <- x2 %*% invxx %*% t(x2)", "\t\tsumV <- sum(V)", "\t\testvar <- s2 * sumV", 
"\t\tcv <- (100 * sqrt(estvar))/yhat", "\t}", "\telse cv <- NULL", 
"\tlist(yhat = yhat, N = N, n = n, negs = sum(!good), coef = b, r2 = r^", 
"\t\t2, s = sqrt(s2), cv = cv)", "}"))
tts5.sample <-
structure(function (data, up, dn, stay = 2, revpct = c(10, 20), 
    repwait = 18, minstg = 0, turblim = 2000, limskip = 2, initcode = 3, 
    revval = 5, startwait = 72) 
{
    sam <- numeric(0)
    thresh <- numeric(0)
    thrcode <- numeric(0)
    thrcount <- revcount <- 0
    lastris <- rep(-999, length(up))
    lastfal <- rep(-999, length(dn))
    lastsam <- -999
    tmax <- tmin <- data$turb[1]
    tmin <- tmax + 1
    tcode <- initcode
    j <- 1
    if (tcode == 1) 
        j <- 1 + sum(data$turb[1] >= up)
    else if (tcode == 2) 
        j <- sum(data$turb[1] > dn)
    i <- 0
    imax <- length(data$turb)
    while (i < imax) {
        while (data$stg[i + 1] < minstg) {
            tcode <- 0
            i <- i + 1
            if (i == imax) 
                break
        }
        if (tcode == 0 && i < imax) {
            if ((data$turb[i + 1] >= up[1]) && (i - startwait > 
                lastsam)) {
                i <- i + 1
                lastsam <- i
                sam <- c(sam, i)
                thrcode <- c(thrcode, 4)
                thresh <- c(thresh, NA)
            }
            j <- 1 + sum(data$turb[i] >= up)
            tcode <- 1
            tmax <- data$turb[i]
        }
        else if (tcode == 3) {
            if (i == 0) 
                i <- i + 1
            else if (i == 1) {
                if (data$turb[1] < data$turb[2]) {
                  tcode <- 1
                  j <- 1 + sum(data$turb[2] >= up)
                }
                else {
                  tcode <- 2
                  j <- sum(data$turb[2] > dn)
                }
            }
        }
        while (tcode == 1 && i < imax) {
            if (data$stg[i + 1] < minstg) 
                break
            i <- i + 1
            t <- data$turb[i]
            if (j <= length(up) & t >= up[j]) {
                thrcount <- thrcount + 1
                if (thrcount >= stay) {
                  if (i > lastris[j] + repwait) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, up[j])
                    lastris[j] <- i
                    lastsam <- i
                  }
                  j <- 1 + sum(t >= up)
                  thrcount <- 0
                }
            }
            else if (t >= turblim) {
                if (i > lastsam + limskip) {
                  sam <- c(sam, i)
                  thrcode <- c(thrcode, 5)
                  thresh <- c(thresh, NA)
                  lastsam <- i
                  revcount <- 0
                }
            }
            if (t >= tmax) {
                tmax <- t
                revcount <- 0
            }
            else if (t < data$turb[i - 1] && t < turblim) {
                revcount <- revcount + 1
                revthresh <- tmax - max(revval, 0.01 * revpct[1] * 
                  tmax)
                if (t <= revthresh && revcount >= stay) {
                  tcode <- 2
                  j <- sum(t > dn)
                  tmin <- t
                  if ((j < length(dn)) && (tmax > dn[j + 1]) && 
                    (i > lastfal[j + 1] + repwait)) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, dn[j + 1])
                    lastfal[j + 1] <- i
                    lastsam <- i
                  }
                }
            }
        }
        thrcount <- 0
        revcount <- 0
        while (tcode == 2 && (i < imax)) {
            if (data$stg[i + 1] < minstg) 
                break
            i <- i + 1
            t <- data$turb[i]
            if (j > 0 && t <= dn[j]) {
                thrcount <- thrcount + 1
                if (thrcount >= stay) {
                  if (i > lastfal[j] + repwait) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, dn[j])
                    lastfal[j] <- i
                    lastsam <- i
                  }
                  j <- sum(t > dn)
                  thrcount <- 0
                }
            }
            if (t <= tmin) {
                tmin <- t
                revcount <- 0
            }
            else if (t > data$turb[i - 1] && t < turblim) {
                revcount <- revcount + 1
                revthresh <- tmin + max(revval, 0.01 * revpct[2] * 
                  tmin)
                if (t >= revthresh && revcount >= stay) {
                  tcode <- 1
                  j <- 1 + sum(t >= up)
                  tmax <- t
                  if ((j > 1) && (tmin < up[j - 1]) && (i > lastris[j - 
                    1] + repwait)) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, up[j - 1])
                    lastris[j - 1] <- i
                    lastsam <- i
                  }
                }
            }
            else if (t >= turblim) {
                tcode <- 1
                tmax <- t
                j <- 1 + sum(t >= up)
                thrcount <- 0
                if (i > lastsam + limskip) {
                  sam <- c(sam, i)
                  thrcode <- c(thrcode, 5)
                  thresh <- c(thresh, NA)
                  lastsam <- i
                  revcount <- 0
                }
            }
        }
        thrcount <- 0
        revcount <- 0
    }
    result <- data.frame(sam, thrcode, thresh)
    attr(result, "minstg") <- minstg
    result
}, source = c("function(data, up, dn, stay = 2, revpct = c(10, 20), repwait = 18, minstg = 0, turblim = 2000, limskip = 2, initcode = 3, revval = 5, startwait = 72)", 
"{", "\t#  Simulator to partially mimic sampling logic of Campbell TTS code", 
"\t#  Omits logic related to data dumps, equipment changes, and temperature conditions", 
"\t#  Rev  3.1  JL  061018 fixed bug that could cause a crash coming out of baseflow", 
"\t#       if j=0 because last falling turbidity was below all thresholds.", 
"\t#       Just had to move the statement that sets j when exiting baseflow", 
"\t#  Rev  3.0  JL  060927 incorporates changes corresponding to Campbell TTS rev 5.0", 
"\t#       Incorporates lastris and lastfal arrays", "\t#       Uses lastsam to remember latest sample of any kind", 
"\t#       No longer needs a zero or 9999 threshold", "\t#       \"basewait\" parameter renamed \"startwait\" to match Campbell code", 
"\t#       New defaults: repwait = 18, startwait = 72", "\t#       Added a column to ouput: the sampled threshold", 
"\t#       Columns are now: (1) interval number (2) threshold code (3) threshold", 
"\t#  Rev  2.2  JL  051103 added revval and basewait to argument list", 
"        #       Fixed bug where last.rsam was not being set for startup samples", 
"\t#       Changed baseflow condition to exclude equality condition (stg=minstg)", 
"\t#  Rev  2.1  JL  051028 changed initcode default to 3 (rising status undetermined)", 
"\t#       With default initcode, rising status is determined at second interval", 
"\t#  Rev  2.0  JL  051027 disabled lowest rising and falling thresholds (normally zero).  ", 
"\t#       Treatment of zero rising threshold differs from Campbell code which seems to ", 
"\t#       be able to sample the zero threshold but rarely does when stg > minstg.", 
"\t#  Rev  1.9  JL  041027 added minimum stage as an attribute to the returned data frame", 
"\t#  Rev  1.8  JL  040323 moved the \"switch to rising\" code snippet out of the \"if\"", 
"\t#       block within the falling turbidity \"saturation\" code", 
"\t#  Rev  1.7  JL  040322 made 3 changes in \"saturation\" logic to match pseudocode", 
"\t#     - Comparisons for saturation turbidity include equality (t >= turblim)", 
"\t#     - Checks for saturation turbidity during falling condition.  At Rev 1.4,", 
"\t#            this block had been mistakenly put in the 'rising' while block", 
"\t#     - Suppresses rising-to-falling reversal count when saturated ", 
"\t#  Rev  1.6  JL  040130 added initcode option to allow starting in falling condition", 
"\t#     And no longer gives error if no samples are produced in the simulation", 
"\t#  Rev  1.5  JL  031218 fixed bug: tmax had not been set when coming out of baseflow", 
"\t#  Rev  1.4  JL  031212 updated to include overflow sampling", 
"        #  Rev  1.3  JL  030116 updated to TTS Rev 4.0 sampling logic", 
"        #     Now mimics Campbell TTS sampling exactly when not saturated", 
"        #     Returns indexes to samples and list of threshold codes", 
"        #  Rev. 1.2  JL  950807 revroom parameter dropped, ", 
"        #     was no longer needed after Rev 1.1.", "        #  Rev. 1.1  JL  950804", 
"        #     This version samples after a reversal if a threshold", 
"        #     has been crossed since the prior peak or trough, ", 
"        #     regardless of revroom.", "        #  data is a data frame with stg and turb columns", 
"        #  up: vector of ascending thresholds for rising turbidities", 
"        #  dn: vector of ascending thresholds for falling turbidities", 
"        #  stay: number of intervals required before a threshold    ", 
"        #    or reversal condition is deemed to be met", "        #  revpct: percent change required for detection of reversal", 
"        #    revpct[1] is for peaks and revpct[2] is for troughs.", 
"\t#  revval: absolute change required for detection of reversal", 
"        #    the maximum of the revpct and revval criterion is used", 
"        #  repwait: number of intervals before reusing a threshold", 
"        #  minstg: minimum stage for sampling", "        #  turblim: sensor limit", 
"        #  limskip: number of intervals skipped between samples when turblim exceeded", 
"        #  initcode: initial value for tcode", "\t#      initcode=1 starts simulation in rising condition", 
"\t#      initcode=2 starts simulation in falling condition", 
"\t#      initcode=3 (default) starts simulation in unknown condition", 
"        #  ", "        #  i: index to turbidity vector", "        #  j: index to next turbidity threshold", 
"        #  thrcount: number of intervals meeting threshold condition", 
"        #  revcount: number of intervals meeting reversal condition", 
"        #  tcode: same as thrcode in TTS, but equals 4 for startup sample", 
"        #  thrcode: vector of tcode values", "        sam <- numeric(0)", 
"\tthresh <- numeric(0)", "        thrcode <- numeric(0)", "        thrcount <- revcount <- 0", 
"\tlastris <- rep(-999,length(up))", "\tlastfal <- rep(-999,length(dn))", 
"\tlastsam <- -999", "        tmax <- tmin <- data$turb[1]", 
"\ttmin <- tmax + 1", "\ttcode <- initcode", "\tj <- 1", "        if (tcode == 1)", 
"\t\tj <- 1 + sum(data$turb[1] >= up)", "\telse if (tcode == 2)", 
"\t\tj <- sum(data$turb[1] > dn)", "        i <- 0", "        imax <- length(data$turb)", 
"        while(i < imax) {", "                while(data$stg[i + 1] < minstg) {", 
"                        # Process baseflow data", "                        tcode <- 0", 
"                        i <- i + 1", "                        if(i == imax)", 
"                                break", "                }", 
"                if(tcode == 0 && i < imax) {", "                        # Emerging from baseflow", 
"                        if ((data$turb[i + 1] >= up[1]) && (i - startwait > lastsam)) {", 
"                                # Startup sample", "                                i <- i + 1", 
"\t\t\t\tlastsam <- i", "                                sam <- c(sam, i)", 
"                                thrcode <- c(thrcode, 4)", "\t\t\t\tthresh <- c(thresh, NA)", 
"                        }", "\t\t\tj <- 1 + sum(data$turb[i] >= up)", 
"                        tcode <- 1", "\t\t\ttmax <- data$turb[i]", 
"                }", "\t\telse if (tcode == 3) {", "\t\t\t# Rising condition unknown (first or second interval only)\t\t", 
"\t\t\tif (i == 0)", "\t\t\t   i <- i + 1", "\t\t\telse if (i == 1) {", 
"\t\t\t    # note: we're at interval 2 (i has not yet been incremented)", 
"\t\t\t    # compare first two intervals to set turbidity condition\t\t\t", 
"\t\t\t   if (data$turb[1] < data$turb[2]) {", "\t\t\t\ttcode <- 1", 
"\t\t\t\tj <- 1 + sum(data$turb[2] >= up)", "\t\t\t   }", "\t\t\t   else {", 
"\t\t\t\ttcode <- 2", "\t\t\t\tj <- sum(data$turb[2] > dn)", 
"\t\t\t   }", "\t\t\t}", "\t\t}", "                while(tcode == 1 && i < imax) {", 
"                        # Process rising turbidities", "                        if(data$stg[i + 1] < minstg) break", 
"                        i <- i + 1", "                        t <- data$turb[i]", 
"                        if(j <= length(up) & t >= up[j]) {", 
"                                # Next rising threshold reached", 
"                                thrcount <- thrcount + 1", "                                if(thrcount >= stay) {", 
"\t\t\t\t\t# Threshold condition satisfied", "                                        if(i > lastris[j] + repwait) {", 
"                                        \t# Take a sample", 
"                                        \tsam <- c(sam, i)", 
"                                        \tthrcode <- c(thrcode, tcode)", 
"\t\t\t\t\t\tthresh <- c(thresh,up[j])", "                                        \tlastris[j] <- i", 
"\t\t\t\t\t\tlastsam <- i", "                                        }", 
"                                        j <- 1 + sum(t >= up)", 
"                                        thrcount <- 0", "                                }", 
"                        }", "\t\t\telse if (t >= turblim) {", 
"\t\t\t\tif (i > lastsam + limskip) {", "\t\t\t\t\t# Collect an 'overflow' sample", 
"\t\t\t\t\tsam <- c(sam, i)", "\t\t\t\t\tthrcode <- c(thrcode, 5)", 
"\t\t\t\t\tthresh <- c(thresh, NA)", "\t\t\t\t\tlastsam <- i", 
"\t\t\t\t\trevcount <- 0", "\t\t\t\t}", "\t\t\t}", "                        if(t >= tmax) {", 
"                                # Highest turbidity so far", 
"                                tmax <- t", "                                revcount <- 0", 
"                        }", "                        else if(t < data$turb[i - 1] && t < turblim) {", 
"                                # Turbidity has started to fall", 
"                                revcount <- revcount + 1", "                                revthresh <- tmax - max(revval, 0.01 * revpct[1] *", 
"                                        tmax)", "                                if(t <= revthresh && revcount >= stay) {", 
"                                        # Turbidity has definitely reversed", 
"                                        tcode <- 2", "                                        j <- sum(t > dn)", 
"                                        tmin <- t", "\t\t\t\t\tif ((j < length(dn)) && (tmax > dn[j+1]) &&", 
"                                            (i > lastfal[j+1] + repwait)) {", 
"                                                # Take a sample", 
"                                                sam <- c(sam, i)", 
"                                                thrcode <- c(thrcode, tcode)", 
"\t\t\t\t\t\tthresh <- c(thresh, dn[j+1])", "                                                lastfal[j+1] <- i", 
"\t\t\t\t\t\tlastsam <- i", "                                        }", 
"\t\t\t\t}", "                        }", "                }", 
"                thrcount <- 0", "                revcount <- 0", 
"                while(tcode == 2 && (i < imax)) {", "                        # Process receding turbidity", 
"                        if(data$stg[i + 1] < minstg) break", 
"                        i <- i + 1", "                        t <- data$turb[i]", 
"                        if(j > 0 && t <= dn[j]) {", "                                # Next falling threshold reached ", 
"                                thrcount <- thrcount + 1", "                                if(thrcount >= stay) {", 
"                                        # Threshold condition satisfied", 
"                                        if(i > lastfal[j] + repwait) {", 
"                                                # Take a sample", 
"                                                sam <- c(sam, i)", 
"                                                thrcode <- c(thrcode, tcode)", 
"\t\t\t\t\t\tthresh <- c(thresh,dn[j])", "                                                lastfal[j] <- i", 
"\t\t\t\t\t\tlastsam <- i", "                                        }", 
"                                        j <- sum(t > dn)", "                                        thrcount <- 0", 
"                                }", "                        }", 
"                        if(t <= tmin) {", "                                # Lowest turbidity this recession", 
"                                tmin <- t", "                                revcount <- 0", 
"                        }", "                        else if(t > data$turb[i - 1] && t < turblim) {", 
"                                # Turbidity has started to rise again", 
"                                revcount <- revcount + 1", "                                revthresh <- tmin + max(revval, 0.01 * revpct[2] *", 
"                                        tmin)", "                                if(t >= revthresh && revcount >= stay) {", 
"                                        # Turbidity has definitely reversed", 
"                                        tcode <- 1", "                                        j <- 1 + sum(t >= up)", 
"                                        tmax <- t", "                                        if ((j > 1) && (tmin < up[j-1]) && ", 
"                                            (i > lastris[j-1] + repwait)) {", 
"                                                # Take a sample", 
"                                                sam <- c(sam, i)", 
"                                                thrcode <- c(thrcode, tcode)", 
"\t\t\t\t\t\tthresh <- c(thresh,up[j-1])", "                                                lastris[j-1] <- i", 
"\t\t\t\t\t\tlastsam <- i", "                                        }", 
"                                }", "                        }", 
"\t\t\telse if (t >= turblim) {", "\t\t\t\t# Switch to rising", 
"\t\t\t\ttcode <- 1", "\t\t\t\ttmax <- t", "\t\t\t\tj <- 1 + sum(t >= up)", 
"                                thrcount <- 0", "\t\t\t\tif (i > lastsam + limskip) {", 
"\t\t\t\t\t# Collect an 'overflow' sample", "\t\t\t\t\tsam <- c(sam, i)", 
"\t\t\t\t\tthrcode <- c(thrcode, 5)", "\t\t\t\t\tthresh <- c(thresh,NA)", 
"\t\t\t\t\tlastsam <- i", "\t\t\t\t\trevcount <- 0", "\t\t\t\t}", 
"\t\t\t}", "                }", "                thrcount <- 0", 
"                revcount <- 0", "        }", "        \tresult <- data.frame(sam, thrcode, thresh)", 
"\t\tattr(result,\"minstg\") <- minstg", "\t\tresult", "}"))
ttsplot <-
structure(function (stn, hy, ..., stagetics = seq(0, 4, 0.1), 
    adj = T, number = T, units = "cfs", split = 0.35, grid1, 
    grid2) 
{
    arglist <- as.list(match.call()[-1])
    predobj <- as.list(match.call(expand.dots = F)$...)
    msc.n <- 0
    tts.n <- 0
    n <- 0
    tts <- NULL
    for (name in predobj) {
        n <- n + 1
        obj <- eval(name)
        if (data.class(obj) == "list" && "meth" %in% names(obj)) {
            if (obj$meth == 1 && obj$type != "pairs") {
                tts.n <- tts.n + 1
                if (tts.n > 2) 
                  stop("No more than 2 turbsrc objects permitted")
                objname <- paste("tts", tts.n, sep = "")
            }
            else if (obj$meth %in% 2:3 || obj$type == "pairs") {
                msc.n <- msc.n + 1
                if (msc.n > 3) 
                  stop("No more than 3 flowsrc and lineartime objects allowed")
                objname <- paste("msc", msc.n, sep = "")
            }
            else stop(paste("Invalid meth component for object", 
                name))
            names(arglist)[n + 2] <- objname
        }
        else stop(paste("Inappropriate prediction object", deparse(substitute(name))))
    }
    do.call("oldttsplot", arglist)
}, source = c("function(stn, hy, ..., stagetics = seq(0, 4, 0.1), adj = T,", 
"        number = T, units = \"cfs\", split = 0.35, grid1, grid2) {", 
"        # Jan 25, 2016 added obj$bias==\"comp\" kludge to identify composite method", 
"        # New arguments added by Aaron gridlines1 and gridlines2 add grids to the plots", 
"        # grid1, if given, sets number of gridlines in first plot", 
"        # grid2, if given, sets number of gridlines in second plot", 
"        # New version Jun 29, 2005", "        # Prediction objects can be passed in any order without naming them", 
"        # i.e. ttsplot(\"mun\",04,tts1=obj1,msc1=obj2,msc2=obj3)", 
"        # can now be simply ttsplot(\"mun\",04,obj1,obj2,obj3)", 
"        # This function figures out object types, then calls old function", 
"        arglist <- as.list(match.call()[-1])", "        predobj <- as.list(match.call(expand.dots = F)$...)", 
"        msc.n <- 0", "        tts.n <- 0", "        n <- 0", 
"        tts <- NULL", "        for(name in predobj) {", "                n <- n + 1", 
"                obj <- eval(name)", "                if(data.class(obj) == \"list\" && \"meth\" %in% names(obj)) {", 
"                        if(obj$meth == 1 && obj$type != \"pairs\") {", 
"                                tts.n <- tts.n + 1", "                                if(tts.n > 2)", 
"                                        stop(\"No more than 2 turbsrc objects permitted\"", 
"                                                )", "                                objname <- paste(\"tts\", tts.n, sep = \"\")", 
"                        }", "                        else if(obj$meth %in% 2:3 || obj$type == \"pairs\") {", 
"                                msc.n <- msc.n + 1", "                                if(msc.n > 3)", 
"                                        stop(\"No more than 3 flowsrc and lineartime objects allowed\"", 
"                                                )", "                                objname <- paste(\"msc\", msc.n, sep = \"\")", 
"                        }", "                        else stop(paste(\"Invalid meth component for object\",", 
"                                        name))", "                        names(arglist)[n+2] <- objname", 
"                }", "                else stop(paste(\"Inappropriate prediction object\", deparse(", 
"                                substitute(name)))) ", "        }", 
"        do.call(\"oldttsplot\", arglist)", "}"))
ttsplot.gui <-
structure(function () 
{
    plot.it <- function(panel) {
        with(panel, {
            stn <- tolower(pars["stn"])
            hy4 <- pars["hy4"]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- as.vector(substring(hy4, 3, 4))
            setTTSenv(c("STN", "HY", "ADJUST"), c(stn, hy4, checkvars["adj"]))
            objkeep <- objnames[obj]
            objstring <- paste(objkeep, collapse = ",")
            stnstring <- paste("\"", stn, "\"", sep = "")
            argstring <- paste(stnstring, hy2, objstring, paste("number", 
                checkvars["number"], sep = "="), paste("adj", 
                checkvars["adj"], sep = "="), sep = ",")
            cmd <- paste("ttsplot(", argstring, ")", sep = "")
            eval(parse(text = cmd))
            saveCommand(stn, hy2, save = checkvars["savecmd"], 
                cmd = cmd)
        })
        panel
    }
    nothing <- function(panel) panel
    env <- getTTSenv()
    init.stn <- env["STN", ]
    init.hy <- env["HY", ]
    init.adjust <- env["ADJUST", ]
    objnames <- model.objects()
    pars <- c(init.stn, init.hy)
    panel <- rp.control("Plot model(s) for SSC", objnames = objnames)
    my.textentry(panel, pars, labels = c("Station", "Water year"), 
        names = c("stn", "hy4"), title = "Enter values", initval = pars)
    rp.checkbox(panel, obj, labels = objnames, title = "Select objects to plot", 
        action = nothing)
    rp.checkbox(panel, checkvars, initval = c(init.adjust, T, 
        T), names = c("adj", "number", "savecmd"), action = nothing, 
        labels = c("Adjust SSC to depth-integrated equivalent", 
            "Plot sample numbers", "Save command to file"))
    rp.button(panel, action = plot.it, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}, source = c("function() {", "  # Creates a GUI using rpanel package to run ttsplot() ", 
"  plot.it <- function(panel) {", "    with(panel, {", "      stn <- tolower(pars[\"stn\"])", 
"      hy4 <- pars[\"hy4\"]", "            if (nchar(hy4) < 3) {", 
"        if (nchar(hy4)==2) hy4 <- paste(\"20\",hy4,sep=\"\")", 
"        if (nchar(hy4)==1) hy4 <- paste(\"200\",hy4,sep=\"\")", 
"      }", "      hy2 <- as.vector(substring(hy4,3,4))  # as.vector required to get rid of the name \"hy4\"", 
"      ", "      # Save current values in TTS Environment", "      setTTSenv(c(\"STN\",\"HY\",\"ADJUST\"), c(stn,hy4,checkvars[\"adj\"]))", 
"      objkeep <- objnames[obj]", "      objstring <- paste(objkeep,collapse=\",\")", 
"      stnstring <- paste('\\\"',stn,'\\\"',sep=\"\")", "      argstring <- paste(stnstring,hy2,objstring,", 
"                         paste(\"number\",checkvars[\"number\"],sep=\"=\"),", 
"                         paste(\"adj\",checkvars[\"adj\"],sep=\"=\"),sep=\",\")", 
"      # print(argstring)", "      cmd <- paste(\"ttsplot(\",argstring,\")\",sep=\"\")", 
"      eval(parse(text=cmd))", "      saveCommand(stn,hy2,save=checkvars[\"savecmd\"],cmd=cmd)", 
"    })", "    panel", "  }", "  nothing <- function(panel) panel", 
"  env <- getTTSenv()", "  init.stn <- env[\"STN\",]", "  init.hy <- env[\"HY\",]  ", 
"  init.adjust <- env[\"ADJUST\",]", "  objnames <- model.objects()", 
"  ", "  pars <- c(init.stn,init.hy)", "  panel <- rp.control(\"Plot model(s) for SSC\",objnames=objnames)", 
"  my.textentry(panel,pars,labels=c(\"Station\",\"Water year\"),", 
"               names=c(\"stn\",\"hy4\"), title=\"Enter values\",initval=pars)", 
"  rp.checkbox(panel, obj, labels=objnames, title=\"Select objects to plot\", action=nothing)", 
"  rp.checkbox(panel, checkvars, initval = c(init.adjust,T,T), names=c(\"adj\",\"number\",\"savecmd\"), action=nothing,", 
"              labels= c(\"Adjust SSC to depth-integrated equivalent\",\"Plot sample numbers\",\"Save command to file\"))  ", 
"  rp.button(panel, action = plot.it, title=\"OK\", pos=\"left\",quit=FALSE)", 
"  rp.button(panel, action = nothing, title=\"Done\", pos=\"right\", quit=TRUE)", 
"  rp.do(panel,nothing)", "}"))
turbcomp <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = NULL, long = T, 
    adj = T, var = T, units = "cfs", comp = T, opt = c(0, 0), 
    oldcomp = F) 
{
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
    if (!(is.null(exclude))) 
        sam <- sam[!(sam$turbcode %in% exclude), ]
    print(sam)
    if (any(is.na(sam$ssc) | is.na(sam$turb))) {
        if (all(is.na(sam$ssc) | is.na(sam$turb))) 
            stop("No samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb), ]
        print("missing value(s) removed from sample data")
    }
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$turb <= 0)) 
        stop("Zero or negative value in sample turbidity.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("Sorry, you can't use this method where turbidity values are missing")
    if (any(pop$turb == 0)) {
        print("Zero turbidities found in flo data: changed to 1 NTU")
        pop$turb[pop$turb == 0] <- 1
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    xsample <- log(sam$turb)
    ysample <- log(sam$ssc)
    x1 <- cbind(rep(1, n), xsample)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% ysample
    poppredssc <- b[1] + b[2] * log(pop$turb)
    sampredssc <- b[1] + b[2] * xsample
    resid <- ysample - sampredssc
    r2 <- cor(ysample, sampredssc)^2
    s <- sqrt(sum(resid^2)/(n - 2))
    if (comp) 
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
    else addback <- 0
    newssc <- exp(poppredssc + addback)
    good <- newssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    estflux <- sum(k * popq[good] * newssc[good])
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA, 
            coef = b, type = "logxy", meth = 1, bias = "comp", 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA, 
        coef = b, type = "logxy", meth = 1, bias = "comp")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"        stime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"        exclude = NULL, long = T, adj = T, var = T, units = \"cfs\", ", 
"        comp=T, opt=c(0,0), oldcomp=F)", "{", "        #  Revised 2015mar28   Changed how opt works with composite method, unless oldcomp=T.", 
"        #     See comments in turbsrc() for detailed explanation  ", 
"        #  Revised 2016mar24. Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"        #  Coded 2016jan25 using turbmvue as a template", "        #  Estimates sediment load using composite method (Aulenbach 2006)", 
"        #  based on log-log model of SSC vs. turbidity ", "        #  sam contains sample turbidities, and ssc", 
"        #  pop contains population turbidities and stages at intervals", 
"        #     specified by \"interval\" argument", "        #  sam contains minutes since midnight, pop needs only military time", 
"        #  Extracts records from sample and population files", 
"        #  between specified starting and ending dates and times.", 
"        #  If dumps and bottles are specified, the regression uses only those.", 
"        #  Excludes samples with turbcode matching values in exclude vector.", 
"        #  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"        #  Predicted values of SSC <= 0 are excluded from the flux calculation", 
"        hy <- zfill(hy, 2)", "        k <- 0.06 * interval", 
"        pop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"        sam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"        if(missing(dumps) && missing(bottles)) {", "                # Use bottles between specified start and end dates", 
"                sam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"        }", "        else if(missing(bottles)) {", "                # Use all bottles in specified dumps", 
"                sam <- sam[allmatches(dumps, sam$dump),  ]", 
"        }", "        else {", "                # Use only specified bottles from specified dumps", 
"                sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"                        sam$bottle)),  ]", "        }", "        if (!(is.null(exclude)))", 
"                sam <- sam[!(sam$turbcode %in% exclude),  ]", 
"        print(sam)", "        if(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"                if(all(is.na(sam$ssc) | is.na(sam$turb)))", 
"                        stop(\"No samples match specified criteria\")", 
"                sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", 
"                print(\"missing value(s) removed from sample data\")", 
"        }", "        if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"        if (any(sam$turb<=0)) stop('Zero or negative value in sample turbidity.  Cannot take logarithm.')", 
"        if (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "        pop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"        if(any(is.na(pop$q)))", "                stop(\"\\nSorry, the specified period has missing discharge values\")", 
"        if(any(is.na(pop$turb)))", "                stop(\"Sorry, you can't use this method where turbidity values are missing\")", 
"        if(any(pop$turb == 0)) {", "                print(\"Zero turbidities found in flo data: changed to 1 NTU\")", 
"                pop$turb[pop$turb == 0] <- 1", "        }", 
"        #convert flow to m^3/sec", "        if(units == \"cfs\") ", 
"                qfactor <- 35.31467 ", "        else if(units == \"cumecs\")", 
"                qfactor <- 1", "        else stop(\"flow units must be cfs or cumecs\")", 
"        popq <- pop$q/qfactor", "        n <- dim(sam)[1]", 
"        xsample <- log(sam$turb)", "        ysample <- log(sam$ssc)", 
"        x1 <- cbind(rep(1, n), xsample)", "        xx <- t(x1) %*% x1", 
"        invxx <- solve(xx)", "        tmp <- invxx %*% t(x1)", 
"        b <- tmp %*% ysample", "        poppredssc <- b[1] + b[2] * log(pop$turb)", 
"        sampredssc <- b[1] + b[2] * xsample", "        resid <- ysample - sampredssc", 
"        r2 <- cor(ysample, sampredssc)^2", "        s <- sqrt(sum(resid^2)/(n - 2))", 
"        # The next \"if\" is just for compatibility with other turb... functions", 
"        # This function will normally never be called with comp=F", 
"        if (comp)", "           addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"        else addback <- 0  ", "        newssc <- exp(poppredssc + addback)", 
"        good <- newssc > 0", "        if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"        estflux <- sum(k * popq[good] * newssc[good])", "        if(long)", 
"             list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA,", 
"                        coef = b, type = \"logxy\", meth = 1, bias = \"comp\",", 
"                        chr = pop$chr, turb = pop$turb, predssc = newssc)", 
"        else list(yhat = estflux, n = n, r2 = r2, s = s, cv = NA,", 
"                        coef = b, type = \"logxy\", meth = 1, bias = \"comp\")", 
"}"))
turbduan <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs") 
{
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
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$turb <= 0)) 
        stop("Zero or negative value in sample turbidity.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("Sorry, you can't use this method where turbidity values are missing")
    if (any(pop$turb == 0)) {
        print("Zero turbidities found in flo data: changed to 1 NTU")
        pop$turb[pop$turb == 0] <- 1
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    result <- logxy.duan(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, 
        interval, var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "duan", meth = 1, 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "duan")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\")", 
"{", "      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  5/18/07 now calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates sediment load and variance using log-log model of", 
"\t#  SSC vs. turbidity and Duan's smearing bias-correction", 
"\t#  sam contains sample turbidities, and ssc", "\t#  pop contains population turbidities and stages at intervals", 
"\t#     specified by \"interval\" argument", "\t#  sam contains minutes since midnight, pop needs only military time", 
"\t#  Extracts records from sample and population files", "\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Estimates the sediment load and its variance for that period.", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$turb)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$turb<=0)) stop('Zero or negative value in sample turbidity.  Cannot take logarithm.')", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(is.na(pop$turb)))", "\t\tstop(\"Sorry, you can't use this method where turbidity values are missing\"", 
"\t\t\t)", "\tif(any(pop$turb == 0)) {", "\t\tprint(\"Zero turbidities found in flo data: changed to 1 NTU\")", 
"\t\tpop$turb[pop$turb == 0] <- 1", "\t}", "\t#convert flow to m^3/sec", 
"\tif(units == \"cfs\") ", "\t\tqfactor <- 35.31467 ", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"        result <- logxy.duan(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, interval, var = var)", 
"\tn <- dim(sam)[1]", "        newssc <- result$predssc", "        r2 <- result$rsquare", 
"        s <- result$s", "        coef <- result$betahat", "        estflux <- result$est.load", 
"        cv <- (100 * result$est.rmse)/estflux", "        if(long)", 
"                list(yhat = estflux, n = n, r2 = r2, s = s, cv = ", 
"                        cv, coef = coef, type = \"logxy\", bias = \"duan\", meth = 1,", 
"                        chr = pop$chr, turb = pop$turb, predssc = newssc)", 
"        else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"                        coef = coef, type = \"logxy\", bias = \"duan\")", 
"}"))
turbloess <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs", span = 1, degree = 1, comp = F, 
    opt = c(0, 0), oldcomp = F) 
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
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\", span = ", 
"\t1, degree = 1, comp = F, opt = c(0, 0), oldcomp=F)", "{", 
"      #  Revised 2015mar28   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbsrc() for detailed explanation ", 
"      #  Modified 2016jan25. Implemented composite method.  ALSO changed so that ", 
"      #     negative predictions are ALWAYS set to zero in flux calculation", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #     New: sets negative predictions to zero for estimating the load ", 
"      #        (predssc output component still contains the negatives)", 
"      #  Modified 5/18/07; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Modified 6/13/2006; xypoints needed to be created before following block", 
"\t#  Predicts concentrations using a loess fit of ssc to turbidity", 
"\t#    Extends the range linearly if necessary for prediction", 
"\t#  Calculates a guess for cv based on the correlation matrix", 
"\t#    from the linear model's predictions.  The standard errors", 
"\t#    from loess seem too high, so I'm returning NA for now.", 
"\t#  sam contains sample turbidities, and ssc", "\t#  pop contains population turbidities and stages at intervals", 
"\t#     specified by \"interval\" argument", "\t#  Extracts records from sample and population files", 
"\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Excludes samples whose turbcode matches any value in exclude vector", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\t#  Output component \"predssc\" may contain negative values, but", 
"\t#  only positive values of predssc are included in load estimate", 
"      bias <- NULL", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", 
"\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$turb)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(is.na(pop$turb)))", "\t\tstop(\"\\nSorry, the specified period has missing turbidity values\")", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpop$q <- pop$q/qfactor", "\tn <- dim(sam)[1]", "\tfit1 <- loess(ssc ~ turb, data = sam, span = span, degree = degree,", 
"\t\tfamily = \"gaussian\", model = T)", "\tpred1 <- predict(fit1, newdata = pop, se = T)", 
"\typred <- pred1$fit", "\tsepred <- pred1$se", "\txypoints <- loess.smooth(sam$turb, sam$ssc, span = span, degree", 
"\t\t= degree, family = \"gaussian\")", "\txy <- data.frame(xypoints)", 
"\tif(any(is.na(ypred))) {", "\t\tnp <- length(xy$x)", "\t\tlofit <- lm(y ~ x, data = xy[1:4,  ])", 
"\t\thifit <- lm(y ~ x, data = xy[(np - 3):np,  ])", "\t\tlows <- (pop$turb < xy$x[1])", 
"\t\thighs <- (pop$turb > xy$x[np])", "\t\tse.grid <- predict(fit1, newdata = data.frame(turb = xy$x),", 
"\t\t\tse = T)$se", "\t\tif(any(lows)) {", "\t\t\typred[lows] <- predict(lofit, newdata = data.frame(", 
"\t\t\t\tx = pop$turb[lows]))", "\t\t\tsepred[lows] <- se.grid[1]", 
"\t\t}", "\t\tif(any(highs)) {", "\t\t\typred[highs] <- predict(hifit, newdata = data.frame(", 
"\t\t\t\tx = pop$turb[highs]))", "\t\t\tsepred[highs] <- se.grid[np]", 
"\t\t}", "\t\tif(any(is.na(ypred)))", "\t\t\tstop(\"Extrapolation failed to eliminate missing values\"", 
"\t\t\t\t)", "\t\t# Extrapolate curve to turb=0 and twice the max", 
"\t\tymin <- predict(lofit, newdata = data.frame(x = 0))", "\t\tymax <- predict(hifit, newdata = data.frame(x = 2 * max(pop$", 
"\t\t\tturb)))", "\t\txypoints$x <- c(0, xypoints$x, 2 * max(pop$turb))", 
"\t\txypoints$y <- c(ymin, xypoints$y, ymax)", "\t}", "      if (comp) {", 
"         bias <- \"comp\"", "         resid <- sam$ssc - fitted(fit1)", 
"         addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"         ypred <- ypred + addback", "      }", "      good <- ypred > 0", 
"      if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"      yhat <- sum(k * pop$q[good] * ypred[good])", "      r2 <- cor(fit1$fitted, fit1$fitted + fit1$resid)^2", 
"      s <- fit1$s", "      coef <- c(NA, NA)", "      # Variance code removed", 
"      cv <- NA", "          if(long)", "                list(yhat = yhat, n = n, r2 = r2, s = s, cv = NA, coef = coef,", 
"                     type = \"loess\", xy = xypoints, meth = 1, bias = bias,", 
"                     chr = pop$chr, turb = pop$turb, predssc = ypred)", 
"          else list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = NA, coef = ", 
"                     coef, type = \"loess\", meth = 1, bias = bias, xy = xypoints)", 
"}"))
turbmvue <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs") 
{
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
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$turb <= 0)) 
        stop("Zero or negative value in sample turbidity.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("Sorry, you can't use this method where turbidity values are missing")
    if (any(pop$turb == 0)) {
        print("Zero turbidities found in flo data: changed to 1 NTU")
        pop$turb[pop$turb == 0] <- 1
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    result <- logxy.mvue(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, 
        interval, var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "mvue", meth = 1, 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "mvue")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\")", 
"{", "      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates sediment load and variance using log-log model of", 
"\t#  SSC vs. turbidity and MVUE estimator (Gilroy et al. 1990)", 
"\t#  sam contains sample turbidities, and ssc", "\t#  pop contains population turbidities and stages at intervals", 
"\t#     specified by \"interval\" argument", "\t#  sam contains minutes since midnight, pop needs only military time", 
"\t#  Extracts records from sample and population files", "\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Excludes samples with turbcode matching values in exclude vector.", 
"\t#  Estimates the sediment load and it variance for that period.", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$turb)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$turb<=0)) stop('Zero or negative value in sample turbidity.  Cannot take logarithm.')", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(is.na(pop$turb)))", "\t\tstop(\"Sorry, you can't use this method where turbidity values are missing\")", 
"\tif(any(pop$turb == 0)) {", "\t\tprint(\"Zero turbidities found in flo data: changed to 1 NTU\")", 
"\t\tpop$turb[pop$turb == 0] <- 1", "\t}", "\t#convert flow to m^3/sec", 
"\tif(units == \"cfs\") ", "\t\tqfactor <- 35.31467 ", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"        result <- logxy.mvue(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, interval, var = var)", 
"        n <- dim(sam)[1]", "        newssc <- result$predssc", 
"        r2 <- result$rsquare", "        s <- result$s", "        coef <- result$betahat", 
"        estflux <- result$est.load", "        cv <- (100 * result$est.rmse)/estflux", 
"        if(long)", "                list(yhat = estflux, n = n, r2 = r2, s = s, cv = ", 
"                        cv, coef = coef, type = \"logxy\", bias = \"mvue\", meth = 1,", 
"                        chr = pop$chr, turb = pop$turb, predssc = newssc)", 
"        else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"                        coef = coef, type = \"logxy\", bias = \"mvue\")", 
"}"))
turbpairs <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    interval = 10, opt = 1, long = T, adj = T, units = "cfs") 
{
    hy <- zfill(hy, 2)
    k <- 0.06 * interval
    pop <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    samples <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("\nSorry, the specified period has missing turbidity values")
    samples <- subtime(samples, sdate1, stime1, edate1, etime1)
    mergesam <- samples[, c("chr", "ssc")]
    mergepop <- merge(pop, mergesam, all.x = T)
    mergepop <- mergepop[order(mergepop$chr), ]
    conf <- intersect(names(mergepop), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in turbpairs:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    attach(mergepop)
    on.exit(detach(mergepop))
    if (adj) 
        ssc <- dis.adjust(stn, ssc)
    N <- dim(mergepop)[1]
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
        xy <- approx(x1[!duplicated(x1)], y1[!duplicated(x1)], 
            xout = turb[i])
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
    if (long) 
        list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
            coef = coef, type = "pairs", meth = 1, chr = mergepop$chr, 
            predssc = newssc)
    else list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, 
        coef = coef, type = "pairs")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, interval = 10, opt = 1, long", 
"\t = T, adj = T, units = \"cfs\")", "{", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Extracts records from pop between given starting date/time", 
"\t#  and ending date/time and merges with samples from the", 
"\t#  correct station for the same time period.  Then estimates", 
"\t#  the load using local pairwise functions of turbidity.", 
"\t#  Uses approx() with the 2 points on each side (4 total).", 
"\t#  approx() will not extrapolate beyond those 4 turbidities, so ", 
"\t#  a simple regression is applied to all data when approx returns NA.", 
"\t#  Extrapolates to (0,0) at lower end unless opt = 2, in which", 
"\t#  case the simple regression is applied.", "\t#  The reason 4 points are used instead of two is to reduce the chance", 
"\t#  that predictions will be required outside the range of x.", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\t# ", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsamples <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", "\tif(any(is.na(pop$q)))", 
"\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(is.na(pop$turb)))", "\t\tstop(\"\\nSorry, the specified period has missing turbidity values\")", 
"\tsamples <- subtime(samples, sdate1, stime1, edate1, etime1)", 
"\tmergesam <- samples[, c(\"chr\", \"ssc\")]", "\tmergepop <- merge(pop, mergesam, all.x = T)", 
"\tmergepop <- mergepop[order(mergepop$chr),  ]", "\tconf <- intersect(names(mergepop),objects(1))", 
"\tif (length(conf) > 0) {", "\t\tcat(\"The following objects conflict with object names in turbpairs:\\n\")", 
"\t\tcat(conf,\"\\n\")", "\t\tcat(\"Please remove conflicting objects before proceeding.\\n\")", 
"\t\treturn(invisible())", "\t}", "\tattach(mergepop)", "\ton.exit(detach(mergepop))", 
"\tif(adj) ssc <- dis.adjust(stn,ssc)", "\tN <- dim(mergepop)[1]", 
"\tindex <- 1:N", "\tsam <- index[ - which.na(ssc)]", "\tdiffs <- diff(c(0, sam, N))", 
"\tn <- length(sam)", "\tlow1 <- rep(c(NA, sam), diffs)", "\tlow2 <- rep(c(NA, NA, sam[ - n]), diffs)", 
"\thigh1 <- rep(c(sam, NA), diffs)", "\thigh2 <- rep(c(sam[-1], NA, NA), diffs)", 
"\t# cubic meters per sec", "\tx <- matrix(turb[c(low2, low1, high1, high2)], ncol = 4)", 
"\ty <- matrix(ssc[c(low2, low1, high1, high2)], ncol = 4)", 
"\tif(opt == 1) {", "\t\t# Permit extrapolation down to (0,0)", 
"\t\tx[is.na(x)] <- 0", "\t\ty[is.na(y)] <- 0", "\t}", "\tnewssc <- numeric(N)", 
"\tfor(i in index) {", "\t\t# Estimate ssc with local linear function of discharge", 
"\t\tx1 <- x[i,  ]", "\t\ty1 <- y[i,  ]", "\t\tx1 <- x1[!is.na(x1)]", 
"\t\ty1 <- y1[!is.na(y1)]", "\t\txy <- approx(x1[!duplicated(x1)], y1[!duplicated(x1)], xout = turb[i])", 
"\t\tnewssc[i] <- xy$y", "\t}", "\tunfit <- is.na(newssc)", "\tif(sum(unfit) != 0)", 
"\t\tnewssc[unfit] <- predict.simple(turb[sam], ssc[sam], turb[", 
"\t\t\tunfit])", "\tcoef <- coef(lm(ssc[sam] ~ turb[sam]))", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\testflux <- sum((k * newssc * q)/qfactor)", "\tif(long)", "\t\tlist(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, coef = ", 
"\t\t\tcoef, type = \"pairs\", meth = 1, chr = mergepop$chr,", 
"\t\t\tpredssc = newssc)", "\telse list(yhat = estflux, n = n, r2 = NA, s = NA, cv = NA, coef = coef,", 
"\t\t\ttype = \"pairs\")", "}"))
turbpower <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs", comp = F, opt = c(0, 0), 
    oldcomp = F) 
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
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\", ", 
"      comp=F, opt=c(0,0), oldcomp=F)", "{", "\t#  Revised 2015mar28   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbsrc() for detailed explanation ", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2016jan25.  Excludes negative predictions from composite method flux", 
"      #  Revised 2015aug18.  Added option for composite method", 
"      #  Revised 2011jun14.  Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Predicts concentrations using a power function of turbidity", 
"\t#    estimated by non-linear least squares", "\t#  Approximates CV using the delta method", 
"\t#  sam contains sample turbidities, and ssc", "\t#  pop contains population turbidities and stages at intervals", 
"\t#     specified by \"interval\" argument", "\t#  sam contains minutes since midnight, pop needs only military time", 
"\t#  Extracts records from sample and population files", "\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Uses only samples with turbcode matching values in codes vector", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"      bias <- NULL", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", 
"\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$turb)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(is.na(pop$turb)))", "\t\tstop(\"\\nSorry, the specified period has missing turbidity values\")", 
"\tif(units == \"cfs\")", "\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpop$q <- pop$q/qfactor", "\tn <- dim(sam)[1]", "\tstartdata <- sam[sam$turb != 0 & sam$ssc != 0,  ]", 
"\tstartmodel <- lm(log(ssc) ~ log(turb), data = startdata)", 
"\ta0 <- exp(coef(startmodel)[1])", "\tb0 <- coef(startmodel)[2]", 
"\tfit1 <- nls(ssc ~ a * turb^b, data = sam, start = list(a = a0, b = b0))", 
"\tpred1 <- predict(fit1, newdata = pop)", "\tyhat <- sum(k * pop$q * pred1)", 
"\tr <- cor(sam$ssc, fitted(fit1))", "      resid <- sam$ssc - fitted(fit1)", 
"\ts <- summary(fit1)$sigma", "\tcoef <- coef(fit1)", "\tnames(coef) <- c(\"constant\", \"power\")", 
"      if (comp) {", "         bias <- \"comp\"", "         # Turn off variance computation, interpolate residuals and add to predictions", 
"         var <- F", "         addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"         pred1 <- pred1 + addback", "      }", "      # Exclude zero and negative values of SSC (for composite method)", 
"      good <- pred1 > 0 ", "      if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"      yhat <- sum(k * pop$q[good] * pred1[good])", "      if (var) {", 
"            # Use delta method to approximate covariance matrix", 
"            vcov <- vcov.nls(fit1)", "            b0 <- coef[\"constant\"]", 
"            b1 <- coef[\"power\"]", "            col1 <- pop$turb^b1", 
"            col2 <- b0 * log(pop$turb) * col1", "            x2 <- k * pop$q * cbind(col1, col2)", 
"            V <- try(x2 %*% vcov %*% t(x2))", "            if (inherits(V, \"try-error\")) {", 
"                  print(\"Cannot calculate covariance matrix.  Time series too long\")", 
"                  cv <- NA", "            }", "            else", 
"                  cv <- (100 * sqrt(sum(V)))/yhat", "      }", 
"      else cv <- NA", "      if(long)", "            list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef,", 
"                 type = \"power\", meth = 1, bias = bias, chr = pop$chr, ", 
"                 turb = pop$turb, predssc = pred1)", "      else list(yhat = yhat, n = n, r2 = r^2, s = s, cv = cv, coef = coef,", 
"                 type = \"power\", meth = 1, bias = bias)", 
"}"))
turbqmle <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs") 
{
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
    if (any(sam$ssc <= 0)) 
        stop("Zero or negative value in sample ssc.  Cannot take logarithm.")
    if (any(sam$turb <= 0)) 
        stop("Zero or negative value in sample turbidity.  Cannot take logarithm.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("Sorry, you can't use this method where turbidity values are missing")
    if (any(pop$turb == 0)) {
        print("Zero turbidities found in flo data: changed to 1 NTU")
        pop$turb[pop$turb == 0] <- 1
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    result <- logxy.qmle(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, 
        interval, var = var)
    n <- dim(sam)[1]
    newssc <- result$predssc
    r2 <- result$rsquare
    s <- result$s
    coef <- result$betahat
    estflux <- result$est.load
    cv <- (100 * result$est.rmse)/estflux
    if (long) 
        list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
            coef = coef, type = "logxy", bias = "qmle", meth = 1, 
            chr = pop$chr, turb = pop$turb, predssc = newssc)
    else list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv, 
        coef = coef, type = "logxy", bias = "qmle")
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\")", 
"{", "      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates sediment load and variance using log-log model of", 
"\t#  SSC vs. turbidity and 'naive' QMLE bias-correction", "\t#  sam contains sample turbidities, and ssc", 
"\t#  pop contains population turbidities and stages at intervals", 
"\t#     specified by \"interval\" argument", "\t#  sam contains minutes since midnight, pop needs only military time", 
"\t#  Extracts records from sample and population files", "\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Excludes samples with turbcode matching values in exclude vector.", 
"\t#  Estimates the sediment load and its variance for that period.", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"\tif(missing(dumps) && missing(bottles)) {", "\t\t# Use bottles between specified start and end dates", 
"\t\tsam <- subtime(sam, sdate2, stime2, edate2, etime2)", "\t}", 
"\telse if(missing(bottles)) {", "\t\t# Use all bottles in specified dumps", 
"\t\tsam <- sam[allmatches(dumps, sam$dump),  ]", "\t}", "\telse {", 
"\t\t# Use only specified bottles from specified dumps", "\t\tsam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"\t\t\tsam$bottle)),  ]", "\t}", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$turb)))", "\t\t\tstop(\"No samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "      if (any(sam$ssc<=0)) stop('Zero or negative value in sample ssc.  Cannot take logarithm.')", 
"      if (any(sam$turb<=0)) stop('Zero or negative value in sample turbidity.  Cannot take logarithm.')", 
"\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tif(any(is.na(pop$turb)))", "\t\tstop(\"Sorry, you can't use this method where turbidity values are missing\"", 
"\t\t\t)", "\tif(any(pop$turb == 0)) {", "\t\tprint(\"Zero turbidities found in flo data: changed to 1 NTU\")", 
"\t\tpop$turb[pop$turb == 0] <- 1", "\t}", "\t#convert flow to m^3/sec", 
"\tif(units == \"cfs\") ", "\t\tqfactor <- 35.31467 ", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tresult <- logxy.qmle(sam$turb, sam$ssc, pop$turb, pop$q/qfactor, interval, var = var)", 
"\tn <- dim(sam)[1]", "\tnewssc <- result$predssc", "\tr2 <- result$rsquare", 
"\ts <- result$s", "\tcoef <- result$betahat", "\testflux <- result$est.load", 
"\tcv <- (100 * result$est.rmse)/estflux", "\tif(long)", "\t\tlist(yhat = estflux, n = n, r2 = r2, s = s, cv = ", 
"\t\t\tcv, coef = coef, type = \"logxy\", bias = \"qmle\", meth = 1,", 
"\t\t\tchr = pop$chr, turb = pop$turb, predssc = newssc)", "\telse list(yhat = estflux, n = n, r2 = r2, s = s, cv = cv,", 
"\t\t\tcoef = coef, type = \"logxy\", bias = \"qmle\")", "}"))
turbsqrt <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, units = "cfs", comp = F, opt = c(0, 0), 
    oldcomp = F) 
{
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
    if (any(sam$ssc < 0)) 
        stop("Negative value in sample ssc.  Cannot take square root.")
    if (any(sam$turb < 0)) 
        stop("Negative value in sample turbidity.  Cannot take square root.")
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified segment has missing discharge values")
    if (any(is.na(pop$turb))) 
        stop("\nSorry, the specified segment has missing turbidity values")
    if (any(pop$turb < 0)) 
        stop("Negative value found in population turbidity.  Cannot take square root")
    if (units == "cfs") 
        qfactor <- 35.3147
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    fit <- lm(sqrt(ssc) ~ sqrt(turb), data = sam)
    betahat <- coef(fit)
    resid <- fit$residuals
    n <- length(resid)
    if (comp) {
        bias <- "comp"
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        pred <- predict(fit, newdata = pop)
        newpred <- pred + addback
        if (any(newpred < 0)) {
            cat(sum(newpred < 0), "negative predictions were set to zero before squaring\n")
            newpred[newpred < 0] <- 0
        }
        predssc <- newpred^2
    }
    else {
        bias = "duan"
        duan <- duan.sqrt(fit, newdata = pop)
        predssc <- duan$corrected
    }
    summ <- summary(fit)
    rsquare <- summ$r.sq
    sigma <- summ$sigma
    flux <- k * popq * predssc
    yhat <- sum(flux)
    if (long) 
        list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, 
            coef = betahat, type = "sqrt", bias = bias, meth = 1, 
            chr = pop$chr, turb = pop$turb, predssc = predssc)
    else list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, 
        coef = betahat, type = "sqrt", bias = bias, meth = 1)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, units = \"cfs\",", 
"      comp = F, opt = c(0,0), oldcomp = F)", "{", "      #  Revised 2015mar28   Changed how opt works with composite method, unless oldcomp=T.", 
"      #     See comments in turbsrc() for detailed explanation ", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2016jan25.  Added option for composite method.", 
"      #  Revised 2011jun14   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Modified May 18, 2007; calls dis.adjust to limit extrapolated adjustments", 
"\t#  Estimates sediment load and variance using sqrt-sqrt model of", 
"\t#  SSC vs. turbidity and Duan's smearing bias-correction", 
"\t#  sam contains sample turbidities, and ssc", "\t#  pop contains population turbidities and stages at intervals", 
"\t#     specified by \"interval\" argument", "\t#  sam contains minutes since midnight, pop needs only military time", 
"\t#  Extracts records from sample and population files", "\t#  between specified starting and ending dates and times.", 
"\t#  If dumps and bottles are specified, the regression uses only those.", 
"\t#  Uses only samples with turbcode matching values in codes vector", 
"\t#  Estimates the sediment load and it variance for that period.", 
"\t#  Set adj=T to adjust concentrations using DIS/AUX regressions", 
"\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", "\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"        if(missing(dumps) && missing(bottles)) {", "                # Use bottles between specified start and end dates", 
"                sam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"        }", "        else if(missing(bottles)) {", "                # Use all bottles in specified dumps", 
"                sam <- sam[allmatches(dumps, sam$dump),  ]", 
"        }", "        else {", "                # Use only specified bottles from specified dumps", 
"                sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"                        sam$bottle)),  ]", "        }", "\t  if (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "        print(sam)", "        if(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"                if(all(is.na(sam$ssc) | is.na(sam$turb)))", 
"                        stop(\"No samples match specified criteria\")", 
"                sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", 
"                print(\"missing value(s) removed from sample data\")", 
"        }", "\tif(any(sam$ssc < 0))", "\t\tstop(\"Negative value in sample ssc.  Cannot take square root.\")", 
"\tif(any(sam$turb < 0))", "\t\tstop(\"Negative value in sample turbidity.  Cannot take square root.\"", 
"\t\t\t)", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified segment has missing discharge values\"", 
"\t\t\t)", "\tif(any(is.na(pop$turb)))", "\t\tstop(\"\\nSorry, the specified segment has missing turbidity values\"", 
"\t\t\t)", "\tif(any(pop$turb < 0))", "\t\tstop(\"Negative value found in population turbidity.  Cannot take square root\"", 
"\t\t\t)", "\t#convert flow to m^3/sec", "\tif(units == \"cfs\") ", 
"\t\tqfactor <- 35.3147 ", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpopq <- pop$q/qfactor", "\tfit <- lm(sqrt(ssc) ~ sqrt(turb), data = sam)", 
"\tbetahat <- coef(fit)", "      resid <- fit$residuals", "      n <- length(resid)", 
"      if (comp) {", "          bias <- \"comp\"", "          addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"          pred <- predict(fit, newdata = pop)", "          newpred <- pred+addback", 
"          # Set negative predictions to zero before squaring", 
"          if (any(newpred < 0)) {", "              cat(sum(newpred<0),\"negative predictions were set to zero before squaring\\n\")", 
"              newpred[newpred < 0] <- 0", "          }", "          predssc <- newpred^2", 
"      }", "      else {", "          bias=\"duan\"", "          duan <- duan.sqrt(fit, newdata = pop)", 
"          predssc <- duan$corrected", "      }", "      summ <- summary(fit)", 
"      rsquare <- summ$r.sq", "      sigma <- summ$sigma", "      flux <- k * popq * predssc", 
"      yhat <- sum(flux)", "      if(long)", "           list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA,", 
"                coef = betahat, type = \"sqrt\", bias = bias, meth = 1,", 
"                chr = pop$chr, turb = pop$turb, predssc = predssc)", 
"      else list(yhat = yhat, n = n, r2 = rsquare, s = sigma, cv = NA, coef = ", 
"                betahat, type = \"sqrt\", bias = bias, meth = 1)", 
"}"))
turbsrc <-
structure(function (stn, hy, sdate1, stime1, edate1, etime1, 
    sdate2 = sdate1, stime2 = stime1, edate2 = edate1, etime2 = etime1, 
    dumps, bottles, interval = 10, exclude = TRUE, long = T, 
    adj = T, var = T, type = "linear", bias = "mvue", units = "cfs", 
    span = 1, degree = 1, comp = F, oldcomp = F, opt = c(0, 0)) 
{
    args <- as.list(match.call())
    for (vname in c("dumps", "bottles", "interval", "span", "degree", 
        "opt")) {
        if (vname %in% names(args) && is.character(args[[vname]])) {
            args[[vname]] <- eval(parse(text = args[[vname]]))
            assign(vname, args[[vname]])
        }
    }
    if (type != "linear" && type != "logx") {
        pass <- c("stn", "hy", "sdate1", "stime1", "edate1", 
            "etime1", "sdate2", "stime2", "edate2", "etime2", 
            "dumps", "bottles", "interval", "exclude", "long", 
            "adj", "var", "units", "span", "degree", "comp", 
            "opt", "oldcomp")
        keep <- intersect(pass, names(args))
        if (type == "logxy") {
            if (comp) 
                estfunc <- "turbcomp"
            else estfunc <- paste("turb", bias, sep = "")
        }
        else estfunc <- paste("turb", type, sep = "")
        result <- do.call(estfunc, args[keep])
        return(result)
    }
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
            stop("\nNo samples match specified criteria")
        sam <- sam[!is.na(sam$ssc) & !is.na(sam$turb), ]
        print("missing value(s) removed from sample data")
    }
    if (adj) 
        sam$ssc <- dis.adjust(stn, sam$ssc)
    pop <- subtime(pop, sdate1, stime1, edate1, etime1)
    if (any(is.na(pop$q))) 
        stop("\nSorry, the specified period has missing discharge values")
    popturb <- pop$turb
    if (any(is.na(popturb))) 
        stop("\nSorry, the specified period has missing turbidity values")
    if (type == "logx") {
        if (any(sam$turb <= 0)) 
            stop("\nZero or negative value in sample turbidity.  Cannot take logarithm.")
        sam$turb <- log(sam$turb)
        popturb <- log(popturb)
        if (any(popturb <= 0)) {
            print("\nZero or negative turbidities found in flo data.  Changed to 1 NTU.")
            popturb[popturb <= 0] <- 1
        }
    }
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    popq <- pop$q/qfactor
    n <- dim(sam)[1]
    x1 <- cbind(rep(1, n), sam$turb)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    tmp <- invxx %*% t(x1)
    b <- tmp %*% sam$ssc
    poppredssc <- b[1] + b[2] * popturb
    sampredssc <- b[1] + b[2] * sam$turb
    resid <- sam$ssc - sampredssc
    r <- cor(sam$ssc, sampredssc)
    s2 <- sum(resid^2)/(n - 2)
    if (comp) {
        bias <- "comp"
        var <- F
        addback <- composite(sam$chr, resid, pop$chr, opt, oldcomp)
        poppredssc <- poppredssc + addback
    }
    good <- poppredssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    yhat <- sum(k * popq[good] * poppredssc[good])
    if (var) {
        x2 <- cbind(k * popq, k * popq * popturb)
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Cannot calculate covariance matrix.  Time series too long")
            var <- F
        }
    }
    if (var) {
        sumV <- sum(V)
        estvar <- s2 * sumV
        cv <- (100 * sqrt(estvar))/yhat
    }
    else cv <- NA
    if (long) 
        list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = cv, 
            coef = b, type = type, meth = 1, bias = bias, chr = pop$chr, 
            turb = pop$turb, predssc = poppredssc)
    else list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = cv, 
        coef = b, type = type, meth = 1, bias = bias)
}, source = c("function(stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, stime2 = ", 
"\tstime1, edate2 = edate1, etime2 = etime1, dumps, bottles, interval = 10,", 
"\texclude = TRUE, long = T, adj = T, var = T, type=\"linear\", bias=\"mvue\", ", 
"      units = \"cfs\", span = 1, degree = 1, comp = F, oldcomp=F, opt = c(0,0)) ", 
"{", "      #  Revised 2016apr8.  Changed defaults to units=\"cfs\" and interval=10 for CC", 
"\t#  Revised 2015mar29   Changed how opt works with composite method.  This governs how", 
"      #      residuals are interpolated before the first sample of the specified period", 
"      #      and after the last sample. In the original code, these options had no effect", 
"      #      if additional samples were specified that were outside the estimation period. ", 
"      #      The residual would be interpolated based on the time of the nearest sample", 
"      #      outside the estimation period.  That results in a nearly constant residual", 
"      #      if the nearest sample is far removed in time.  Now the options will work as", 
"      #      follows: 0 = interpolate to zero at endpoint, 1 = constant residual, 2 = use", 
"      #      the nearest residual outside the estimation period. To reproduce estimates ", 
"      #      computed before this revision, use oldcomp=T with the original opt specification.", 
"      #      Note: if option 2 is selected, either (sdate2,stime2,edate2,etime2) or ", 
"      #      dumps and bottles should be specified and should include samples before and/or", 
"      #      after the estimation period.  If such samples are not specified with option 2,", 
"      #      then option 1 will be employed and a warning will be issued. ", 
"      #  Revised 2016mar24   Converts numeric arguments that were passed as strings", 
"      #      This was critical as GUI inputs are strings and command inputs are numeric", 
"      #  Revised 2016mar24.  Changed defaults to units=\"cumecs\" and interval=15 for HRC", 
"      #  Revised 2016jan25.  Composite method implemented (comp=T) for all types but \"pairs\"", 
"      #  Revised 2011jun14   Now tells user when negative predictions are set to zero", 
"      #  Revised 2011feb10   Changed the way exclude works.  The exclude argument", 
"      #     is a logical variable, by default TRUE, governing whether to exclude", 
"      #     samples from analyses according to the column of the same name,", 
"      #     \"exclude\", in the sediment data frame.  This column is set and", 
"      #     modified interactively from the TTS menu.", "      #  Revised 2007may18.  Calls dis.adjust to limit extrapolated adjustments", 
"\t#  Revised 2005jun09.  New arguments: type, bias, span, degree", 
"\t#  Revised 2004aug31.  Reports an error if zeros or negatives are found", 
"\t#      in discharge or SSC and a log-transformation was requested", 
"\t#  Estimates the load and its variance from a model of ssc vs turbidity.", 
"\t#  Obtains sample turbidity and ssc from .sed object", "\t#  Obtains population turbidity from .flo object which also contains", 
"\t#     stages at intervals specified by \"interval\" argument.", 
"\t#  Extracts records from sample and population between specified starting", 
"\t#  and ending dates and times.  If two sets of starting and ending dates ", 
"\t#  and times are specified, the population data are extracted using the", 
"\t#  first set and the sample data are extracted using the second set.", 
"\t#  If dumps and bottles are specified, dates are overridden.", 
"      #    dumpstr = expression for vector of dump numbers to include in plot", 
"      #    bottlestr = expression for vector of bottle numbers to include in plot", 
"      #    dumpstr can be included without bottlestr: all bottles for those dumps will be plotted", 
"      #    if both dumpstr and bottlestr are specified, the vectors must have same length", 
"\t#  Set adj=T to adjust concentrations using DIS regressions", 
"\t#  type can be \"linear\", \"logx\", \"logxy\", \"power\", \"loess\", or \"pairs\"", 
"\t#    When type==\"logxy\", meth argument selects one of 3 ", 
"\t#    bias-correction methods:  \"mvue\", \"duan\", or \"qmle\".  ", 
"\t#    \"mvue\" is the preferred method for small data sets.", 
"\t#    For large data sets (more than, a few days) \"mvue\" bogs down.  ", 
"\t#    \"duan\" is preferred to \"qmle\"", "\t#    \"qmle\" assumes normal residuals.", 
"\t#  When type is \"loess\", use \"span\" to control smoothness", 
"\t#     \"degree\" can be adjusted for loess, but is best left alone", 
"\t#  Output component \"predssc\" may contain negative values, but", 
"\t#  only positive values of predssc are included in load estimate", 
"", "      #  If numeric variables were passed as strings, convert them", 
"      args <- as.list(match.call())", "      for (vname in c(\"dumps\",\"bottles\",\"interval\",\"span\",\"degree\",\"opt\")) {", 
"          if (vname %in% names(args) && is.character(args[[vname]])) {", 
"            args[[vname]] <- eval(parse(text=args[[vname]]))", 
"            assign(vname, args[[vname]])", "          }", "      }", 
"      if(type != \"linear\" && type != \"logx\") {", "            # Just run estimation function and quit", 
"            pass <- c(\"stn\", \"hy\", \"sdate1\", \"stime1\", \"edate1\", \"etime1\",", 
"                      \"sdate2\", \"stime2\", \"edate2\", \"etime2\", \"dumps\", ", 
"                      \"bottles\", \"interval\", \"exclude\", \"long\", \"adj\", \"var\",", 
"                      \"units\", \"span\", \"degree\",\"comp\",\"opt\",\"oldcomp\")", 
"            keep <- intersect(pass, names(args))", "            # estfunc will be turbcomp, turbmvue, turbqmle, turbduan, ", 
"            #    turbpower, turbloess, or turbpairs", "            if(type == \"logxy\") {", 
"                 if (comp) ", "                      estfunc <- \"turbcomp\"", 
"                 else   ", "                      estfunc <- paste(\"turb\", bias, sep = \"\")", 
"            }", "            else estfunc <- paste(\"turb\", type, sep = \"\")", 
"            result <- do.call(estfunc, args[keep])", "            return(result)", 
"      }", "      # remaining code is for type==\"linear\" or type==\"logx\"", 
"      bias <- NULL", "\thy <- zfill(hy, 2)", "\tk <- 0.06 * interval", 
"\tpop <- eval(as.name(paste(stn, hy, \".flo\", sep = \"\")))", 
"\tsam <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"      if(missing(dumps) && missing(bottles)) {", "            # Use bottles between specified start and end dates", 
"            sam <- subtime(sam, sdate2, stime2, edate2, etime2)", 
"      }", "      else if(missing(bottles)) {", "            # Use all bottles in specified dumps", 
"            sam <- sam[allmatches(dumps, sam$dump),  ]", "      }", 
"      else {", "            # Use only specified bottles from specified dumps", 
"            sam <- sam[allmatches(paste(dumps, bottles), paste(sam$dump,", 
"                        sam$bottle)),  ]", "      }", "\tif (exclude & !is.null(sam$exclude))", 
"\t\tsam <- sam[!sam$exclude,  ]", "\tprint(sam)", "\tif(any(is.na(sam$ssc) | is.na(sam$turb))) {", 
"\t\tif(all(is.na(sam$ssc) | is.na(sam$turb)))", "\t\t\tstop(\"\\nNo samples match specified criteria\")", 
"\t\tsam <- sam[!is.na(sam$ssc) & !is.na(sam$turb),  ]", "\t\tprint(\"missing value(s) removed from sample data\")", 
"\t}", "\tif (adj) sam$ssc <- dis.adjust(stn,sam$ssc)", "\tpop <- subtime(pop, sdate1, stime1, edate1, etime1)", 
"\tif(any(is.na(pop$q)))", "\t\tstop(\"\\nSorry, the specified period has missing discharge values\")", 
"\tpopturb <- pop$turb", "\tif(any(is.na(popturb)))", "\t\tstop(\"\\nSorry, the specified period has missing turbidity values\")", 
"\tif(type == \"logx\") {", "\t\tif(any(sam$turb <= 0))", "\t\t\tstop(\"\\nZero or negative value in sample turbidity.  Cannot take logarithm.\")", 
"\t\tsam$turb <- log(sam$turb)", "\t\tpopturb <- log(popturb)", 
"\t\tif(any(popturb <= 0)) {", "\t\t\tprint(\"\\nZero or negative turbidities found in flo data.  Changed to 1 NTU.\")", 
"\t\t\tpopturb[popturb <= 0] <- 1", "\t\t}", "\t}", "\tif(units == \"cfs\")", 
"\t\tqfactor <- 35.31467", "\telse if(units == \"cumecs\")", 
"\t\tqfactor <- 1", "\telse stop(\"flow units must be cfs or cumecs\")", 
"\tpopq <- pop$q/qfactor", "\tn <- dim(sam)[1]", "\tx1 <- cbind(rep(1, n), sam$turb)", 
"\txx <- t(x1) %*% x1", "\tinvxx <- solve(xx)", "\ttmp <- invxx %*% t(x1)", 
"\tb <- tmp %*% sam$ssc", "\tpoppredssc <- b[1] + b[2] * popturb", 
"\tsampredssc <- b[1] + b[2] * sam$turb", "\tresid <- sam$ssc - sampredssc", 
"\tr <- cor(sam$ssc, sampredssc)", "\ts2 <- sum(resid^2)/(n - 2)", 
"      if (comp) {", "         # Turn off variance computation, interpolate residuals, and add to predictions", 
"         bias <- \"comp\"", "         var <- F", "         addback <- composite(sam$chr,resid,pop$chr,opt,oldcomp)", 
"         poppredssc <- poppredssc + addback", "      }", "      good <- poppredssc > 0", 
"      if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"      yhat <- sum(k * popq[good] * poppredssc[good])", "      # estimate variance of yhat", 
"      if (var) { ", "         x2 <- cbind(k * popq, k * popq * popturb)", 
"         V <- try(x2 %*% invxx %*% t(x2))", "         if (inherits(V, \"try-error\")) {", 
"                print(\"Cannot calculate covariance matrix.  Time series too long\")", 
"                var <- F", "         }", "      }", "      if(var) {", 
"         sumV <- sum(V)", "         estvar <- s2 * sumV", "         cv <- (100 * sqrt(estvar))/yhat", 
"      }", "      else", "         cv <- NA", "      if(long)", 
"         list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = cv, coef = b, ", 
"              type = type, meth = 1, bias = bias, chr = pop$chr, ", 
"              turb = pop$turb, predssc = poppredssc)", "      else list(yhat = yhat, n = n, r2 = r^2, s = sqrt(s2), cv = cv, coef = b,", 
"              type = type, meth = 1, bias = bias)", "}"))
turbsscplot <-
structure(function (stn, hy, sdate, stime = 0, edate, etime = 2400, 
    dumpstr, bottlestr, exclude = TRUE, type = "linear", col = T, 
    textsize = 0.6, span = 1, degree = 1, txt = "bottle", ...) 
{
    nhy <- as.numeric(hy)
    prevyr <- ifelse(nhy == 0, "99", zfill(nhy - 1, 2))
    df <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    station.name <- attr(df, "stn")
    stn <- toupper(station.name)
    if (missing(dumpstr) && missing(bottlestr)) {
        if (missing(sdate)) 
            sdate <- paste(prevyr, "0801", sep = "")
        if (missing(edate)) 
            edate <- paste(hy, "0731", sep = "")
        df <- subtime(df, sdate, stime, edate, etime)
        subtitle <- paste(zfill(sdate, 6), ":", zfill(stime, 
            4), " - ", zfill(edate, 6), ":", zfill(etime, 4), 
            sep = "")
    }
    else if (missing(bottlestr)) {
        if (is.numeric(dumpstr)) {
            dumps <- dumpstr
            dumpstr <- deparse(substitute(dumpstr))
        }
        else {
            dumps <- eval(parse(text = dumpstr))
        }
        df <- df[df$dump %in% dumps, ]
        subtitle <- paste("dumps =", dumpstr)
    }
    else {
        if (is.numeric(dumpstr)) {
            dumps <- dumpstr
            dumpstr <- deparse(substitute(dumpstr))
            bottles <- bottlestr
            bottlestr <- deparse(substitute(bottlestr))
        }
        else {
            dumps <- eval(parse(text = dumpstr))
            bottles <- eval(parse(text = bottlestr))
        }
        df <- df[allmatches(paste(dumps, bottles), paste(df$dump, 
            df$bottle)), ]
        subtitle <- paste("dumps =", dumpstr, ", bottles =", 
            bottlestr)
    }
    if (exclude & !is.null(df$exclude)) 
        df <- df[!df$exclude, ]
    if (any(is.na(df$ssc) | is.na(df$turb))) {
        if (all(is.na(df$ssc) | is.na(df$turb))) 
            stop("No samples match specified criteria")
        df <- df[!is.na(df$ssc) & !is.na(df$turb), ]
        print("missing value(s) removed from sample data")
    }
    conf <- intersect(names(df), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in turbsscplot:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    if (!(txt %in% names(df))) 
        stop(paste(txt, "is not a valid column in sed object"))
    attach(df)
    on.exit(detach(df))
    if (type == "logxy") 
        log <- "xy"
    else if (type == "logx") 
        log <- "x"
    else log <- ""
    plot(turb, ssc, type = "n", xlab = "turb", ylab = "ssc", 
        log = log, ...)
    title(paste("Station ", stn, "; ", subtitle, sep = ""), cex.main = 1)
    txtitem <- eval(as.name(txt))
    if (col) {
        udumps <- unique(df$dump)
        for (i in seq(along = udumps)) {
            d <- udumps[i]
            text(turb[dump == d], ssc[dump == d], txtitem[dump == 
                d], cex = textsize, col = i)
        }
    }
    else text(turb, ssc, txtitem, cex = textsize)
    if (type == "linear") {
        fit <- lm(ssc ~ turb, na.action = na.omit)
        abline(fit)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit"
    }
    else if (type == "logx") {
        fit <- lm(ssc ~ log(turb), na.action = na.omit)
        x0 <- range(turb)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit to log(x)"
        y0 <- predict(fit, newdata = data.frame(turb = x0))
        lines(x0, y0)
    }
    else if (type == "logxy") {
        if (any(turb <= 0 | ssc <= 0)) {
            keep <- turb > 0 & ssc > 0
            turb <- turb[keep]
            ssc <- ssc[keep]
        }
        fit <- logline(turb, ssc)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "log-log fit"
    }
    else if (type == "sqrt") {
        fit <- lm(sqrt(ssc) ~ sqrt(turb), subset = (turb >= 0 & 
            ssc >= 0))
        x0 <- seq(min(turb), max(turb), len = 50)
        y0 <- predict(fit, newdata = data.frame(turb = x0))
        lines(x0, y0^2)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "sqrt-sqrt fit"
    }
    else if (type == "power") {
        startdata <- df[turb != 0 & ssc != 0, ]
        startmodel <- lm(log(ssc) ~ log(turb), data = startdata)
        a0 <- exp(coef(startmodel)[1])
        b0 <- coef(startmodel)[2]
        fit <- nls(ssc ~ a * turb^b, start = list(a = a0, b = b0))
        x0 <- seq(min(turb), max(turb), len = 25)
        fit$s <- summary(fit)$sigma
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- "power fit"
        y0 <- predict(fit, newdata = data.frame(turb = x0))
        lines(x0, y0)
    }
    else if (type == "loess") {
        if (span == 0) 
            span <- optimize.loess(turb, ssc, 0.5, 1, by = 0.05, 
                degree)
        lines(loess.smooth(turb, ssc, span = span, degree = degree, 
            family = "gaussian"))
        fit <- loess(ssc ~ turb, degree = degree, span = span, 
            family = "gaussian")
        fit$aicc <- aic.loess(fit)
        fit$s <- summary(fit)$s
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- paste("loess(span=", round(span, 2), ",degree=", 
            degree, ")", sep = "")
    }
    else if (type == "pairs") {
        lines(turb, ssc)
        label <- "pairwise fit"
    }
    mtext(label, side = 3, line = 0.5, cex = textsize)
    if (type != "pairs") 
        if (!is.null(fit$aicc)) 
            list(model = fit, r2 = fit$r2, s = fit$s, AICc = fit$aicc)
        else list(model = fit, r2 = fit$r2, s = fit$s)
}, source = c("function(stn, hy, sdate, stime = 0, edate, etime = 2400, dumpstr, bottlestr, exclude", 
"         = TRUE, type=\"linear\", col = T, textsize = 0.6, span = 1, degree = 1, ", 
"         txt = \"bottle\", ...) {", "  #  Revised 2012sep02 Uses \"stn\" attribute of data in plot title", 
"  #     For GUI, permits dumps and bottles to be specified as string expressions", 
"  #  Revised 2011jul11   Now checks for valid values of txt argument", 
"  #     txt=\"dumps\" formerly created invalid scatterplots", 
"  #     Also improved titling of plots with dumps and bottles arguments", 
"  #  Revised 2011feb10   Changed the way exclude works.  The exclude argument", 
"  #     is a logical variable, by default TRUE, governing whether to exclude", 
"  #     samples from analyses according to the column of the same name,", 
"  #     \"exclude\", in the sediment data frame.  This column is set and", 
"  #     modified interactively from the TTS menu.", "  # Arguments:", 
"  # stn = 3-letter station name (e.g. \"nfc\")", "  # hy = 2-digit water year", 
"  # sdate = start date (e.g. 980104)", "  # stime = start military time (e.g. 1830)", 
"  # edate = end date", "  # etime = end military time", "  # dumpstr = expression for vector of dump numbers to include in plot", 
"  # bottlestr = expression for vector of bottle numbers to include in plot", 
"  #    dumpstr can be included without bottlestr: all bottles for those dumps will be plotted", 
"  #    if both dumpstr and bottlestr are specified, the vectors must have same length", 
"  # exclude = the plot excludes data with turbidity codes matching these", 
"  # type = \"linear\", \"logx\", \"logxy\", \"power\", \"loess\", or \"pairs\"", 
"  # col = whether or not to use color to identify data dumps", 
"  # textsize = relative text size for plotting txt symbols", 
"  # span = smoothing parameter for loess (0 - 1)", "  # degree = degree of loess smooth (1 or 2)", 
"  # txt = name of text item to use as plotting symbols for samples", 
"  # ... other graphical parameters passed through to plot()", 
"  nhy <- as.numeric(hy)", "  prevyr <- ifelse(nhy == 0, \"99\", zfill(nhy - 1, 2))", 
"  df <- eval(as.name(paste(stn, hy, \".sed\", sep = \"\")))", 
"  # Get station name from \"stn\" attribute since interstorm data get renamed", 
"  station.name <- attr(df,\"stn\")", "  stn <- toupper(station.name)", 
"  if(missing(dumpstr) && missing(bottlestr)) {", "    # Use bottles between specified start and end dates", 
"    if(missing(sdate)) sdate <- paste(prevyr, \"0801\", sep = \"\")", 
"    if(missing(edate))", "      edate <- paste(hy, \"0731\", sep = \"\")", 
"    df <- subtime(df, sdate, stime, edate, etime)", "    subtitle <- paste(zfill(sdate,6), \":\", zfill(stime, 4), \" - \", ", 
"                      zfill(edate,6), \":\", zfill(etime, 4),sep=\"\")", 
"  }", "  else if(missing(bottlestr)) {", "    if (is.numeric(dumpstr)) {", 
"      # handles numeric inputs, i.e. when user types the command", 
"      dumps <- dumpstr", "      dumpstr <- deparse(substitute(dumpstr))", 
"    }", "    else {", "      # handles string input, i.e. when command is created from GUI", 
"      dumps <- eval(parse(text=dumpstr))", "    }", "    # Use all bottles in specified dumps", 
"    df <- df[df$dump %in% dumps,  ]", "    # dumparg <- format(match.call()$dumps)", 
"    subtitle <- paste(\"dumps =\", dumpstr)", "  }", "  else {", 
"    if (is.numeric(dumpstr)) {", "      # handles numeric inputs, i.e. when user types the command", 
"      # I assume here that if dumpstr is numeric then so is bottlestr", 
"      dumps <- dumpstr", "      dumpstr <- deparse(substitute(dumpstr))", 
"      bottles <- bottlestr", "      bottlestr <- deparse(substitute(bottlestr))", 
"    }", "    else {", "      # handles string input, i.e. when command is created from GUI", 
"      dumps <- eval(parse(text=dumpstr))", "      bottles <- eval(parse(text=bottlestr))", 
"    }", "    # Use only specified bottles from specified dumps", 
"    df <- df[allmatches(paste(dumps, bottles), paste(df$dump, df$", 
"      bottle)),  ]", "    # dumparg <- format(match.call()$dumps)", 
"    # botarg <- format(match.call()$bottles)", "    subtitle <- paste(\"dumps =\", dumpstr, \", bottles =\", bottlestr)", 
"  }", "  if (exclude & !is.null(df$exclude))", "    df <- df[!df$exclude,  ]", 
"  if(any(is.na(df$ssc) | is.na(df$turb))) {", "    if(all(is.na(df$ssc) | is.na(df$turb)))", 
"      stop(\"No samples match specified criteria\")", "    df <- df[!is.na(df$ssc) & !is.na(df$turb),  ]", 
"    print(\"missing value(s) removed from sample data\")", "  }", 
"  conf <- intersect(names(df),objects(1))", "  if (length(conf) > 0) {", 
"    cat(\"The following objects conflict with object names in turbsscplot:\\n\")", 
"    cat(conf,\"\\n\")", "    cat(\"Please remove conflicting objects before proceeding.\\n\")", 
"    return(invisible())", "  }", "  if (!(txt %in% names(df)))", 
"    stop(paste(txt,\"is not a valid column in sed object\"))", 
"  attach(df)", "  on.exit(detach(df))", "  if(type == \"logxy\")", 
"    log <- \"xy\"", "  else if(type == \"logx\")", "    log <- \"x\"", 
"  else log <- \"\"", "  plot(turb, ssc, type = \"n\", xlab = \"turb\", ylab = \"ssc\", log = log, ...)", 
"  title(paste(\"Station \", stn, \"; \", subtitle, sep=\"\"), cex.main=1)", 
"  txtitem <- eval(as.name(txt))", "  if(col) {", "    udumps <- unique(df$dump)", 
"    for(i in seq(along = udumps)) {", "      d <- udumps[i]", 
"      text(turb[dump == d], ssc[dump == d], txtitem[dump ==", 
"        d], cex = textsize, col = i)", "    }", "  }", "  else text(turb, ssc, txtitem, cex = textsize)", 
"  if(type == \"linear\") {", "    fit <- lm(ssc ~ turb, na.action = na.omit)", 
"    abline(fit)", "    fit$aicc <- aic.lm(fit)[\"aic.c\"]", 
"    fit$s <- summary(fit)$sigma", "    fit$r2 <- summary(fit)$r.sq", 
"    label <- \"linear fit\"", "  }", "  else if(type == \"logx\") {", 
"    fit <- lm(ssc ~ log(turb), na.action = na.omit)", "    x0 <- range(turb)", 
"    fit$aicc <- aic.lm(fit)[\"aic.c\"]", "    fit$s <- summary(fit)$sigma", 
"    fit$r2 <- summary(fit)$r.sq", "    label <- \"linear fit to log(x)\"", 
"    y0 <- predict(fit, newdata = data.frame(turb = x0))", "    lines(x0, y0)", 
"  }", "  else if(type == \"logxy\") {", "    if (any(turb<=0 | ssc<=0)) {", 
"      keep <- turb > 0 & ssc > 0", "      turb <- turb[keep]", 
"      ssc <- ssc[keep]", "    }", "    fit <- logline(turb, ssc)", 
"    fit$s <- summary(fit)$sigma", "    fit$r2 <- summary(fit)$r.sq", 
"    label <- \"log-log fit\"", "  }", "  else if(type == \"sqrt\") {", 
"    fit <- lm(sqrt(ssc) ~ sqrt(turb), subset = (turb >= 0 & ssc >=", 
"      0))", "    x0 <- seq(min(turb), max(turb), len = 50)", 
"    y0 <- predict(fit, newdata = data.frame(turb = x0))", "    lines(x0, y0^2)", 
"    fit$s <- summary(fit)$sigma", "    fit$r2 <- summary(fit)$r.sq", 
"    label <- \"sqrt-sqrt fit\"", "  }", "  else if(type == \"power\") {", 
"    startdata <- df[turb != 0 & ssc != 0,  ]", "    startmodel <- lm(log(ssc) ~ log(turb), data = startdata)", 
"    a0 <- exp(coef(startmodel)[1])", "    b0 <- coef(startmodel)[2]", 
"    fit <- nls(ssc ~ a * turb^b, start = list(a = a0, b = b0))", 
"    x0 <- seq(min(turb), max(turb), len = 25)", "    fit$s <- summary(fit)$sigma", 
"    fit$r2 <- cor(fitted(fit),ssc)^2", "    label <- \"power fit\"", 
"    y0 <- predict(fit, newdata = data.frame(turb = x0))", "    lines(x0, y0)", 
"  }", "  else if(type == \"loess\") {", "    # Uses gaussian family so turbloess can extract RSE", 
"    if(span == 0) span <- optimize.loess(turb, ssc, 0.5, 1, by = ", 
"      0.05, degree)", "    lines(loess.smooth(turb, ssc, span = span, degree = degree,", 
"                       family = \"gaussian\"))", "    fit <- loess(ssc ~ turb, degree = degree, span = span, family", 
"                 = \"gaussian\")", "    fit$aicc <- aic.loess(fit)", 
"    fit$s <- summary(fit)$s", "    fit$r2 <- cor(fitted(fit),ssc)^2", 
"    label <- paste(\"loess(span=\", round(span,2), \",degree=\", degree, \")\", sep = \"\")", 
"  }", "  else if(type == \"pairs\") {", "    lines(turb, ssc)", 
"    label <- \"pairwise fit\"", "  }", "  mtext(label, side = 3, line = 0.5, cex=textsize)", 
"  if(type != \"pairs\")", "    if (!is.null(fit$aicc))", "      list(model=fit,r2=fit$r2,s=fit$s,AICc=fit$aicc)", 
"  else", "    list(model=fit,r2=fit$r2,s=fit$s)", "}"))
two <-
structure(function () 
par(mfrow = c(1, 2)), source = "function() par(mfrow=c(1,2))")
ujccalc <-
structure(function (stg) 
{
    a <- 13.084
    b <- 3.9429
    a * stg^b
}, source = c("function(stg)", "{", "\t# Stg in meters, Q in m3/sec", 
"\ta <- 13.084", "\tb <- 3.9429", "\ta * stg^b", "}"))
usual.mle <-
structure(function (xsam, ysam, qpop, var = T, interval = 10) 
{
    k <- 0.06 * interval
    n <- length(xsam)
    N <- length(qpop)
    x1 <- cbind(rep(1, n), xsam)
    xx <- t(x1) %*% x1
    invxx <- solve(xx)
    betahat <- invxx %*% t(x1) %*% ysam
    yhat <- x1 %*% betahat
    resid <- ysam - yhat
    rsquare <- (cor(ysam, yhat))^2
    if (n == 2) 
        s2 <- 0
    else s2 <- sum(resid^2)/(n - 2)
    poppredssc <- betahat[1] + betahat[2] * qpop
    good <- poppredssc > 0
    if (sum(!good) > 0) 
        cat(sum(!good), "negative predictions were set to zero for the load estimate\n")
    est.load <- sum(k * qpop[good] * poppredssc[good])
    if (var) {
        x2 <- cbind(k * qpop, k * qpop * qpop)
        V <- try(x2 %*% invxx %*% t(x2))
        if (inherits(V, "try-error")) {
            print("Time series too long, cannot compute covariance matrix")
            var <- F
        }
    }
    if (var) {
        sumV <- sum(V)
        estvar <- s2 * sumV
        RMSE <- sqrt(estvar)
    }
    else RMSE <- NA
    list(predssc = poppredssc, est.load = est.load, rsquare = rsquare, 
        betahat = betahat, s = sqrt(s2), est.rmse = RMSE)
}, source = c("function(xsam, ysam, qpop, var = T, interval = 10)", 
"{", "\t#  Rev. 1.0  JL  040602", "\t#  Called by the function globalqmle for untransformed regressions.", 
"\t#  The function qmle does the same thing for a log-log regression.", 
"\t#  Computes the MLE estimate of load and its estimated RMSE for the ", 
"\t#  whole storm, based on an untransformed regression of ssc vs q from a", 
"\t#  a sample (xsam,ysam).  The regression is used to estimate ssc for ", 
"\t#  all intervals including the sampled ones.  qpop is complete flow record.  ", 
"\t#  Negative predictions of concentration are set to 0.", "\t#  var is a logical value that says whether to compute variance or not.", 
"\t#  Units: q (m3/sec), ssc (mg/l), interval (min)", "\t# ", 
"\tk <- 0.06 * interval", "\tn <- length(xsam)", "\tN <- length(qpop)", 
"\tx1 <- cbind(rep(1, n), xsam)", "\txx <- t(x1) %*% x1", "\tinvxx <- solve(xx)", 
"\tbetahat <- invxx %*% t(x1) %*% ysam", "\tyhat <- x1 %*% betahat", 
"\tresid <- ysam - yhat", "\trsquare <- (cor(ysam, yhat))^2", 
"\tif(n == 2)", "\t\ts2 <- 0", "\telse s2 <- sum(resid^2)/(n - 2)", 
"\tpoppredssc <- betahat[1] + betahat[2] * qpop", "\tgood <- poppredssc > 0", 
"      if (sum(!good) > 0) cat(sum(!good),\"negative predictions were set to zero for the load estimate\\n\")", 
"\test.load <- sum(k * qpop[good] * poppredssc[good])", "        if (var) {", 
"\t    x2 <- cbind(k * qpop, k * qpop * qpop) ", "            V <- try(x2 %*% invxx %*% t(x2))", 
"            if (inherits(V,\"try-error\")) {", "                print(\"Time series too long, cannot compute covariance matrix\")", 
"                var <- F", "            }", "        }", "\tif(var) {", 
"\t\tsumV <- sum(V)", "\t\testvar <- s2 * sumV", "\t\tRMSE <- sqrt(estvar)", 
"\t}", "\telse RMSE <- NA", "\tlist(predssc = poppredssc, est.load = est.load, rsquare = rsquare,", 
"\t\tbetahat = betahat, s = sqrt(s2), est.rmse = RMSE)", "}"))
var.check <-
structure(function (tclvar) 
{
    attach(tclvar, warn = F)
    on.exit(detach(tclvar))
    tflag <- T
    tclvalue(sd) <- gsub("-", "/", tclvalue(sd))
    tclvalue(ed) <- gsub("-", "/", tclvalue(ed))
    sd <- try(dates(tclvalue(sd)))
    ed <- try(dates(tclvalue(ed)))
    if (inherits(sd, "try-error")) {
        winDialog(type = c("ok"), "ERROR: Start date is not in the proper format (mm/dd/yy).")
        tflag <- F
    }
    if (inherits(ed, "try-error")) {
        winDialog(type = c("ok"), "ERROR: End date is not in the proper format (mm/dd/yy).")
        tflag <- F
    }
    st <- as.integer(tclvalue(st))
    et <- as.integer(tclvalue(et))
    if (is.integer(st) & !is.na(st)) {
        if (st < 0 | st > 2400) {
            winDialog(type = c("ok"), "ERROR: Start time is required to be between 0 and 2400.")
            tflag <- F
        }
    }
    else {
        winDialog(type = c("ok"), "ERROR: Start time must be an integer value.")
        tflag <- F
    }
    if (is.integer(et) & !is.na(et)) {
        if (et < 0 | et > 2400) {
            winDialog(type = c("ok"), "ERROR: End time is required to be between 0 and 2400.")
            tflag <- F
        }
    }
    else {
        winDialog(type = c("ok"), "ERROR: End time must be an integer value.")
        tflag <- F
    }
    if (tflag != F) {
        schron <- sd + mt2msm(st)/1440
        echron <- ed + mt2msm(et)/1440
        if (schron > echron) {
            winDialog(type = c("ok"), "ERROR: Start date-time is greater than end date-time.")
            tflag <- F
        }
    }
    min1 <- as.numeric(tclvalue(min1))
    max1 <- as.numeric(tclvalue(max1))
    if (is.na(min1)) {
        winDialog(type = c("ok"), "ERROR: The left axis minimum stage must be an numeric value.")
        tflag <- F
    }
    if (is.na(max1)) {
        winDialog(type = c("ok"), "ERROR: The left axis maximum stage must be an numeric value.")
        tflag <- F
    }
    if (!is.na(min1) & !is.na(max1)) {
        if (min1 > max1) {
            winDialog(type = c("ok"), "ERROR: The left axis minimum stage is\ngreater than the left axis maximum stage.")
            tflag <- F
        }
    }
    if (tclvalue(pright) == 1) {
        min2 <- as.numeric(tclvalue(min2))
        max2 <- as.numeric(tclvalue(max2))
        if (is.na(min2)) {
            winDialog(type = c("ok"), "ERROR: The right axis minimum stage must be an numeric value.")
            tflag <- F
        }
        if (is.na(max2)) {
            winDialog(type = c("ok"), "ERROR: The right axis maximum stage must be an numeric value.")
            tflag <- F
        }
        if (!is.na(min2) & !is.na(max2)) {
            if (min2 > max2) {
                winDialog(type = c("ok"), "ERROR: The right axis minimum stage is\ngreater than the right axis maximum stage.")
                tflag <- F
            }
        }
    }
    tflag
}, source = c("function (tclvar) ", "{", "  attach(tclvar,warn=F)", 
"  on.exit(detach(tclvar))", "  tflag<-T", "  tclvalue(sd)<-gsub(\"-\",\"/\",tclvalue(sd))", 
"  tclvalue(ed)<-gsub(\"-\",\"/\",tclvalue(ed))  ", "  sd<-try(dates(tclvalue(sd)))", 
"  ed<-try(dates(tclvalue(ed)))", "  if(inherits(sd,\"try-error\")) {", 
"    winDialog(type=c(\"ok\"),\"ERROR: Start date is not in the proper format (mm/dd/yy).\")", 
"    tflag<-F", "  }", "  if(inherits(ed,\"try-error\")) {", 
"    winDialog(type=c(\"ok\"),\"ERROR: End date is not in the proper format (mm/dd/yy).\")", 
"    tflag<-F", "  }", "   ", "  st<-as.integer(tclvalue(st))", 
"  et<-as.integer(tclvalue(et))", "  if(is.integer(st)&!is.na(st)) {", 
"    if(st<0|st>2400) {", "      winDialog(type=c(\"ok\"),\"ERROR: Start time is required to be between 0 and 2400.\")", 
"      tflag<-F", "    }", "  }", "  else {", "    winDialog(type=c(\"ok\"),\"ERROR: Start time must be an integer value.\")", 
"    tflag<-F  ", "  }", "  if(is.integer(et)&!is.na(et)) {", 
"    if(et<0|et>2400) {", "      winDialog(type=c(\"ok\"),\"ERROR: End time is required to be between 0 and 2400.\")", 
"      tflag<-F", "    }", "  }", "  else {", "    winDialog(type=c(\"ok\"),\"ERROR: End time must be an integer value.\")", 
"    tflag<-F  ", "  }", "  ", "  if(tflag!=F) {", "    schron<-sd+mt2msm(st)/1440", 
"    echron<-ed+mt2msm(et)/1440", "    if(schron>echron) {", 
"      winDialog(type=c(\"ok\"),\"ERROR: Start date-time is greater than end date-time.\")", 
"      tflag<-F", "    }", "  }", "  min1<-as.numeric(tclvalue(min1))", 
"  max1<-as.numeric(tclvalue(max1))", "  if(is.na(min1)) {", 
"    winDialog(type=c(\"ok\"),\"ERROR: The left axis minimum stage must be an numeric value.\")", 
"    tflag<-F", "  }", "  if(is.na(max1)) {", "    winDialog(type=c(\"ok\"),\"ERROR: The left axis maximum stage must be an numeric value.\")", 
"    tflag<-F", "  }", "  if(!is.na(min1)&!is.na(max1)) {", "    if(min1>max1) {", 
"      winDialog(type=c(\"ok\"),\"ERROR: The left axis minimum stage is\\ngreater than the left axis maximum stage.\") ", 
"      tflag<-F", "    }", "  }", "  ", "  if(tclvalue(pright)==1) {", 
"    min2<-as.numeric(tclvalue(min2))", "    max2<-as.numeric(tclvalue(max2))  ", 
"    if(is.na(min2)) {", "      winDialog(type=c(\"ok\"),\"ERROR: The right axis minimum stage must be an numeric value.\")", 
"      tflag<-F", "    }", "    if(is.na(max2)) {", "      winDialog(type=c(\"ok\"),\"ERROR: The right axis maximum stage must be an numeric value.\") ", 
"      tflag<-F", "    } ", "    if(!is.na(min2)&!is.na(max2)) {", 
"      if(min2>max2) {", "        winDialog(type=c(\"ok\"),\"ERROR: The right axis minimum stage is\\ngreater than the right axis maximum stage.\") ", 
"        tflag<-F", "      }", "    }", "  }", "  ", "  tflag  ", 
"}"))
vcov.nls <-
structure(function (object) 
{
    sm <- summary(object)
    sm$cov.unscaled * sm$sigma^2
}, source = c("function(object)", "{", "\tsm <- summary(object)", 
"\tsm$cov.unscaled * sm$sigma^2", "}"))
weightedmean.se <-
structure(function (fit, x, x0, y) 
{
    n <- length(x)
    xbar <- mean(x)
    xssq <- sum((x - xbar)^2)
    s <- summary(fit)$sigma
    term <- 1/n + ((x0 - xbar)^2)/xssq
    halfwidth <- s * sqrt(term)
    sum(y * halfwidth)/sum(y)
}, source = c("function(fit, x, x0, y) {", "# Take the weighted average of a standard error for yhat", 
"# x are the x values in the original regression", "# x0 are the x values where the regression is applied", 
"# The weights, y, should be the predicted values from the regression at x0", 
"        n <- length(x)", "        xbar <- mean(x)", "        xssq <- sum((x - xbar)^2.)", 
"        s <- summary(fit)$sigma", "        term <- 1./n + ((x0 - xbar)^2.)/xssq", 
"        halfwidth <- s * sqrt(term)", "        sum(y*halfwidth)/sum(y)", 
"}"))
weighthalfwidth <-
structure(function (fit, x, x0, y, intercept = TRUE) 
{
    if (data.class(x) != data.class(x0)) 
        error("x and x0 not of same data class")
    if (is.vector(x)) {
        n <- length(x)
        n0 <- length(x0)
    }
    else if (is.matrix(x)) {
        dim1 <- dim(x)
        dim0 <- dim(x0)
        if (dim1[2] != dim0[2]) 
            stop("Number of columns of x and x0 must be the same")
        n <- dim1[1]
        n0 <- dim0[1]
    }
    else stop("x must be a vector or matrix")
    if (intercept) {
        x1 <- cbind(rep(1, n), x)
        x01 <- cbind(rep(1, n0), x0)
    }
    else {
        x1 <- x
        x01 <- x0
    }
    midterm <- solve(t(x1) %*% x1)
    term <- apply(x01, 1, function(x) {
        xmat <- matrix(x)
        t(x) %*% midterm %*% x
    })
    s <- summary(fit)$sigma
    halfwidth <- s * sqrt(term)
    sum(y * halfwidth)/sum(y)
}, source = c("function(fit, x, x0, y, intercept=TRUE) {", "# Generalized to work for multiple linear regression rating equations", 
"# Take the weighted average of a standard error for yhat", "# x is the x values in the original regression", 
"# x0 are the x values where the regression is applied", "# In multiple regession x and x0 are matrixes with p columns", 
"# If there is an intercept an extra column of 1 is automatically added", 
"# Arguments x and x0 must be transformed by user if the model requires them", 
"# The weights, y, will normally be the discharge in raw discharge units", 
"        if (data.class(x) != data.class(x0))", "           error(\"x and x0 not of same data class\")", 
"        if (is.vector(x)) {", "           n <- length(x)", "           n0 <- length(x0)", 
"        }", "        else if (is.matrix(x)) {", "           dim1 <- dim(x)", 
"           dim0 <- dim(x0)", "           if (dim1[2] != dim0[2])", 
"              stop(\"Number of columns of x and x0 must be the same\")", 
"           n <- dim1[1]", "           n0 <- dim0[1]", "        }", 
"        else", "           stop(\"x must be a vector or matrix\")       ", 
"        if (intercept) {", "           x1 <- cbind(rep(1,n),x)", 
"           x01 <- cbind(rep(1,n0),x0)", "        }", "        else {", 
"           x1 <- x", "           x01 <- x0", "        }", "        midterm <- solve(t(x1)%*%x1)", 
"        term <- apply(x01, 1, function(x) {", "           xmat <- matrix(x)", 
"           t(x) %*% midterm %*% x", "        })", "        s <- summary(fit)$sigma", 
"        halfwidth <- s * sqrt(term)", "        sum(y*halfwidth)/sum(y)", 
"}"))
weircalc <-
structure(function (s) 
{
    a <- 4.33
    b <- 2.5
    q <- numeric(length(s))
    s1 <- s[s <= 2]
    q[s <= 2] <- a * s1^b
    s2 <- s[s > 2]
    rd <- s2 - 2
    q[s > 2] <- (1 + 0.2489 * (rd/(s2 + 1))^2) * (61.656 - 0.668 * 
        s2) * rd^1.47 + a * (s2^b - rd^b)
    q
}, source = c("function(s)", "{", "\ta <- 4.33", "\tb <- 2.5", 
"\tq <- numeric(length(s))", "\ts1 <- s[s <= 2.]", "\tq[s <= 2.] <- a * s1^b", 
"\ts2 <- s[s > 2.]", "\trd <- s2 - 2.", "\tq[s > 2.] <- (1. + 0.2489 * (rd/(s2 + 1.))^2.) * (61.656 - 0.668 * s2) * rd^1.47 + a * (s2^b - rd^b)", 
"\tq", "}"))
when.ge <-
structure(function (tts, lim = 0) 
{
    if (tts$xlog) {
        ssc <- tts$coef[1] + tts$coef[2] * log(tts$turb)
    }
    else {
        ssc <- tts$coef[1] + tts$coef[2] * tts$turb
    }
    range(tts$chr[ssc >= lim])
}, source = c("function(tts, lim = 0)", "{", "\t# For a tts object, gives the date range for which the", 
"\t# estimated ssc >= lim", "\tif(tts$xlog) {", "\t\tssc <- tts$coef[1] + tts$coef[2] * log(tts$turb)", 
"\t}", "\telse {", "\t\tssc <- tts$coef[1] + tts$coef[2] * tts$turb", 
"\t}", "\trange(tts$chr[ssc >= lim])", "}"))
which.na <-
structure(function (x) 
{
    seq(along = x)[is.na(x)]
}, source = c("function (x) ", "{", "# Returns the indexes of missing values in x", 
"seq(along=x)[is.na(x)]", "}"))
write.flo <-
structure(function (data, filename, ttshome = "C:/Users/Jack/My Documents/TTS/data") 
{
    stn <- substr(filename, 1, 3)
    filepath <- paste(ttshome, stn, filename, sep = "/")
    outdata <- data
    outdata$year <- as.numeric(as.character(years(data$chr)))
    outdata$mo <- zfill(as.numeric(months(data$chr)), 2)
    outdata$dy <- zfill(as.numeric(days(data$chr)), 2)
    outdata$mt <- zfill(msm2mt(round(1440 * (as.numeric(data$chr)%%1))), 
        4)
    outdata <- outdata[, c(12:15, 2:11)]
    write.table(outdata, filepath, quote = FALSE, row.names = F, 
        col.names = F, sep = ",")
    data
}, source = c("function(data, filename,ttshome=\"C:/Users/Jack/My Documents/TTS/data\") {", 
"   # Given a flo file object, write out the text file", "      stn <- substr(filename,1,3)", 
"      filepath <- paste(ttshome,stn,filename,sep=\"/\")", "      outdata <- data", 
"      outdata$year <- as.numeric(as.character(years(data$chr)))", 
"      outdata$mo <- zfill(as.numeric(months(data$chr)),2)", 
"      outdata$dy <- zfill(as.numeric(days(data$chr)),2)", "      outdata$mt <- zfill(msm2mt(round(1440*(as.numeric(data$chr)%%1))),4)", 
"      outdata <- outdata[,c(12:15,2:11)]", "      write.table(outdata,filepath,quote=FALSE,row.names=F,col.names=F,sep=\",\")", 
"   data", "}"))
write.sed <-
function (file, stn, hy, path = paste("H:/DataManagement/CASPARCREEK/Sediment/SuspendedSediment/Data/ProcessedData/WriteSed", stn, sep = "/")) 
{
# Writes a neatly formatted text file from the annual sed object"                                                                             
# Inserts rawcodes, stage, and stage code from .flo object"                                                                                   
# Output the year with 4 digits instead of the default of 2"
# Modified 14DEC2018: path changed to HYDRO file system, no longer to website_image 
    options(chron.year.abb = FALSE)
    on.exit(options(chron.year.abb = TRUE))
    hy <- zfill(hy, 2)
    seddata <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    flodata <- eval(as.name(paste(stn, hy, ".flo", sep = "")))
    row.names(flodata) <- format(flodata$chr)
    date <- format(dates(seddata$chr))
    time <- format.times(seddata$chr - trunc(seddata$chr))
    time <- substring(time, 1, 5)
    dump <- zfill(seddata$dump, 2)
    bot <- zfill(seddata$bottle, 2)
    key <- format(seddata$chr)
    rawcode <- flodata[key, "codes"]
    stg <- format(flodata[key, "stg"], nsmall = 3, width = 6)
    stgcode <- flodata[key, "stgcode"]
    q <- q0 <- seddata$q
    q[is.na(q0)] <- "        NA"
    q[!is.na(q0)] <- scientific(q0[!is.na(q0)], 3, 2)
    turb <- t0 <- seddata$turb
    turb[is.na(t0)] <- "  NA"
    turb[!is.na(t0)] <- zfill(t0[!is.na(t0)], 4)
    turbcode <- seddata$turbcode
    ssc <- scientific(seddata$ssc, 3, 2)
    labcodes <- paste(seddata$labcode1, seddata$labcode2, sep = "")
    outdata <- data.frame(date, time, dump, bot, rawcode, stg, 
        stgcode, q, turb, turbcode, ssc, labcodes)
    col.names <- c("DATE", "TIME", "DUMP", "BOT", "RAWCODE", 
        "STG", "STGCODE", "Q", "TURB", "TURBCODE", "SSED", "TRK/LABCODE")
    names(outdata) <- col.names
    outfile <- paste(path, file, sep = "/")
    if (!file.exists(path)) {
        dir.create(path)
        if (!file.exists(path)) 
            dir.create(path, recursive = T)
        if (!file.exists(path)) 
            stop(paste("Unable to create", path))
        else cat("Created folder", path, "\n")
    }
    else if (file.exists(outfile)) 
        cat("Rewriting file", outfile, "\n")
    else cat("Writing sample data to", outfile, "\n")
    write.table(outdata, outfile, col.names = T, row.names = F, 
        quote = FALSE)
}
write.ssc <-
function (file, stn, hy, path = paste("H:/DataManagement/CASPARCREEK/Sediment/SuspendedSediment/Data/ProcessedData/WriteSSC", stn, sep = "/")) 
{
# Modified 14DEC2018: path changed to HYDRO file system, no longer to website_image
# Modified 14Jan2012: Now handles stn.....ssc"                                                
# Modified 08Jun2011: Only retains data within the year specified;"                           
#    Prints warnings or errors for some common issues"                                        
# Assembles and writes a file of all objects whose names match one"                           
# of the templates stn...ssc or stn....ssc ( . means any character)"                          
# Hopefully these are the 10-minute SSC files created by tts.ssc"                             
# Inserts discharge, turb, and turbcode from the flo data frame too"                          
# file = output file name"                                                                    
# stn = 3-character station name"                                                             
# hy = 2-digit water year (needed to find flo data frame)"                                    
# path = system location where output file will be written " 
    objects2 <- objects(pat = paste(stn, "...ssc", sep = ""), 
        pos = 1)
    objects3 <- objects(pat = paste(stn, "....ssc", sep = ""), 
        pos = 1)
    objects4 <- objects(pat = paste(stn, ".....ssc", sep = ""), 
        pos = 1)
    objects <- c(objects2, objects3, objects4)
    if (length(objects) == 0) 
        stop("No objects found matching templates 'stn...ssc' or 'stn....ssc'")
    null <- numeric(0)
    data <- data.frame(chr = null, ssc = null, meth = null)
    for (obj in objects) {
        newdata <- eval(as.name(obj))
        if (data.class(newdata) != "data.frame") 
            stop(paste("Object", obj, "is not a data frame"))
        if (dim(newdata)[2] != dim(data)[2]) 
            stop(paste("Object", obj, "has incorrect number of columns"))
        data <- rbind(data, eval(newdata))
    }
    data$chr <- as.chron(data$chr)
    data <- data[order(data$chr), ]
    data$ssc <- scientific(data$ssc, 3, 2)
    data$date <- format(dates(data$chr))
    data$time <- format.times(data$chr - trunc(data$chr))
    data$time <- substring(data$time, 1, 5)
    floname <- paste(stn, zfill(hy, 2), ".flo", sep = "")
    flodat <- eval(as.name(floname))
    row.names(flodat) <- format(flodat$chr)
    data <- data[data$chr >= min(flodat$chr) & data$chr <= max(flodat$chr), 
        ]
    if (length(data$chr) < length(flodat$chr)) 
        warning(paste("Objects do not cover full period represented by", 
            floname))
    if (any(duplicated(data$chr))) 
        warning("Redundant or overlapping objects found")
    data$q <- flodat[format(data$chr), "q"]
    data$q <- scientific(data$q, 3, 2)
    data$turb <- flodat[format(data$chr), "turb"]
    data$turbcode <- as.character(flodat[format(data$chr), "turbcode"])
    data$turbcode <- padstring(data$turbcode, 3, right = F)
    output <- data[, c("date", "time", "q", "turb", "turbcode", 
        "ssc", "meth")]
    col.names <- c("DATE", "TIME", "FLOW", "TURB", "TURBQUAL", 
        "SSC", "METHOD")
    names(output) <- padstring(col.names, c(8, 5, 9, 4, 9, 3, 
        6), right = T)
    outfile <- paste(path, file, sep = "/")
    options(chron.year.abb = FALSE)
    on.exit(options(chron.year.abb = TRUE))
    if (!file.exists(path)) {
        dir.create(path)
        if (!file.exists(path)) 
            dir.create(path, recursive = T)
        if (!file.exists(path)) 
            stop(paste("Unable to create", path))
        else cat("Created folder", path, "\n")
    }
    else if (file.exists(outfile)) 
        cat("Rewriting file", outfile, "\n")
    else cat("Writing estimated SSC data to", outfile, "\n")
    write.table(output, outfile, col.names = T, row.names = F, 
        quote = FALSE)
    objects
}
writecmd <-
structure(function (file = "", funcname, arglist, result = "", 
    cmd = "") 
{
    if (cmd == "") {
        tmp <- call(funcname, arglist)[2]
        tmp1 <- substring(tmp, 6, nchar(tmp) - 1)
        tmp2 <- gsub("[ ]", "", tmp1)
        cmd <- paste(funcname, "(", tmp2, ")", sep = "")
        if (result != "") 
            cmd <- paste(result, " <- ", cmd, sep = "")
    }
    if (file != "") {
        cat("Command(s) submitted AND written to", file, ":\n")
        write(cmd, file = "")
    }
    else cat("Command(s) submitted to R:\n")
    write(cmd, file = file, append = T)
}, source = c("function(file=\"\",funcname,arglist,result=\"\",cmd=\"\") {", 
"  # Outputs a command given a function name, argument list and object to receive result", 
"  # If cmd is supplied as argument then just use that", "  if (cmd==\"\") {", 
"    tmp <- call(funcname,arglist)[2]", "    tmp1 <- substring(tmp, 6, nchar(tmp)-1)  # drops \"list(\" from the front and \")\" from the end", 
"    tmp2 <- gsub(\"[ ]\",\"\",tmp1)  # removes spaces", "    cmd <- paste(funcname,\"(\",tmp2,\")\",sep=\"\")", 
"    if (result !=\"\")", "      cmd <- paste(result,\" <- \",cmd,sep=\"\")", 
"  }", "  if (file !=\"\") {", "    cat(\"Command(s) submitted AND written to\",file,\":\\n\")", 
"    write(cmd,file=\"\")", "  }", "  else", "    cat(\"Command(s) submitted to R:\\n\")", 
"  write(cmd,file=file,append=T)", "}"))
ymd2date <-
structure(function (date, out = "m/d/y", origin = c(1, 1, 1970)) 
{
    date <- zfill(date, 6)
    chron(date, format = "ymd", out = out, origin = origin)
}, source = c("function(date, out = \"m/d/y\", origin = c(1., 1., 1970.))", 
"{", "\t# Converts numeric or character date (yymmdd) to \"dates\" object", 
"\tdate <- zfill(date, 6.)", "\tchron(date, format = \"ymd\", out = out, origin = origin)", 
"}"))
yy2year <-
structure(function (x) 
ifelse(x > 60, x + 1900, x + 2000), source = "function(x) ifelse(x>60,x+1900,x+2000)")
zfill <-
structure(function (x, n) 
{
    nc <- nchar(x)
    zeros <- paste(rep(0, n), collapse = "")
    paste(substring(zeros, nc + 1, n), substring(x, 1, nc), sep = "")
}, source = c("function (x, n) ", "{", "# Pads a whole number x (0,1,2,...) with leading zeros to give a ", 
"# character string with n characters", "    nc <- nchar(x)", 
"    zeros <- paste(rep(0, n), collapse = \"\")", "    paste(substring(zeros, nc + 1, n), substring(x, 1, ", 
"        nc), sep = \"\")", "}"))
