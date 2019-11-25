addPlotMenu <-
function () 
{
    winMenuAdd("TTS Plots")
    winMenuAddItem("TTS Plots", "Set Logger Home Dir (for raw data plots)", 
        "setLoggerHome()")
    winMenuAddItem("TTS Plots", "Set TTS Home Dir (for appended data plots)", 
        "setTTSHome()")
    winMenuAddItem("TTS Plots", "Plot Raw Data", "rawplot.gui()")
    winMenuAddItem("TTS Plots", "Plot Appended Data", "floplot.gui()")
}
addttsMenu <-
function () 
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
}
aic.lm <-
function (lmfit) 
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
}
aic.loess <-
function (model) 
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
}
albcalc <-
function (stg) 
{
    ifelse(stg >= 0.4, 6.337 * (stg - 0.4)^2.367, 0)
}
allmatches <-
function (x, y) 
{
    (1:length(y))[!is.na(match(y, x))]
}
arfcalc <-
function (stg) 
{
    print("Warning from qcalc: ARF rating equation is invalid after HY99")
    a <- -0.2927
    b <- 1.4174
    c <- 81.5627
    c * stg^(b + a * log(stg))
}
avg.err <-
function (e) 
(exp(e) - exp(-e))/2
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
bands <-
function (fit, x, log = F, lty = 2, col = 1, type = "conf", conf = 0.95) 
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
}
BoxCox <-
function () 
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
}
build.power.table <-
function (power, n1, n2, prop.n2, alpha, theta1, theta2, disp1, 
    disp2, expand.args, one.sample, compute.what) 
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
}
campbell.date <-
function (year, day, time = 0) 
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
}
check.interstorm <-
function (stn, hy, interstorm, surrogate, checkflo = T, checksed = T) 
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
}
compmodel.gui <-
function () 
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
}
composite <-
function (intime, resid, outtime, opt, oldcomp) 
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
}
cor.matrix <-
function (interval, popq, xsample, xpop, def = 0.75) 
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
}
crbasic.date <-
function (date) 
{
    date.list <- sapply(as.character(date), strsplit, " ")
    date <- sapply(date.list, function(x) x[[1]])
    time <- sapply(date.list, function(x) x[[2]])
    chron(date, time, format = c("y-m-d", "h:m:s"), out.format = c("m/d/y", 
        "h:m:s"))
}
create.flo <-
function (data, filepath, extras, write = T) 
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
}
crit.loess <-
function (h, x, y, degree = 2) 
{
    n <- length(y)
    l <- loess(y ~ x, span = h, degree = degree)
    df <- l$trace.hat
    if (df >= n - 2) 
        Inf
    else log(sum((predict(l, x) - y)^2)/n) + (1 + df/n)/(1 - 
        df/n - 2/n)
}
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
dailystats <-
function (stn, hy) 
{
    data <- read.flo(stn, hy)
    day <- dates(data$chr)
    meandaily <- tapply(data$q, day, mean, na.rm = T)
    mindaily <- tapply(data$q, day, min, na.rm = T)
    maxdaily <- tapply(data$q, day, max, na.rm = T)
    date <- dates(as.numeric(names(meandaily)))
    data.frame(date = date, mean = as.vector(meandaily), min = as.vector(mindaily), 
        max = as.vector(maxdaily))
}
density.draw <-
function (panel) 
{
    plot(density(panel$x, bw = panel$h))
    panel
}
dis.adjust <-
function (stn, ssc) 
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
}
display <-
function (x) 
page(x, method = "print")
drop.parens <-
function (x) 
{
    x1 <- sub("[(]", "", x)
    sub("[)]", "", x1)
}
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
function (stn, hy, match = F, source = "filesystem", loc = paste(getTTSenv("TTSHOME"), 
    "/work/hy", hy, sep = "")) 
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
}
drop.storms.gui <-
function () 
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
}
duan.cbrt <-
function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    result <- yhat^3 + 3 * yhat * mean(res^2)
    list(naive = yhat^3, corrected = result)
}
duan.log <-
function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    naive <- exp(yhat)
    result <- naive * mean(exp(res))
    list(naive = naive, corrected = result)
}
duan.sqrt <-
function (fit, newdata) 
{
    yhat <- predict(fit, newdata = newdata)
    res <- resid(fit)
    result <- yhat^2 + 2 * yhat * mean(res) + mean(res^2)
    result[yhat < 0] <- 0
    list(naive = yhat^2, corrected = result)
}
electStnYear <-
function () 
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
}
endpoints <-
function (object, all = F) 
{
    ssc <- object$predssc
    c(start = ssc[1], end = last.val(ssc))
}
exceed <-
function (x, plevels = c(0.001, 0.01, 0.02, 0.05, 0.1), varname = "ssc", 
    ssclevels = c(25, 50, 100, 200, 500, 1000), interval = 10, 
    months, leap) 
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
}
ezchron <-
function (date, time) 
chron(date, paste(time, "00", sep = ":"))
ezchron2 <-
function (date.time) 
{
    require(chron)
    timelist <- strsplit(date.time, " ")
    date <- sapply(timelist, "[", 1)
    time <- sapply(timelist, "[", 2)
    time <- paste(time, "00", sep = ":")
    chron(date, time)
}
fac2num <-
function (x) 
as.numeric(as.character(x))
find.chardata <-
function (x) 
{
    lev <- levels(x)
    lev <- lev[lev != ""]
    oldopts <- options(warn = -1)
    lev.indices <- which.na(as.numeric(lev))
    options(warn = oldopts$warn)
    chardata <- lev[lev.indices]
    x.indices <- which(x %in% chardata)
    list(values = chardata, indexes = x.indices)
}
find.factors <-
function (data) 
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
}
finney <-
function (m, z) 
{
    N <- length(z)
    if (length(m) == 1) 
        m <- rep(m, N)
    result <- rep(1, N)
    goodones <- (abs(z) < 50) & (m > 0)
    if (sum(goodones) > 0) 
        result[goodones] <- finney2(m[goodones], z[goodones])
    result
}
finney2 <-
function (m, z) 
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
}
floplot.gui <-
function (path = getTTSenv("TTSHOME")) 
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
}
flowcomp <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = NULL, long = T, adj = T, 
    var = T, units = "cfs", comp = T, opt = c(0, 0), oldcomp = F) 
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
}
flowduan <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
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
}
flowloess <-
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
}
flowmvue <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
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
}
flowpairs <-
function (stn, hy, sdate1, stime1, edate1, etime1, interval = 10, 
    opt = 1, long = T, adj = T, units = "cfs") 
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
}
flowpower <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", comp = F, opt = c(0, 0), oldcomp = F) 
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
}
flowqmle <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
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
}
flowsqrt <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", comp = F, opt = c(0, 0), oldcomp = F) 
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
}
flowsrc <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, type = "logxy", bias = "mvue", units = "cfs", span = 1, 
    degree = 1, comp = F, opt = c(0, 0), oldcomp = F) 
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
}
flumecalc <-
function (stn, stg, hy = hy.default()) 
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
}
format.times <-
function (x, format. = "h:m:s", simplify = FALSE, ...) 
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
}
four <-
function () 
par(mfrow = c(2, 2))
ftrcalc <-
function (stg, hy = hy.default()) 
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
}
get.stagetics <-
function (q, stn, hy, nticks = 4) 
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
}
get.thresh <-
function (tstart, tmax, n, f = sqrt, finv = function(x) x^2) 
{
    dx <- (f(tmax) - f(tstart))/(n - 1)
    x <- seq(from = f(tstart), by = dx, length = n)
    round(finv(x))
}
getflows <-
function (hy, stations) 
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
}
getORs <-
function (stn, st, en, ttshome = getTTSenv("TTSHOME")) 
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
}
getpeaks <-
function (hy, stations, match = T) 
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
}
getpeakstage <-
function (hy, stations, match = T) 
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
}
getStormdates <-
function (stn, hy) 
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
}
getTTSenv <-
function (varname) 
{
    if (!file.exists("TTSenvironment.txt")) 
        return(data.frame(row.names = c("TTSHOME", "LOGGERHOME"), 
            value = c(NA, NA), stringsAsFactors = F))
    data <- read.table("TTSenvironment.txt", sep = "=", row.names = 1, 
        as.is = T, header = F, col.names = c("name", "value"))
    if (missing(varname)) 
        data
    else data[varname, "value"]
}
getturb <-
function (hy, stations, match = T, opt = 0) 
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
}
hy.default <-
function () 
{
    today <- date()
    mon <- substring(today, 5, 7)
    nc <- nchar(today)
    year <- as.numeric(substring(today, nc - 3, nc))
    ifelse(mon %in% c("Aug", "Sep", "Oct", "Nov", "Dec"), year + 
        1, year)
}
hydro.year <-
function (chr, start = "Aug") 
{
    yr <- fac2num(years(chr))
    ifelse(months(chr) < start, yr, yr + 1)
}
interp.chron <-
function (data, chr, var) 
{
    t1 <- round2tenmin(chr, "down")
    t2 <- round2tenmin(chr, "up")
    t <- sort(unique(c(t1, t2)))
    rownames <- format(chron(t))
    row.names(data) <- format(data$chr)
    matchdat <- data[rownames, ]
    approx(matchdat[, "chr"], matchdat[, var], chr, rule = 1)$y
}
last.val <-
function (x) 
x[length(x)]
lineartime <-
function (stn, hy, sdate, stime, edate, etime, interval = 10, 
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
}
lineartime.gui <-
function () 
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
}
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
listfuncs <-
function (env = .GlobalEnv, data.class = "function", ...) 
{
    objnames <- objects(env)
    dclass <- sapply(objnames, function(x) data.class(eval(as.name(x))))
    objnames[dclass == data.class]
}
localflowsrc <-
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
}
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
logline <-
function (x, y, ...) 
{
    fit <- lm(log(y) ~ log(x), na.action = na.omit)
    x0 <- range(x, na.rm = T)
    y0 <- exp(coef(fit)[1] + coef(fit)[2] * log(x0))
    lines(x0, y0, ...)
    fit
}
logxy.duan <-
function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
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
}
logxy.mvue <-
function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
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
}
logxy.qmle <-
function (xsam, ysam, xpop, qpop, var = T, interval = 10) 
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
}
make.chr <-
function (year, mo, dy, time) 
{
    chron(paste(mo, dy, year, sep = "/")) + mt2msm(time)/1440
}
make.chron <-
function (date, time, out = "day-mon-year", origin) 
{
    if (missing(origin)) 
        origin <- options()$chron.origin
    ymd2date(date, out = out, origin = origin) + mt2msm(time)/1440
}
merge.flo <-
function (stn, hy, all.lab = F, all.flo = F, na.rm = T) 
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
}
merge.flo.sand <-
function (stn, hy, all.lab = F, all.flo = F, na.rm = T) 
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
}
merge.flo.vr <-
function (stn, hy, all.lab = F) 
{
    lab <- eval(as.name(paste(stn, zfill(hy, 2), ".lab", sep = "")))
    flo <- eval(as.name(paste(stn, zfill(hy, 2), ".flo", sep = "")))
    lab$q <- approx(flo$chr, flo$q, lab$chr)$y
    lab
}
minortics <-
function (side) 
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
}
mismatches <-
function (stn, hy) 
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
}
model.objects <-
function () 
{
    objnames <- objects(envir = .GlobalEnv)
    objtypes <- sapply(objnames, function(x) data.class(eval(as.name(x))))
    lists <- objnames[objtypes == "list"]
    firstcomp <- sapply(lists, function(x) names(eval(as.name(x)))[1])
    lists[firstcomp == "yhat"]
}
modelssc.gui <-
function () 
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
}
msm2mt <-
function (msm) 
{
    hr <- msm%/%60
    mn <- msm%%60
    round(100 * hr + mn)
}
mt2msm <-
function (time) 
{
    if (data.class(time) == "character") 
        time <- as.numeric(time)
    min <- time%%100
    hour <- time%/%100
    60 * hour + min
}
mvue <-
function (mydata, sam = which(!is.na(mydata$ssc)), x = "turb", 
    interval = 10) 
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
}
my.textentry <-
function (panel, var, action = I, labels = NA, names = labels, 
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
}
mystamp <-
function (cex = 0.7, string = date()) 
{
    mtext(string, side = 1, line = -1, cex = cex, outer = T, 
        adj = 1)
}
normal.sample.size <-
function (mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, power = 0.8, 
    alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, one.sample = missing(sd2), 
    alternative = "two.sided", expand.args = T, exact.n = F, 
    recompute.power = F) 
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
}
nss <-
function (mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, power = 0.8, 
    alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, one.sample = missing(sd2), 
    alternative = "two.sided", expand.args = T, exact.n = F, 
    recompute.power = F) 
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
}
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
one <-
function () 
par(mfrow = c(1, 1))
optimize.loess <-
function (xvar, yvar, min, max, by = 0.05, degree = 1) 
{
    span <- seq(min, max, by)
    aic <- sapply(span, function(s, x, y, d) crit.loess(s, x, 
        y, d), x = xvar, y = yvar, d = degree)
    span[aic == min(aic)]
}
origin <-
function (x) 
attr(x, "origin")
padstring <-
function (string, len, char = " ", right = T, chop = T) 
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
}
pairedTTest <-
function () 
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
}
panelselectStnYear <-
function () 
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
}
parse.format <-
function (format, year.abb = getOption("chron.year.abb"), ...) 
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
}
parseDate <-
function (str) 
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
}
pdi.append <-
function (stn, hy, ttshome = ".", outpath = "K:/water/caspar/website_image/sediment/samples") 
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
}
pickone <-
function (values, whatitis = "value") 
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
}
plot.or <-
function (stn, hy, sdate, stime, edate, etime, dumps, xaxis = "time", 
    cor = F, reread = T, linear = F, span = 2/3, degree = 1, 
    choose = F, ...) 
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
}
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
plotf <-
function (panel) 
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
}
predict.loglog <-
function (x, y, xnew, ...) 
{
    fit <- lm(log(y) ~ log(x), na.action = na.omit, ...)
    pred <- predict(fit, newdata = data.frame(x = xnew))
    sigma <- summary(fit)$sigma
    if (is.na(sigma)) 
        exp(pred)
    else exp(0.5 * sigma^2 + pred)
}
predict.nls <-
function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
    interval = c("none", "confidence", "prediction"), level = 0.95, 
    ...) 
{
    if (missing(newdata)) 
        return(as.vector(fitted(object)))
    object$m$predict(newdata)
}
predict.simple <-
function (x, y, xnew) 
{
    fit <- lm(y ~ x)
    predict(fit, newdata = data.frame(x = xnew))
}
pwr.t2n.test <-
function (n1 = NULL, n2 = NULL, d = NULL, sig.level = 0.05, power = NULL, 
    alternative = c("two.sided", "less", "greater")) 
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
}
qcalc <-
function (stn, stg, hy) 
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
}
qmle <-
function (xsam, ysam, q, var = T, interval = 10) 
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
}
qsscplot <-
function (stn, hy, sdate, stime = 0, edate, etime = 2400, dumpstr, 
    bottlestr, exclude = TRUE, type = "logxy", col = T, textsize = 0.6, 
    span = 1, degree = 1, txt = "bottle", units = "cfs", ...) 
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
}
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
quecalc <-
function (stg) 
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
}
rawplot.gui <-
function (path = getTTSenv("LOGGERHOME")) 
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
}
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
function (file, path = getTTSenv("LOGGERHOME")) 
{
    data <- read.table(paste(path, file, sep = "\\"), sep = ",")[, 
        2:10]
    names(data) <- c("year", "day", "time", "dump", "bottle", 
        "code1", "code2", "stg", "turb")
    data$chr <- campbell.date(data$year, data$day, data$time)
    nc <- dim(data)[[2]]
    data[, c(nc, 5:(nc - 1))]
}
read.crbasic <-
function (pathname) 
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
}
read.excel <-
function (header = TRUE, ...) 
{
    read.table("clipboard", sep = "\t", header = header, ...)
}
read.flo <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
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
}
read.flo.vr <-
function (file, path = ".") 
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
}
read.flofile <-
function (flopath, extravars) 
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
}
read.flostages <-
function () 
{
}
read.lab <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
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
}
read.lab.vr <-
function (file, path = ".") 
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
}
read.mixed <-
function (pathname) 
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
}
read.or <-
function (stn, hy, choose.staff = F, ttshome = getTTSenv("TTSHOME")) 
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
}
read.raw <-
function (filepath) 
{
    filetype <- scan(filepath, what = "", n = 1)
    if (filetype %in% c("TOACI1", "TOA5")) 
        data <- read.crbasic(filepath)
    else data <- read.mixed(filepath)
}
read.sand <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
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
}
read.ssc <-
function (stn, hy, path = paste(getTTSenv("TTSHOME"), "tenminssc", 
    sep = "/")) 
{
    fname <- paste(stn, zfill(hy, 2), "ssc.txt", sep = "")
    pathname <- paste(path, fname, sep = "/")
    data <- read.table(pathname, header = T, as.is = T)
    data$chr <- chron(data$DATE, paste(data$TIME, "00", sep = ":"))
    data
}
read.stages <-
function (stn, hy, ttshome = getTTSenv("TTSHOME")) 
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
}
read.storms <-
function (stn, hy, alt = T) 
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
}
readAndMerge.gui <-
function () 
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
}
redraw <-
function (panel) 
{
    rp.tkrreplot(panel, tkrp)
    panel
}
regmat <-
function (formula, xpro = 0.78, ypro = 0.92, ...) 
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
}
removettsMenu <-
function () 
winMenuDel("TTS")
reselData.gui <-
function () 
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
}
reselData.simple <-
function (type) 
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
}
reselect <-
function (data, xvar, yvar, dataname, subset) 
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
}
reselect.simple <-
function (data, xvar, yvar, type = "Omit", dataname) 
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
}
rmse <-
function (x, y) 
sqrt(mean((y - x)^2))
round2tenmin <-
function (x, dir = "nearest") 
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
}
saveCommand <-
function (stn, hy2, funcname, arglist, result, savetofile, cmd = "") 
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
}
scatterPlot.gui <-
function () 
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
}
scientific <-
function (x, mandigits, expdigits) 
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
}
searchfuncs <-
function (string, names) 
{
    if (missing(names)) 
        names <- listfuncs()
    funcstrings <- sapply(names, function(x) paste(deparse(eval(as.name(x))), 
        collapse = " "))
    names[grep(string, funcstrings)]
}
select.file <-
function (pathname = getTTSenv("LOGGERHOME")) 
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
}
select.flofile <-
function (stn, allfiles) 
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
}
select.station <-
function (path) 
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
}
selStnYear <-
function () 
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
}
setLoggerHome <-
function (initial = getTTSenv("LOGGERHOME")) 
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
}
setTTSenv <-
function (name, value, env) 
{
    if (missing(env)) 
        env <- getTTSenv()
    env[name, "value"] <- value
    write.table(env, "TTSenvironment.txt", sep = "=", col.names = F, 
        quote = F)
}
setTTSHome <-
function (initial = getTTSenv("TTSHOME")) 
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
}
sev <-
function (ssc, dur, a, b, c) 
{
    result <- a + b * log(dur) + c * log(ssc)
    result[dur == 0] <- 0
    result
}
sev.mat <-
function (matrix, a, b, c) 
{
    sevmat <- matrix
    ssc <- as.numeric(dimnames(matrix)[[2]])
    for (i in 1:length(ssc)) {
        sevmat[, i] <- sev(ssc[i], matrix[, i], a, b, c)
    }
    sevmat
}
ssc.mycdf <-
function (x, title, months, log = "x", zero = 0.1, leap = F, 
    interval = 10, grid = "days", yvar = "SSC", ...) 
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
}
ssc.mycdfs <-
function (xlist, title, months, log = "x", zero = 0.1, leap = F, 
    interval = 10, grid = "days", yvar = "SSC", ...) 
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
}
staffnames <-
function (stn) 
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
}
statmode <-
function (x) 
{
    xtab <- table(x)
    xmax <- which(xtab == max(xtab))
    xmode <- names(xmax)
    if (is.numeric(x)) 
        as.numeric(xmode)
    else xmode
}
stgcalc <-
function (q0, lower, upper, stn, hy) 
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
}
subtime <-
function (frame, sdate, stime, edate, etime) 
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
}
subtime2 <-
function (frame, sdate, stime, edate, etime) 
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
}
sumdays <-
function (months, leap) 
{
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if (leap) 
        days[2] <- 29
    sum(days[months])
}
total <-
function (...) 
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
}
total.gui <-
function () 
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
}
tts.gui <-
function () 
{
    while (1) {
        choice <- tolower(readline("\nPlot raw or appended files? Enter 'r' or 'a': "))
        if (choice %in% c("r", "a")) 
            break
    }
    if (choice == "r") 
        rawplot.gui()
    else floplot.gui()
}
tts.rawplot <-
function (dump, tclvar, schron, echron, minstg) 
{
    attach(tclvar, warn = F)
    on.exit(detach(tclvar))
    msgflag <- 0
    if (is.null(dev.list())) 
        print("Error in tts.rawplot: line commented out because win.graph function issue")
        #win.graph(width = 10, height = 7, pointsize = 12)
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
tts.ssc <-
function (...) 
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
}
tts.ssc.gui <-
function () 
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
}
tts.stats <-
function (sam, pop, interval = 10, var = T, units = "cfs") 
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
}
tts5.sample <-
function (data, up, dn, stay = 2, revpct = c(10, 20), repwait = 18, 
    minstg = 0, turblim = 2000, limskip = 2, initcode = 3, revval = 5, 
    startwait = 72) 
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
}
ttsplot <-
function (stn, hy, ..., stagetics = seq(0, 4, 0.1), adj = T, 
    number = T, units = "cfs", split = 0.35, grid1, grid2) 
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
}
ttsplot.gui <-
function () 
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
}
turbcomp <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = NULL, long = T, adj = T, 
    var = T, units = "cfs", comp = T, opt = c(0, 0), oldcomp = F) 
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
}
turbduan <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
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
}
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
turbmvue <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
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
}
turbpairs <-
function (stn, hy, sdate1, stime1, edate1, etime1, interval = 10, 
    opt = 1, long = T, adj = T, units = "cfs") 
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
}
turbpower <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", comp = F, opt = c(0, 0), oldcomp = F) 
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
}
turbqmle <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs") 
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
}
turbsqrt <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, units = "cfs", comp = F, opt = c(0, 0), oldcomp = F) 
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
}
turbsrc <-
function (stn, hy, sdate1, stime1, edate1, etime1, sdate2 = sdate1, 
    stime2 = stime1, edate2 = edate1, etime2 = etime1, dumps, 
    bottles, interval = 10, exclude = TRUE, long = T, adj = T, 
    var = T, type = "linear", bias = "mvue", units = "cfs", span = 1, 
    degree = 1, comp = F, oldcomp = F, opt = c(0, 0)) 
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
}
turbsscplot <-
function (stn, hy, sdate, stime = 0, edate, etime = 2400, dumpstr, 
    bottlestr, exclude = TRUE, type = "linear", col = T, textsize = 0.6, 
    span = 1, degree = 1, txt = "bottle", ...) 
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
}
two <-
function () 
par(mfrow = c(1, 2))
ujccalc <-
function (stg) 
{
    a <- 13.084
    b <- 3.9429
    a * stg^b
}
usual.mle <-
function (xsam, ysam, qpop, var = T, interval = 10) 
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
}
var.check <-
function (tclvar) 
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
}
vcov.nls <-
function (object) 
{
    sm <- summary(object)
    sm$cov.unscaled * sm$sigma^2
}
weightedmean.se <-
function (fit, x, x0, y) 
{
    n <- length(x)
    xbar <- mean(x)
    xssq <- sum((x - xbar)^2)
    s <- summary(fit)$sigma
    term <- 1/n + ((x0 - xbar)^2)/xssq
    halfwidth <- s * sqrt(term)
    sum(y * halfwidth)/sum(y)
}
weighthalfwidth <-
function (fit, x, x0, y, intercept = TRUE) 
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
}
weircalc <-
function (s) 
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
}
when.ge <-
function (tts, lim = 0) 
{
    if (tts$xlog) {
        ssc <- tts$coef[1] + tts$coef[2] * log(tts$turb)
    }
    else {
        ssc <- tts$coef[1] + tts$coef[2] * tts$turb
    }
    range(tts$chr[ssc >= lim])
}
which.na <-
function (x) 
{
    seq(along = x)[is.na(x)]
}
write.flo <-
function (data, filename, ttshome = "C:/Users/Jack/My Documents/TTS/data") 
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
}
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
function (file = "", funcname, arglist, result = "", cmd = "") 
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
}
ymd2date <-
function (date, out = "m/d/y", origin = c(1, 1, 1970)) 
{
    date <- zfill(date, 6)
    chron(date, format = "ymd", out = out, origin = origin)
}
yy2year <-
function (x) 
ifelse(x > 60, x + 1900, x + 2000)
zfill <-
function (x, n) 
{
    nc <- nchar(x)
    zeros <- paste(rep(0, n), collapse = "")
    paste(substring(zeros, nc + 1, n), substring(x, 1, nc), sep = "")
}
