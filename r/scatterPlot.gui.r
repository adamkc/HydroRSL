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
