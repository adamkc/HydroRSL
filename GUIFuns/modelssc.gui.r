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
