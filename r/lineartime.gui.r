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
