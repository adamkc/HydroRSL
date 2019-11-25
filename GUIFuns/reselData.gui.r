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
