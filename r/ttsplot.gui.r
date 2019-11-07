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
