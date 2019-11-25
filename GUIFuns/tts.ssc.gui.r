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
