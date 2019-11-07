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
