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
