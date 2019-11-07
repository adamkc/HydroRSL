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
