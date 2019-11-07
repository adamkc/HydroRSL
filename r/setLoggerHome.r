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
