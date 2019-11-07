setTTSHome <-
function (initial = getTTSenv("TTSHOME")) 
{
    library(tcltk)
    title <- "Select the TTS Home Directory (i.e. where the station folders reside)"
    tcldir <- tkchooseDirectory(title = title, initialdir = initial, 
        mustexist = T)
    newhome <- tclvalue(tcldir)
    if (newhome == "" || (!is.na(initial) && newhome == initial)) {
        cat("TTS Home not changed\n")
        return(invisible())
    }
    setTTSenv("TTSHOME", newhome)
    cat("TTS Home set to", newhome, "\n")
}
