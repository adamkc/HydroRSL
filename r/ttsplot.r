ttsplot <-
function (stn, hy, ..., stagetics = seq(0, 4, 0.1), adj = T, 
    number = T, units = "cfs", split = 0.35, grid1, grid2) 
{
    arglist <- as.list(match.call()[-1]) #collect Args
    #Collect ... args (i.e. turbsrc objects):
    predobj <- as.list(match.call(expand.dots = F)$...) 
print(units)
    msc.n <- 0
    tts.n <- 0
    n <- 0
    tts <- NULL
    for (name in predobj) {
        n <- n + 1
        obj <- eval(name)
        if (data.class(obj) == "list" && "meth" %in% names(obj)) {
            if (obj$meth == 1 && obj$type != "pairs") {
                tts.n <- tts.n + 1
                if (tts.n > 2) 
                  stop("No more than 2 turbsrc objects permitted")
                objname <- paste("tts", tts.n, sep = "")
            }
            else if (obj$meth %in% 2:3 || obj$type == "pairs") {
                msc.n <- msc.n + 1
                if (msc.n > 3) 
                  stop("No more than 3 flowsrc and lineartime objects allowed")
                objname <- paste("msc", msc.n, sep = "")
            }
            else stop(paste("Invalid meth component for object", 
                name))
            names(arglist)[n + 2] <- objname
        }
        else stop(paste("Inappropriate prediction object", deparse(substitute(name))))
    }
    #print(arglist)
    do.call("oldttsplot", arglist)
}
