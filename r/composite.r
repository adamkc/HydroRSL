composite <-
function (intime, resid, outtime, opt, oldcomp) 
{
    if (length(intime) != length(resid)) 
        stop("Number of samples does not match number of sample times")
    if (oldcomp && (opt[1] == 2 || opt[2] == 2)) 
        warning("With oldcomp=T, you should specify the original opt values (0 or 1 only).")
    if (oldcomp == F && opt[1] == 2 && min(intime) >= outtime[1]) {
        opt[1] <- 1
        warning("No external samples were specified prior to estimation period.")
    }
    if (oldcomp == F && opt[2] == 2 && max(intime) <= last.val(outtime)) {
        opt[2] <- 1
        warning("No external samples were specified after estimation period.")
    }
    if (oldcomp || opt[1] == 2) 
        begin <- intime[1]
    else begin <- min(intime[intime >= outtime[1]])
    if (oldcomp || opt[2] == 2) 
        end <- last.val(intime)
    else end <- max(intime[intime <= last.val(outtime)])
    xin <- intime[intime >= begin & intime <= end]
    yin <- resid[intime >= begin & intime <= end]
    if (outtime[1] < begin) {
        xin <- c(outtime[1], xin)
        ystart <- ifelse(opt[1] == 0, 0, yin[1])
        yin <- c(ystart, yin)
    }
    if (last.val(outtime) > end) {
        xin <- c(xin, last.val(outtime))
        yend <- ifelse(opt[2] == 0, 0, last.val(yin))
        yin <- c(yin, yend)
    }
    approx(xin, yin, outtime)$y
}
