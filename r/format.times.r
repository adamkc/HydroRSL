format.times <-
function (x, format. = "h:m:s", simplify = FALSE, ...) 
{
    if (!as.logical(length(x))) 
        return("")
    if (all(is.na(x))) 
        return(rep("NA", length = length(x)))
    if (!is.numeric(x)) 
        stop(paste(deparse(substitute(x)), "must be numeric"))
    att <- attributes(x)
    if (inherits(x, "times")) {
        if (missing(format.)) 
            format. <- switch(mode(att$format), character = , 
                list = rev(att$format)[[1]], name = , `function` = att$format, 
                `NULL` = format., stop("invalid output times format"))
        class(x) <- NULL
    }
    if (!is.character(format.)) {
        FUN <- switch(mode(format.), `function` = format., name = eval(format.), 
            stop(paste("unrecognized time format", deparse(substitute(format.)))))
        return(FUN(unclass(x), ...))
    }
    else format. <- rev(format.)[1]
    nas <- is.na(x)
    days <- abs(trunc(x))
    att$class <- att$format <- att$origin <- NULL
    if (any(days[!nas] > 0)) {
        attributes(x) <- att
        return(format(x))
    }
    sec <- round(24 * 3600 * abs(x))
    hh <- sec%/%3600
    mm <- (sec - hh * 3600)%/%60
    ss <- trunc(sec - hh * 3600 - 60 * mm)
    out <- list(h = substring(paste("0", hh, sep = ""), nchar(paste(hh))), 
        m = substring(paste("0", mm, sep = ""), nchar(paste(mm))), 
        s = substring(paste("0", ss, sep = ""), nchar(paste(ss))))
    style <- parse.format(format.)
    o <- style$periods
    if (!simplify) 
        out <- paste(out[[o[1]]], out[[o[2]]], out[[o[3]]], sep = style$sep)
    else {
        if (simplify == 1) {
            o <- o[o != "s"]
            out <- paste(out[[o[1]]], out[[o[2]]], sep = style$sep)
        }
        else out <- out$h
    }
    if (any(x[!nas] < 0)) 
        out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
    out[nas] <- NA
    out[x == Inf] <- "Inf"
    out[x == -Inf] <- "-Inf"
    attributes(out) <- att
    out
}
