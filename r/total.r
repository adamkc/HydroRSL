total <-
function (...) 
{
    arglist <- as.list(match.call()[-1])
    names <- unlist(arglist)
    vsum <- 0
    ysum <- 0
    mtx <- NULL
    type <- NULL
    meth <- NULL
    start <- NULL
    end <- NULL
    times <- NULL
    for (objname in names) {
        obj <- eval(as.name(objname))
        start <- c(start, obj$chr[1])
    }
    names <- names[order(start)]
    start <- sort(start)
    for (objname in names) {
        obj <- eval(as.name(objname))
        vsum <- vsum + (obj$cv/100 * obj$yhat)^2
        ysum <- ysum + obj$yhat
        mtx <- rbind(mtx, unlist(obj[c("yhat", "n", "r2", "s", 
            "cv")]))
        if (!is.null(obj$bias)) {
            type.bias <- paste(obj$type, obj$bias, sep = ":")
            type <- c(type, type.bias)
        }
        else type <- c(type, obj$type)
        meth <- c(meth, obj$meth)
        end <- c(end, last.val(obj$chr))
        times <- c(times, obj$chr)
    }
    dimnames(mtx)[[1]] <- names
    df <- data.frame(round(mtx, 3))
    df$type <- type
    surrogates <- c("turb", "flow", "time")
    df$surr <- surrogates[meth]
    start <- format(chron(start))
    end <- format(chron(end))
    df$start <- substring(start, 2, 15)
    df$end <- substring(end, 2, 15)
    if (length(table(diff(round(1440 * times)))) > 1) 
        cat("Warning: Objects are non-sequential: check for gaps and overlaps\n")
    df <- df[1:dim(df)[1], c("start", "end", "type", "surr", 
        "yhat", "n", "r2", "s", "cv")]
    print(df)
    cat("Total load =", round(ysum), "kg")
    if (!is.na(vsum)) 
        cat(",  CV =", round((100 * sqrt(vsum))/ysum, 2), "%\n")
    else cat("\n")
    invisible(df)
}
