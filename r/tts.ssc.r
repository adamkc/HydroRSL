tts.ssc <-
function (...) 
{
    arglist <- as.list(match.call()[-1])
    chr <- chron(NULL)
    ssc <- numeric(0)
    meth <- numeric(0)
    yhat <- 0
    for (name in arglist) {
        obj <- eval(name)
        ssc <- append(ssc, obj$predssc)
        chr <- append(chr, obj$chr)
        meth <- append(meth, rep(obj$meth, length(obj$chr)))
        yhat <- yhat + obj$yhat
    }
    ssc[ssc < 0] <- 0
    cat("yhat:", round(yhat), "\n")
    dat <- data.frame(chr, ssc, meth)
    dat <- dat[order(chr), ]
    row.names(dat) <- 1:dim(dat)[1]
    dat
}
