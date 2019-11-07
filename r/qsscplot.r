qsscplot <-
function (stn, hy, sdate, stime = 0, edate, etime = 2400, dumpstr, 
    bottlestr, exclude = TRUE, type = "logxy", col = T, textsize = 0.6, 
    span = 1, degree = 1, txt = "bottle", units = "cfs", ...) 
{
    nhy <- as.numeric(hy)
    prevyr <- ifelse(nhy == 0, "99", zfill(nhy - 1, 2))
    hy <- zfill(hy, 2)
    df <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    station.name <- attr(df, "stn")
    stn <- toupper(stn)
    if (missing(dumpstr) && missing(bottlestr)) {
        if (missing(sdate)) 
            sdate <- paste(prevyr, "0801", sep = "")
        if (missing(edate)) 
            edate <- paste(hy, "0731", sep = "")
        df <- subtime(df, sdate, stime, edate, etime)
        subtitle <- paste(zfill(sdate, 6), ":", zfill(stime, 
            4), " - ", zfill(edate, 6), ":", zfill(etime, 4), 
            sep = "")
    }
    else if (missing(bottlestr)) {
        if (is.numeric(dumpstr)) {
            dumps <- dumpstr
            dumpstr <- deparse(substitute(dumpstr))
        }
        else {
            dumps <- eval(parse(text = dumpstr))
        }
        df <- df[df$dump %in% dumps, ]
        subtitle <- paste("dumps =", dumpstr)
    }
    else {
        if (is.numeric(dumpstr)) {
            dumps <- dumpstr
            dumpstr <- deparse(substitute(dumpstr))
            bottles <- bottlestr
            bottlestr <- deparse(substitute(bottlestr))
        }
        else {
            dumps <- eval(parse(text = dumpstr))
            bottles <- eval(parse(text = bottlestr))
        }
        df <- df[allmatches(paste(dumps, bottles), paste(df$dump, 
            df$bottle)), ]
        subtitle <- paste("dumps =", dumpstr, ", bottles =", 
            bottlestr)
    }
    if (exclude & !is.null(df$exclude)) 
        df <- df[!df$exclude, ]
    if (units == "cfs") 
        qfactor <- 35.31467
    else if (units == "cumecs") 
        qfactor <- 1
    else stop("flow units must be cfs or cumecs")
    df$q <- df$q/qfactor
    if (all(is.na(df$ssc) | is.na(df$q))) 
        stop("No matching samples or all specified samples have missing ssc or q")
    df <- df[!is.na(df$ssc), ]
    conf <- intersect(names(df), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in qsscplot:\n")
        cat(conf, "\n")
        cat("Please remove conflicting objects before proceeding.\n")
        return(invisible())
    }
    if (!(txt %in% names(df))) 
        stop(paste(txt, "is not a valid column in sed object"))
    attach(df)
    on.exit(detach(df))
    if (type == "logxy") 
        log <- "xy"
    else if (type == "logx") 
        log <- "x"
    else log <- ""
    plot(q, ssc, type = "n", xlab = "discharge (m3/sec)", ylab = "ssc", 
        log = log, ...)
    title(paste("Station ", stn, "; ", subtitle, sep = ""), cex.main = 1)
    txtitem <- eval(as.name(txt))
    if (col) {
        dumps <- unique(dump)
        for (i in seq(along = dumps)) {
            d <- dumps[i]
            text(q[dump == d], ssc[dump == d], txtitem[dump == 
                d], cex = textsize, col = i)
        }
    }
    else text(q, ssc, txtitem, cex = textsize)
    if (type == "linear") {
        fit <- lm(ssc ~ q, na.action = na.omit)
        abline(fit)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit"
    }
    else if (type == "logx") {
        fit <- lm(ssc ~ log(q), na.action = na.omit)
        x0 <- range(q)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit to log(x)"
        y0 <- predict(fit, newdata = data.frame(q = x0))
        lines(x0, y0)
    }
    else if (type == "logxy") {
        if (any(q <= 0 | ssc <= 0)) {
            keep <- q > 0 & ssc > 0
            q <- q[keep]
            ssc <- ssc[keep]
        }
        fit <- logline(q, ssc)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "log-log fit"
    }
    else if (type == "sqrt") {
        fit <- lm(sqrt(ssc) ~ sqrt(q), subset = (q >= 0 & ssc >= 
            0))
        x0 <- seq(min(q), max(q), len = 50)
        y0 <- predict(fit, newdata = data.frame(q = x0))
        lines(x0, y0^2)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "sqrt-sqrt fit"
    }
    else if (type == "power") {
        startdata <- df[q != 0 & ssc != 0, ]
        startmodel <- lm(log(ssc) ~ log(q), data = startdata)
        a0 <- exp(coef(startmodel)[1])
        b0 <- coef(startmodel)[2]
        fit <- nls(ssc ~ a * q^b, start = list(a = a0, b = b0))
        x0 <- seq(min(q), max(q), len = 25)
        fit$s <- summary(fit)$sigma
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- "power fit"
        y0 <- predict(fit, newdata = data.frame(q = x0))
        lines(x0, y0)
    }
    else if (type == "loess") {
        if (span == 0) 
            span <- optimize.loess(q, ssc, 0.5, 1, by = 0.05, 
                degree)
        lines(loess.smooth(q, ssc, span = span, degree = degree, 
            family = "gaussian"))
        fit <- loess(ssc ~ q, degree = degree, span = span, family = "gaussian")
        fit$aicc <- aic.loess(fit)
        fit$s <- summary(fit)$s
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- paste("loess(span=", round(span, 2), ",degree=", 
            degree, ")", sep = "")
    }
    else if (type == "pairs") {
        lines(q, ssc)
        label <- "pairwise fit"
    }
    mtext(label, side = 3, line = 0.5, cex = 0.8)
    if (type != "pairs") 
        if (!is.null(fit$aicc)) 
            list(model = fit, r2 = fit$r2, s = fit$s, AICc = fit$aicc)
        else list(model = fit, r2 = fit$r2, s = fit$s)
}
