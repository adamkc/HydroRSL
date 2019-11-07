turbsscplot <-
function (stn, hy, sdate, stime = 0, edate, etime = 2400, dumpstr, 
    bottlestr, exclude = TRUE, type = "linear", col = T, textsize = 0.6, 
    span = 1, degree = 1, txt = "bottle", ...) 
{
    nhy <- as.numeric(hy)
    prevyr <- ifelse(nhy == 0, "99", zfill(nhy - 1, 2))
    df <- eval(as.name(paste(stn, hy, ".sed", sep = "")))
    station.name <- attr(df, "stn")
    stn <- toupper(station.name)
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
    if (any(is.na(df$ssc) | is.na(df$turb))) {
        if (all(is.na(df$ssc) | is.na(df$turb))) 
            stop("No samples match specified criteria")
        df <- df[!is.na(df$ssc) & !is.na(df$turb), ]
        print("missing value(s) removed from sample data")
    }
    conf <- intersect(names(df), objects(1))
    if (length(conf) > 0) {
        cat("The following objects conflict with object names in turbsscplot:\n")
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
    plot(turb, ssc, type = "n", xlab = "turb", ylab = "ssc", 
        log = log, ...)
    title(paste("Station ", stn, "; ", subtitle, sep = ""), cex.main = 1)
    txtitem <- eval(as.name(txt))
    if (col) {
        udumps <- unique(df$dump)
        for (i in seq(along = udumps)) {
            d <- udumps[i]
            text(turb[dump == d], ssc[dump == d], txtitem[dump == 
                d], cex = textsize, col = i)
        }
    }
    else text(turb, ssc, txtitem, cex = textsize)
    if (type == "linear") {
        fit <- lm(ssc ~ turb, na.action = na.omit)
        abline(fit)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit"
    }
    else if (type == "logx") {
        fit <- lm(ssc ~ log(turb), na.action = na.omit)
        x0 <- range(turb)
        fit$aicc <- aic.lm(fit)["aic.c"]
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "linear fit to log(x)"
        y0 <- predict(fit, newdata = data.frame(turb = x0))
        lines(x0, y0)
    }
    else if (type == "logxy") {
        if (any(turb <= 0 | ssc <= 0)) {
            keep <- turb > 0 & ssc > 0
            turb <- turb[keep]
            ssc <- ssc[keep]
        }
        fit <- logline(turb, ssc)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "log-log fit"
    }
    else if (type == "sqrt") {
        fit <- lm(sqrt(ssc) ~ sqrt(turb), subset = (turb >= 0 & 
            ssc >= 0))
        x0 <- seq(min(turb), max(turb), len = 50)
        y0 <- predict(fit, newdata = data.frame(turb = x0))
        lines(x0, y0^2)
        fit$s <- summary(fit)$sigma
        fit$r2 <- summary(fit)$r.sq
        label <- "sqrt-sqrt fit"
    }
    else if (type == "power") {
        startdata <- df[turb != 0 & ssc != 0, ]
        startmodel <- lm(log(ssc) ~ log(turb), data = startdata)
        a0 <- exp(coef(startmodel)[1])
        b0 <- coef(startmodel)[2]
        fit <- nls(ssc ~ a * turb^b, start = list(a = a0, b = b0))
        x0 <- seq(min(turb), max(turb), len = 25)
        fit$s <- summary(fit)$sigma
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- "power fit"
        y0 <- predict(fit, newdata = data.frame(turb = x0))
        lines(x0, y0)
    }
    else if (type == "loess") {
        if (span == 0) 
            span <- optimize.loess(turb, ssc, 0.5, 1, by = 0.05, 
                degree)
        lines(loess.smooth(turb, ssc, span = span, degree = degree, 
            family = "gaussian"))
        fit <- loess(ssc ~ turb, degree = degree, span = span, 
            family = "gaussian")
        fit$aicc <- aic.loess(fit)
        fit$s <- summary(fit)$s
        fit$r2 <- cor(fitted(fit), ssc)^2
        label <- paste("loess(span=", round(span, 2), ",degree=", 
            degree, ")", sep = "")
    }
    else if (type == "pairs") {
        lines(turb, ssc)
        label <- "pairwise fit"
    }
    mtext(label, side = 3, line = 0.5, cex = textsize)
    if (type != "pairs") 
        if (!is.null(fit$aicc)) 
            list(model = fit, r2 = fit$r2, s = fit$s, AICc = fit$aicc)
        else list(model = fit, r2 = fit$r2, s = fit$s)
}
