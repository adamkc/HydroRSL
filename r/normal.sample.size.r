normal.sample.size <-
function (mean2, mean.alt, mean = 0, sd1 = 1, sd2 = sd1, power = 0.8, 
    alpha = 0.05, n1 = NULL, n2 = NULL, prop.n2 = 1, one.sample = missing(sd2), 
    alternative = "two.sided", expand.args = T, exact.n = F, 
    recompute.power = F) 
{
    compute.sample.size <- function(Zalpha, Zpower, sd1, sd2, 
        prop.n2, delta, one.sample, exact.n, ...) {
        n <- ((sd1 * (Zalpha + Zpower))/delta)^2
        if (!one.sample) {
            n1 <- n * (1 + sd2^2/(prop.n2 * sd1^2))
            if (!exact.n) {
                n1 <- ceiling(n1)
                n2 <- ceiling(prop.n2 * n1)
            }
            else {
                n2 <- prop.n2 * n1
            }
            n <- list(n1, n2)
        }
        else if (!exact.n) {
            n <- ceiling(n)
        }
        return(n)
    }
    compute.power <- function(Zalpha, sd1, sd2, n1, n2, prop.n2, 
        delta, one.sample, alternative, ...) {
        if (one.sample) {
            sigma.inv <- sqrt(n1)/sd1
        }
        else {
            sigma.inv <- sqrt(n2/(prop.n2 * sd1^2 + sd2^2))
        }
        power <- switch(alternative, greater = ifelse(delta == 
            0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - Zalpha)), 
            less = ifelse(delta == 0, 1 - pnorm(Zalpha), pnorm(-delta * 
                sigma.inv - Zalpha)), two.sided = ifelse(delta == 
                0, 1 - pnorm(Zalpha), pnorm(delta * sigma.inv - 
                Zalpha)) + ifelse(delta == 0, 1 - pnorm(Zalpha), 
                pnorm(-delta * sigma.inv - Zalpha)))
        return(power)
    }
    compute.delta <- function(Zalpha, Zpower, sd1, sd2, n1, n2, 
        prop.n2, one.sample, alternative, ...) {
        if (one.sample) {
            sigma <- sd1/sqrt(n1)
        }
        else {
            sigma <- sqrt((prop.n2 * sd1^2 + sd2^2)/n2)
        }
        delta <- (Zalpha + Zpower) * sigma
        if (alternative == "less") {
            delta <- (-delta)
        }
        return(delta)
    }
    if (!missing(alternative)) {
        alt.expanded <- char.expand(alternative, c("two.sided", 
            "greater", "less"), stop("argument 'alternative' must match one of 'greater', 'less', 'two.sided'."))
    }
    else {
        alt.expanded <- alternative
    }
    if (is.null(n1) && is.null(n2)) {
        compute.what <- "sample.size"
        compute.function <- "compute.sample.size"
        n1 <- n2 <- as.numeric(NA)
    }
    else if ((missing(mean2) || is.null(mean2)) && (missing(mean.alt) || 
        is.null(mean.alt))) {
        compute.what <- "delta"
        compute.function <- "compute.delta"
        delta <- as.numeric(NA)
        mean2 <- as.numeric(NA)
    }
    else {
        compute.what <- "power"
        compute.function <- "compute.power"
        power <- as.numeric(NA)
    }
    if (compute.what != "delta") {
        if (!(missing(mean2) || is.null(mean2))) {
            if (missing(one.sample)) {
                one.sample <- F
            }
        }
        else if (!(missing(mean.alt) || is.null(mean.alt))) {
            mean2 <- mean.alt
            if (missing(one.sample)) {
                one.sample <- T
            }
        }
        else {
            stop(paste("A second (alternative) mean is required to compute", 
                compute.what))
        }
    }
    else if (missing(one.sample)) {
        one.sample <- missing(n2) && missing(prop.n2)
    }
    if (one.sample) {
        arg.names <- c("mean1", "sd1", "mean2", "delta", "alpha", 
            "power", "n1")
        table.names <- c("mean.null", "sd1", "mean.alt", "delta", 
            "alpha", "power", "n1")
        n2 <- NULL
        prop.n2 <- NULL
        sd2 <- NULL
    }
    else {
        table.names <- c("mean1", "sd1", "mean2", "sd2", "delta", 
            "alpha", "power", "n1", "n2", "prop.n2")
        arg.names <- table.names
    }
    power.table <- build.power.table(theta1 = mean, disp1 = sd1, 
        theta2 = mean2, disp2 = sd2, alpha = alpha, power = power, 
        n1 = n1, n2 = n2, prop.n2 = prop.n2, expand.args = expand.args, 
        one.sample = one.sample, compute.what = compute.what)
    names(power.table) <- arg.names
    if (alt.expanded == "two.sided") {
        Zalpha <- qnorm(1 - power.table$alpha/2)
    }
    else {
        Zalpha <- qnorm(1 - power.table$alpha)
    }
    if (!all(is.na(power.table$power))) {
        Zpower <- qnorm(power.table$power)
    }
    else {
        Zpower <- as.numeric(NA)
    }
    arglist <- c(power.table, list(Zalpha = Zalpha, Zpower = Zpower, 
        one.sample = one.sample, exact.n = exact.n, alternative = alt.expanded))
    if (compute.what == "sample.size") {
        if (one.sample) {
            compute.what <- "n1"
        }
        else {
            compute.what <- c("n1", "n2")
        }
    }
    power.table[, compute.what] <- do.call(compute.function, 
        arglist)
    if (recompute.power && !exact.n && compute.function == "compute.sample.size") {
        if (one.sample) {
            arglist[[compute.what]] <- power.table[, compute.what]
        }
        else {
            arglist[compute.what] <- power.table[, compute.what]
        }
        power.table[, "power"] <- do.call("compute.power", arglist)
    }
    if (compute.function == "compute.delta") {
        power.table$mean2 <- switch(alt.expanded, two.sided = NA, 
            greater = power.table$mean1 + abs(power.table$delta), 
            less = power.table$mean1 - abs(power.table$delta))
    }
    names(power.table) <- table.names
    return(power.table)
}
