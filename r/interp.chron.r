interp.chron <-
function (data, chr, var) 
{
    t1 <- round2tenmin(chr, "down")
    t2 <- round2tenmin(chr, "up")
    t <- sort(unique(c(t1, t2)))
    rownames <- format(chron(t))
    row.names(data) <- format(data$chr)
    matchdat <- data[rownames, ]
    approx(matchdat[, "chr"], matchdat[, var], chr, rule = 1)$y
}
