read.or <-
function (stn, hy, choose.staff = F, ttshome = getTTSenv("TTSHOME")) 
{
    orfile <- paste(stn, zfill(hy, 2), ".or", sep = "")
    orpath <- paste(ttshome, stn, orfile, sep = "/")
    staffs <- staffnames(stn)
    staff2 <- gsub(" ", "", staffs)
    n <- length(staffs)
    if (choose.staff && n > 1) {
        default.staff <- staff2[pickone(staffs[-n], "staff plate")]
    }
    else {
        default.staff <- staff2[n]
        if (choose.staff) 
            cat(staffs[n], "is the only staff plate\n")
    }
    or <- read.csv(orpath, col.names = c("year", "mo", "dy", 
        "time", "dump", staff2))
    or$chr <- make.chr(or$year, or$mo, or$dy, or$time)
    flodat <- read.stages(stn, hy)
    or$rawstg <- interp.chron(flodat, or$chr, "rawstg")
    or$corstg <- interp.chron(flodat, or$chr, "corstg")
    or$staff <- or[, default.staff]
    or[, c("chr", "dump", "staff", "rawstg", "corstg")]
}
