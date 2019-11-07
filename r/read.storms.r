read.storms <-
function (stn, hy, alt = T) 
{
    hy <- zfill(hy, 2)
    if (alt) 
        filename <- paste(stn, hy, "storms.alt", sep = "")
    else filename <- paste(stn, hy, "storms.csv", sep = "")
    loc <- paste("sedloadestwork\\hy", hy, sep = "")
    file <- paste(loc, filename, sep = "\\")
    options(show.error.messages = F)
    dat <- try(read.table(file, sep = ","))
    options(show.error.messages = T)
    if (class(dat) == "try-error") {
        if (alt) 
            return(read.storms(stn, hy, alt = F))
        else stop(paste("Cannot find or read", file))
    }
    cat(paste("Storm dates read from file ", filename, "\n", 
        sep = "'"))
    names(dat) <- c("number", "sdate", "stime", "edate", "etime")
    schron <- ymd2date(dat$sdate) + mt2msm(dat$stime)/1440
    echron <- ymd2date(dat$edate) + mt2msm(dat$etime)/1440
    data.frame(number = dat$number, schron, echron)
}
