read.allstorms <-
function(stn, hy, match = T, loc = paste(Sys.getenv("WATER"),"\\Sediment\\SuspendedSediment\\MethodsTesting_Analyses\\SuspendedSedLoads\\LoadEstimation\\hy", hy, sep = "")) 
{
    hy <- zfill(hy, 2)
    if (match) 
        filename <- paste("storms", hy, ".alt", sep = "")
    else filename <- paste("storms", hy, ".csv", sep = "")
    file <- paste(loc, filename, sep = "/")
    options(show.error.messages = F, warn = -1)
    dat <- try(read.table(file, sep = ",", header = T))
    options(show.error.messages = T, warn = 0)
    if (class(dat) == "try-error") 
        if (match) {
            cat("No alt file found: reading csv file\n")
            return(read.allstorms(stn, hy, match = F))
        }
        else stop(paste("Cannot find or read", file))
    cat(paste("Storm dates read from file ", filename, "\n", 
        sep = "'"))
    names(dat) <- c("stn", "number", "sdate", "stime", "edate", 
        "etime")
    schron <- chron(ymd2date(dat$sdate)) + mt2msm(dat$stime)/1440
    echron <- chron(ymd2date(dat$edate)) + mt2msm(dat$etime)/1440
    newdat <- data.frame(stn = dat$stn, number = dat$number, 
        schron, echron)
    newdat$stn <- as.character(newdat$stn)
    newdat[toupper(newdat$stn) == toupper(stn), c("number", "schron", 
        "echron")]
}
