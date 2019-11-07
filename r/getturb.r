getturb <-
function (hy, stations, match = T, opt = 0) 
{
    allturbs <- NULL
    alltimes <- NULL
    for (STN in stations) {
        stn <- tolower(STN)
        data <- read.flo(stn, hy)
        sd <- read.allstorms(stn, hy, match)
        nstorms <- dim(sd)[[1]]
        turb <- numeric(nstorms)
        time <- numeric(nstorms)
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            stormturb <- data$turb[storm]
            turbcodes <- data$turbcode[storm]
            turbcodes <- rev(turbcodes[order(stormturb)])
            sortturb <- rev(stormturb[order(stormturb)])
            if (opt > 0) {
                turb[i] <- sortturb[6 * opt]
                nflags <- sum(sortturb[1:(6 * opt)] %in% c(1, 
                  6, 7))
                if (nflags/6 * opt > 0.5) 
                  turb[i] = -turb[i]
            }
            else {
                turb[i] <- sortturb[1]
                maxima <- (data$turb[storm] == sortturb[1])
                time[i] <- min(data$chr[storm][maxima])
                if (turbcodes[1] %in% c(1, 6, 7)) 
                  turb[i] <- -turb[i]
            }
        }
        cat(STN, " ", turb, "\n")
        allturbs <- c(allturbs, turb)
        if (opt == 0) 
            alltimes <- c(alltimes, time)
    }
    if (length(allturbs) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    turb.mtx <- matrix(allturbs, nr = nstorms, dimnames = dn)
    if (opt == 0) {
        time.mtx <- matrix(alltimes, nr = nstorms, dimnames = dn)
        return(list(turb = turb.mtx, time = chron(time.mtx)))
    }
    else return(turb.mtx)
}
