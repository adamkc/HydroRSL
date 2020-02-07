getflows <-
function (hy, stations) 
{
    allflows <- NULL
    allpoor <- NULL
    allareasknown <- F
    if (!exists("hectares")) {
        cat("'hectares' not found. All results will be in m3\n")
        area <- 1
    }
    else {
        if (all(toupper(stations) %in% names(hectares))) {
            allareasknown <- T
            cat("All results will be in m3/ha\n")
        }
    }
    for (STN in stations) {
        stn <- tolower(STN)
        STN <- toupper(STN)
        data <- read_flo(stn, hy)
        sd <- read.allstorms(stn, hy, match = F)
        nstorms <- dim(sd)[[1]]
        if (nstorms == 0) 
            stop("No storm dates found for ", STN, "\n")
        flows <- numeric(nstorms)
        pctpoor <- numeric(nstorms)
        if (exists("hectares")) {
            if (STN %in% names(hectares)) {
                area <- hectares[STN]
                if (!allareasknown) 
                  cat("Results for ", STN, " will be in m3/ha\n")
            }
            else {
                area <- 1
                cat("Watershed area not found for ", STN, "\n")
                cat("Results for ", STN, " will be in m3\n")
            }
        }
        for (i in 1:nstorms) {
            storm <- (data$chr >= sd$schron[i] & data$chr <= 
                sd$echron[i])
            flows[i] <- round((sum(data$q[storm]) * 600)/35.315/area, 
                2)
            if (any(is.na(storm))) {
                pctpoor[i] <- NA
            }
            else {
                pctpoor[i] <- sum(data$stgcode[storm] %in% c(1, 
                  6, 7))/sum(storm)
                if (pctpoor[i] >= 0.25) 
                  flows[i] <- -flows[i]
            }
        }
        cat(STN, " ", flows, "\n")
        allflows <- c(allflows, flows)
        allpoor <- c(allpoor, pctpoor)
    }
    if (length(allflows) != nstorms * length(stations)) 
        stop("Unequal numbers of storms")
    dn <- list(sd$number, stations)
    flow.mtx <- matrix(allflows, nr = nstorms, dimnames = dn)
    poor.mtx <- matrix(allpoor, nr = nstorms, dimnames = dn)
    list(flow = flow.mtx, pctpoor = round(poor.mtx, 3))
}
