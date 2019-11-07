tts5.sample <-
function (data, up, dn, stay = 2, revpct = c(10, 20), repwait = 18, 
    minstg = 0, turblim = 2000, limskip = 2, initcode = 3, revval = 5, 
    startwait = 72) 
{
    sam <- numeric(0)
    thresh <- numeric(0)
    thrcode <- numeric(0)
    thrcount <- revcount <- 0
    lastris <- rep(-999, length(up))
    lastfal <- rep(-999, length(dn))
    lastsam <- -999
    tmax <- tmin <- data$turb[1]
    tmin <- tmax + 1
    tcode <- initcode
    j <- 1
    if (tcode == 1) 
        j <- 1 + sum(data$turb[1] >= up)
    else if (tcode == 2) 
        j <- sum(data$turb[1] > dn)
    i <- 0
    imax <- length(data$turb)
    while (i < imax) {
        while (data$stg[i + 1] < minstg) {
            tcode <- 0
            i <- i + 1
            if (i == imax) 
                break
        }
        if (tcode == 0 && i < imax) {
            if ((data$turb[i + 1] >= up[1]) && (i - startwait > 
                lastsam)) {
                i <- i + 1
                lastsam <- i
                sam <- c(sam, i)
                thrcode <- c(thrcode, 4)
                thresh <- c(thresh, NA)
            }
            j <- 1 + sum(data$turb[i] >= up)
            tcode <- 1
            tmax <- data$turb[i]
        }
        else if (tcode == 3) {
            if (i == 0) 
                i <- i + 1
            else if (i == 1) {
                if (data$turb[1] < data$turb[2]) {
                  tcode <- 1
                  j <- 1 + sum(data$turb[2] >= up)
                }
                else {
                  tcode <- 2
                  j <- sum(data$turb[2] > dn)
                }
            }
        }
        while (tcode == 1 && i < imax) {
            if (data$stg[i + 1] < minstg) 
                break
            i <- i + 1
            t <- data$turb[i]
            if (j <= length(up) & t >= up[j]) {
                thrcount <- thrcount + 1
                if (thrcount >= stay) {
                  if (i > lastris[j] + repwait) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, up[j])
                    lastris[j] <- i
                    lastsam <- i
                  }
                  j <- 1 + sum(t >= up)
                  thrcount <- 0
                }
            }
            else if (t >= turblim) {
                if (i > lastsam + limskip) {
                  sam <- c(sam, i)
                  thrcode <- c(thrcode, 5)
                  thresh <- c(thresh, NA)
                  lastsam <- i
                  revcount <- 0
                }
            }
            if (t >= tmax) {
                tmax <- t
                revcount <- 0
            }
            else if (t < data$turb[i - 1] && t < turblim) {
                revcount <- revcount + 1
                revthresh <- tmax - max(revval, 0.01 * revpct[1] * 
                  tmax)
                if (t <= revthresh && revcount >= stay) {
                  tcode <- 2
                  j <- sum(t > dn)
                  tmin <- t
                  if ((j < length(dn)) && (tmax > dn[j + 1]) && 
                    (i > lastfal[j + 1] + repwait)) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, dn[j + 1])
                    lastfal[j + 1] <- i
                    lastsam <- i
                  }
                }
            }
        }
        thrcount <- 0
        revcount <- 0
        while (tcode == 2 && (i < imax)) {
            if (data$stg[i + 1] < minstg) 
                break
            i <- i + 1
            t <- data$turb[i]
            if (j > 0 && t <= dn[j]) {
                thrcount <- thrcount + 1
                if (thrcount >= stay) {
                  if (i > lastfal[j] + repwait) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, dn[j])
                    lastfal[j] <- i
                    lastsam <- i
                  }
                  j <- sum(t > dn)
                  thrcount <- 0
                }
            }
            if (t <= tmin) {
                tmin <- t
                revcount <- 0
            }
            else if (t > data$turb[i - 1] && t < turblim) {
                revcount <- revcount + 1
                revthresh <- tmin + max(revval, 0.01 * revpct[2] * 
                  tmin)
                if (t >= revthresh && revcount >= stay) {
                  tcode <- 1
                  j <- 1 + sum(t >= up)
                  tmax <- t
                  if ((j > 1) && (tmin < up[j - 1]) && (i > lastris[j - 
                    1] + repwait)) {
                    sam <- c(sam, i)
                    thrcode <- c(thrcode, tcode)
                    thresh <- c(thresh, up[j - 1])
                    lastris[j - 1] <- i
                    lastsam <- i
                  }
                }
            }
            else if (t >= turblim) {
                tcode <- 1
                tmax <- t
                j <- 1 + sum(t >= up)
                thrcount <- 0
                if (i > lastsam + limskip) {
                  sam <- c(sam, i)
                  thrcode <- c(thrcode, 5)
                  thresh <- c(thresh, NA)
                  lastsam <- i
                  revcount <- 0
                }
            }
        }
        thrcount <- 0
        revcount <- 0
    }
    result <- data.frame(sam, thrcode, thresh)
    attr(result, "minstg") <- minstg
    result
}
