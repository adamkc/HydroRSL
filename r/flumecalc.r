flumecalc <-
function (stn, stg, hy = hy.default()) 
{
    a <- c(6, 10, 16, 10, 32.0542, 6, 12, 8, 16, 8, 27.16, 6, 
        6, 20, 6, 8, 12, 8, 3.95, 3.95, 8, 16, 8)
    b <- c(1.538, 1.559, 1.578, 1.559, 1.883, 1.538, 1.566, 1.55, 
        1.578, 1.55, 1.695, 1.538, 1.538, 1.587, 1.538, 1.55, 
        1.566, rep(1.55, 4), 1.578, 1.55)
    names(a) <- names(b) <- c("ban", "car", "dol", "eag", "fly", 
        "gib", "hen", "ive", "joh", "kje", "lan", "mun", "xra", 
        "xyz", "ogi", "por", "ric", "seq", "tre", "uql", "wil", 
        "yoc", "zie")
    if (hy >= 2000) {
        a["ive"] <- 3.95
        b["ive"] <- 1.55
    }
    if (hy >= 2005) {
        a["car"] <- 6
        b["car"] <- 1.538
        a["eag"] <- 6
        b["eag"] <- 1.538
        a["hen"] <- 8
        b["hen"] <- 1.55
    }
    a[stn] * stg^(b[stn])
}
