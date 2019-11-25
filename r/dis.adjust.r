dis.adjust <-
function (stn, ssc) 
{
    S <- toupper(stn)
    if (!exists("discoef"))
        loadDiscoef()
        #stop("Need 'discoef' data frame to make DI sample adjustments")
    if (S %in% row.names(discoef)) {
        a <- discoef[S, "a"]
        b <- discoef[S, "b"]
        dismax <- discoef[S, "max"]
        tmp1 <- ssc[!is.na(ssc)]
        tmp2 <- exp(a) * tmp1^b
        if (!is.na(dismax) & any(tmp1 > dismax)) {
            maxfactor <- exp(a) * dismax^(b - 1)
            tmp2[tmp1 > dismax] <- maxfactor * tmp1[tmp1 > dismax]
            cat("Note: concentrations above ", dismax, "mg/L found. ")
            cat("Applying limiting DI adjustment factor of ", 
                maxfactor, "\n")
        }
        ssc[!is.na(ssc)] <- tmp2
        if (a == 0 && b == 1) 
            attr(ssc, "adjusted") <- TRUE
        else attr(ssc, "adjusted") <- FALSE
    }
    else stop(paste("Warning: station ", S, " not found in 'discoef' data frame."))
    ssc
}
