qcalc <-
function (stn, stg, hy) 
{
    if (stn == "nfc" || stn == "sfc") 
        weircalc(stg)
    else if (stn == "arf") 
        arfcalc(stg)
    else if (stn == "que") 
        quecalc(stg)
    else if (stn == "ftr") 
        ftrcalc(stg, hy)
    else if (stn == "ujc") 
        ujccalc(stg)
    else if (stn == "alb") 
        albcalc(stg)
    else if (!missing(hy)) 
        flumecalc(stn, stg, hy)
    else if (toupper(stn) %in% c("IVE", "CAR", "EAG", "HEN")) 
        stop(paste("qcalc needs water year for station", stn))
    else flumecalc(stn, stg)
}
