mismatches <-
function (stn, hy) 
{
    tmp1 <- merge_flo(stn, hy, all.lab = T, na.rm = F)
    tmp2 <- merge_flo(stn, hy, all.flo = T, na.rm = F)
    tmp3 <- merge_flo(stn, hy, na.rm = F)
    row.names(tmp1) <- paste(tmp1$dump, tmp1$bottle, sep = ":")
    row.names(tmp2) <- paste(tmp2$dump, tmp2$bottle, sep = ":")
    row.names(tmp3) <- paste(tmp3$dump, tmp3$bottle, sep = ":")
    labmatches <- row.names(tmp1) %in% row.names(tmp3)
    flomatches <- row.names(tmp2) %in% row.names(tmp3)
    lab <- tmp1[!labmatches, ]
    if (dim(lab)[1] == 0) 
        lab <- NULL
    flo <- tmp2[!flomatches, ]
    if (dim(flo)[1] == 0) 
        flo <- NULL
    list(unmatched.lab = lab, unmatched.flo = flo)
}
