merge.flo.vr <-
function (stn, hy, all.lab = F) 
{
    lab <- eval(as.name(paste(stn, zfill(hy, 2), ".lab", sep = "")))
    flo <- eval(as.name(paste(stn, zfill(hy, 2), ".flo", sep = "")))
    lab$q <- approx(flo$chr, flo$q, lab$chr)$y
    lab
}
