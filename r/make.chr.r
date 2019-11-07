make.chr <-
function (year, mo, dy, time) 
{
    chron(paste(mo, dy, year, sep = "/")) + mt2msm(time)/1440
}
