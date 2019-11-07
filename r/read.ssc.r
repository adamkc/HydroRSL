read.ssc <-
function (stn, hy, path = paste(getTTSenv("TTSHOME"), "tenminssc", 
    sep = "/")) 
{
    fname <- paste(stn, zfill(hy, 2), "ssc.txt", sep = "")
    pathname <- paste(path, fname, sep = "/")
    data <- read.table(pathname, header = T, as.is = T)
    data$chr <- chron(data$DATE, paste(data$TIME, "00", sep = ":"))
    data
}
