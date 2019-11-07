ezchron2 <-
function (date.time) 
{
    require(chron)
    timelist <- strsplit(date.time, " ")
    date <- sapply(timelist, "[", 1)
    time <- sapply(timelist, "[", 2)
    time <- paste(time, "00", sep = ":")
    chron(date, time)
}
