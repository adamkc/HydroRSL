read.campbell <-
function (file, path = getTTSenv("LOGGERHOME")) 
{
    data <- read.table(paste(path, file, sep = "\\"), sep = ",")[, 
        2:10]
    names(data) <- c("year", "day", "time", "dump", "bottle", 
        "code1", "code2", "stg", "turb")
    data$chr <- campbell.date(data$year, data$day, data$time)
    nc <- dim(data)[[2]]
    data[, c(nc, 5:(nc - 1))]
}
