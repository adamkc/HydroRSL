read.lab.vr <-
function (file, path = ".") 
{
    full.name <- paste(path, file, sep = "\\")
    print(full.name)
    data <- read.table(full.name, sep = ",", skip = 1, fill = T, 
        as.is = 1:2)
    data <- data[, 1:5]
    names(data) <- c("date", "time", "dump", "bottle", "ssc")
    data <- data[data$time != "NA", ]
    data <- data[!is.na(data$ssc), ]
    data$chr <- chron(data$date, paste(data$time, "00", sep = ":"))
    find.factors(data[, c("dump", "bottle", "ssc")])
    data[, c("chr", "dump", "bottle", "ssc")]
}
