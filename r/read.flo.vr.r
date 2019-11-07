read.flo.vr <-
function (file, path = ".") 
{
    full.name <- paste(path, file, sep = "\\")
    print(full.name)
    data <- read.table(full.name, sep = ",", skip = 1, fill = T, 
        as.is = 1:2)
    data <- data[, 1:4]
    names(data) <- c("date", "time", "stg", "q")
    data$chr <- chron(data$date, paste(data$time, "00", sep = ":"))
    find.factors(data[, c("stg", "q")])
    data[, c("chr", "stg", "q")]
}
