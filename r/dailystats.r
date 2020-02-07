dailystats <-
function (stn, hy) 
{
    data <- read_flo(stn, hy)
    day <- dates(data$chr)
    meandaily <- tapply(data$q, day, mean, na.rm = T)
    mindaily <- tapply(data$q, day, min, na.rm = T)
    maxdaily <- tapply(data$q, day, max, na.rm = T)
    date <- dates(as.numeric(names(meandaily)))
    data.frame(date = date, mean = as.vector(meandaily), min = as.vector(mindaily), 
        max = as.vector(maxdaily))
}
