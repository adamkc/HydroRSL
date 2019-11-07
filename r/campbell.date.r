campbell.date <-
function (year, day, time = 0) 
{
    daynum <- numeric(length(year))
    for (yr in unique(year)) {
        tmp <- dates(day[year == yr], origin = c(12, 31, yr - 
            1))
        daynum[year == yr] <- as.numeric(dates(tmp, origin = c(1, 
            1, 1970)))
    }
    chron <- chron(daynum) + mt2msm(time)/1440
    chron
}
