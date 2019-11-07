crbasic.date <-
function (date) 
{
    date.list <- sapply(as.character(date), strsplit, " ")
    date <- sapply(date.list, function(x) x[[1]])
    time <- sapply(date.list, function(x) x[[2]])
    chron(date, time, format = c("y-m-d", "h:m:s"), out.format = c("m/d/y", 
        "h:m:s"))
}
