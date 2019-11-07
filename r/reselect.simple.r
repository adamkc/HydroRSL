reselect.simple <-
function (data, xvar, yvar, type = "Omit", dataname) 
{
    if (!(xvar %in% names(data))) 
        stop(paste(xvar, "is not a column in", dataname))
    if (!(yvar %in% names(data))) 
        stop(paste(yvar, "is not a column in", dataname))
    if (!("exclude" %in% names(data))) 
        data$exclude <- logical(dim(data)[1])
    repeat {
        plot(data[, xvar], data[, yvar], pch = 1, xlab = xvar, 
            ylab = yvar, main = dataname)
        points(data[data$exclude, xvar], data[data$exclude, yvar], 
            pch = 13, col = 2)
        k <- identify(data[, xvar], data[, yvar], n = 1)
        if (length(k) != 0) 
            data$exclude[k] <- ifelse(type == "Omit", TRUE, FALSE)
        else break
    }
    if (sum(data$exclude) > 0) {
        cat("The following items in", dataname, "are now tagged for exclusion:\n")
        print(data[data$exclude, 1:5])
    }
    else cat("No items in", dataname, "are now tagged for exclusion\n")
    data
}
