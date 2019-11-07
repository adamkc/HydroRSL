reselect <-
function (data, xvar, yvar, dataname, subset) 
{
    if (!(xvar %in% names(data))) 
        stop(paste(xvar, "is not a column in", dataname))
    if (!(yvar %in% names(data))) 
        stop(paste(yvar, "is not a column in", dataname))
    cat("Click points for inclusion/exclusion, use middle mouse button to quit\n")
    if (!("exclude" %in% names(data))) 
        data$exclude <- logical(dim(data)[1])
    if (missing(subset)) 
        subdata <- data
    else subdata <- data[subset, ]
    repeat {
        plot(subdata[, xvar], subdata[, yvar], pch = 1, xlab = xvar, 
            ylab = yvar, main = dataname)
        points(subdata[subdata$exclude, xvar], subdata[subdata$exclude, 
            yvar], pch = 13, col = 2)
        k <- identify(subdata[, xvar], subdata[, yvar], row.names(subdata), 
            n = 1)
        if (length(k) != 0) 
            subdata$exclude[k] <- !subdata$exclude[k]
        else break
    }
    data$exclude[subset] <- subdata$exclude
    if (sum(data$exclude) > 0) {
        cat("The following items in", dataname, "are now tagged for exclusion:\n")
        print(data[data$exclude, 1:5])
    }
    else cat("No items in", dataname, "are now tagged for exclusion\n")
    data
}
