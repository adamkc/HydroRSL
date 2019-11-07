getStormdates <-
function (stn, hy) 
{
    objects2 <- objects(pat = paste(stn, "...ssc", sep = ""), 
        pos = 1)
    objects3 <- objects(pat = paste(stn, "....ssc", sep = ""), 
        pos = 1)
    objects4 <- objects(pat = paste(stn, ".....ssc", sep = ""), 
        pos = 1)
    objects <- c(objects2, objects3, objects4)
    if (length(objects) == 0) 
        stop("No objects found matching templates 'stn...ssc' or 'stn....ssc'")
    number <- schron <- echron <- numeric(0)
    for (newobj in objects) {
        newdata <- eval(as.name(newobj))
        if (data.class(newdata) != "data.frame") 
            cat(paste("Object", newobj, "is not a data frame\n"))
        newnum <- as.numeric(substring(newobj, 4, nchar(newobj) - 
            4))
        number <- c(number, newnum)
        schron <- c(schron, newdata$chr[1])
        echron <- c(echron, last.val(newdata$chr))
    }
    data <- data.frame(number = number, schron = as.chron(schron), 
        echron = as.chron(echron))
    data <- data[data$number%/%100 == as.numeric(hy), ]
    data <- data[!(data$number%%100 %in% c(0, 99)), ]
    data[order(data$schron), ]
}
