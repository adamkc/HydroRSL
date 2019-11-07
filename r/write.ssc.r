write.ssc <-
function (file, stn, hy, path = paste("H:/DataManagement/CASPARCREEK/Sediment/SuspendedSediment/Data/ProcessedData/WriteSSC", stn, sep = "/")) 
{
# Modified 14DEC2018: path changed to HYDRO file system, no longer to website_image
# Modified 14Jan2012: Now handles stn.....ssc"                                                
# Modified 08Jun2011: Only retains data within the year specified;"                           
#    Prints warnings or errors for some common issues"                                        
# Assembles and writes a file of all objects whose names match one"                           
# of the templates stn...ssc or stn....ssc ( . means any character)"                          
# Hopefully these are the 10-minute SSC files created by tts.ssc"                             
# Inserts discharge, turb, and turbcode from the flo data frame too"                          
# file = output file name"                                                                    
# stn = 3-character station name"                                                             
# hy = 2-digit water year (needed to find flo data frame)"                                    
# path = system location where output file will be written " 
    objects2 <- objects(pat = paste(stn, "...ssc", sep = ""), 
        pos = 1)
    objects3 <- objects(pat = paste(stn, "....ssc", sep = ""), 
        pos = 1)
    objects4 <- objects(pat = paste(stn, ".....ssc", sep = ""), 
        pos = 1)
    objects <- c(objects2, objects3, objects4)
    if (length(objects) == 0) 
        stop("No objects found matching templates 'stn...ssc' or 'stn....ssc'")
    null <- numeric(0)
    data <- data.frame(chr = null, ssc = null, meth = null)
    for (obj in objects) {
        newdata <- eval(as.name(obj))
        if (data.class(newdata) != "data.frame") 
            stop(paste("Object", obj, "is not a data frame"))
        if (dim(newdata)[2] != dim(data)[2]) 
            stop(paste("Object", obj, "has incorrect number of columns"))
        data <- rbind(data, eval(newdata))
    }
    data$chr <- as.chron(data$chr)
    data <- data[order(data$chr), ]
    data$ssc <- scientific(data$ssc, 3, 2)
    data$date <- format(dates(data$chr))
    data$time <- format.times(data$chr - trunc(data$chr))
    data$time <- substring(data$time, 1, 5)
    floname <- paste(stn, zfill(hy, 2), ".flo", sep = "")
    flodat <- eval(as.name(floname))
    row.names(flodat) <- format(flodat$chr)
    data <- data[data$chr >= min(flodat$chr) & data$chr <= max(flodat$chr), 
        ]
    if (length(data$chr) < length(flodat$chr)) 
        warning(paste("Objects do not cover full period represented by", 
            floname))
    if (any(duplicated(data$chr))) 
        warning("Redundant or overlapping objects found")
    data$q <- flodat[format(data$chr), "q"]
    data$q <- scientific(data$q, 3, 2)
    data$turb <- flodat[format(data$chr), "turb"]
    data$turbcode <- as.character(flodat[format(data$chr), "turbcode"])
    data$turbcode <- padstring(data$turbcode, 3, right = F)
    output <- data[, c("date", "time", "q", "turb", "turbcode", 
        "ssc", "meth")]
    col.names <- c("DATE", "TIME", "FLOW", "TURB", "TURBQUAL", 
        "SSC", "METHOD")
    names(output) <- padstring(col.names, c(8, 5, 9, 4, 9, 3, 
        6), right = T)
    outfile <- paste(path, file, sep = "/")
    options(chron.year.abb = FALSE)
    on.exit(options(chron.year.abb = TRUE))
    if (!file.exists(path)) {
        dir.create(path)
        if (!file.exists(path)) 
            dir.create(path, recursive = T)
        if (!file.exists(path)) 
            stop(paste("Unable to create", path))
        else cat("Created folder", path, "\n")
    }
    else if (file.exists(outfile)) 
        cat("Rewriting file", outfile, "\n")
    else cat("Writing estimated SSC data to", outfile, "\n")
    write.table(output, outfile, col.names = T, row.names = F, 
        quote = FALSE)
    objects
}
